
#ifndef SENTIMENT_SENTENCES
#define SENTIMENT_SENTENCES

struct SentimentScorerSentences : public RcppParallel::Worker {

  // thread-safe input
  const std::vector< std::vector< std::string > > texts;
  const std::unordered_map< std::string, std::vector< double > > lexiconMap;
  const std::unordered_map< std::string, double > valenceMap;
  const std::string how;
  const int nL;
  std::unordered_map< int, std::unordered_map< std::string, double> > frequencyMap;
  std::unordered_map< std::string, double > inverseFrequencyMap;
  const bool isFreqWeighting;
  const bool hasValenceShifters;
  // output
  RcppParallel::RMatrix<double> sentScores;

  SentimentScorerSentences(const std::vector< std::vector< std::string > > texts,
                           const std::unordered_map< std::string, std::vector< double > > lexiconMap,
                           const std::unordered_map< std::string, double > valenceMap,
                           const std::string how,
                           int nL,
                           std::unordered_map< int, std::unordered_map< std::string, double > > frequencyMap,
                           std::unordered_map< std::string, double > inverseFrequencyMap,
                           const bool isFreqWeighting,
                           const bool hasValenceShifters,
                           Rcpp::NumericMatrix sentScores)
    : texts(texts), lexiconMap(lexiconMap), valenceMap(valenceMap), how(how), nL(nL), frequencyMap(frequencyMap), inverseFrequencyMap(inverseFrequencyMap), isFreqWeighting(isFreqWeighting), hasValenceShifters(hasValenceShifters), sentScores(sentScores) {}

  void operator()(std::size_t begin, std::size_t end) {

    for (std::size_t i = begin; i < end; i++) {

      std::vector< std::string > tokens = texts[i];
      double normalizer = 0.0, maxTokenFrequency = 1.0;
      int nTokens = tokens.size(), nPuncts = 0,punctPosition = 0, lB = 0, nB = 5, nA = 2;
      std::vector< double > scores(nL, 0.0);
      std::vector< double > nPolarized(nL, 0.0);
      std::vector< double > tokenWeights(nTokens, std::sqrt((double) nTokens));
      std::vector< double > tokenShifters(nTokens, 1.0);
      std::vector< std::vector< double > > tokenScores(nTokens,std::vector< double >(nL, 0.0));
      std::unordered_map< std::string, double > freqMap;

      if (isFreqWeighting) {
        update_frequency_map(freqMap, frequencyMap, i);
        update_max_token_frequency(maxTokenFrequency, freqMap, how);
      }

      for (int j = 0; j < nTokens; j++) {
        std::string token = tokens[j];
        check_for_commas(token, nPuncts, punctPosition, j, tokens);

        double tokenFrequency = 1.0, tokenInverseFrequency = 1.0;

        if (lexiconMap.find(token) != lexiconMap.end()) { // hit
          tokenScores[j] = lexiconMap.at(token);
          std::vector<int> shifters(4);

          if (how != "proportional"  && how != "counts" && how != "squareRootCounts") {
            update_token_weights(tokenWeights, normalizer, nPolarized, j, nTokens, how, nL, tokenScores, tokenFrequency, tokenInverseFrequency, maxTokenFrequency); //step 3 and 4: get token Weights
          }

          if (hasValenceShifters) {
            int st = 0, en = 0, position = -1;
            set_cluster_bounds(st, en, nPuncts, punctPosition, nTokens, tokens, lB, nB, nA, j);

            for (int k = st; k < en; k++) {
              if (k == j) continue;

              std::string token_k = tokens[k];
              if (valenceMap.find(token_k) != valenceMap.end()) {
                double valType = valenceMap.at(token_k);
                if (valType == 4) {
                  position = 0;
                  if (j < k) position = 1;
                }
                update_primary_shifters_sentence(shifters, valType, position);
              }
            }
            tokenShifters[j] = compute_sentence_impact(shifters, position);
          }
        }
      }

      update_token_scores_sentences(scores, tokenScores, normalizer, nPolarized, tokenShifters, tokenWeights, nL, nTokens, how, nPuncts);

      sentScores(i, 0) = nTokens - nPuncts;
      for (int m = 0; m < nL; m++) {
        sentScores(i, m + 1) = scores[m];
      }

    }
  }

};

#endif

