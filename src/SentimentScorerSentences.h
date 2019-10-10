
#ifndef SENTIMENT_SENTENCES
#define SENTIMENT_SENTENCES

struct SentimentScorerSentences : public RcppParallel::Worker {

  // thread-safe input
  const std::vector< std::vector< std::string > > texts;
  const std::unordered_map< std::string, std::vector< double > > lexiconMap;
  const std::unordered_map< std::string, double > valenceMap;
  const std::string how;
  const int nL;
  const int N;
  std::unordered_map< int, std::unordered_map< std::string, double> > frequencyMap;
  std::unordered_map< std::string, double > inverseFrequencyMap;
  const bool isFreqWeighting;
  const int valenceType;

  // output
  RcppParallel::RMatrix<double> sentScores;

  SentimentScorerSentences(const std::vector< std::vector< std::string > > texts,
                           const std::unordered_map< std::string, std::vector< double > > lexiconMap,
                           const std::unordered_map< std::string, double > valenceMap,
                           const std::string how,
                           int nL,
                           int N,
                           std::unordered_map< int, std::unordered_map< std::string, double > > frequencyMap,
                           std::unordered_map< std::string, double > inverseFrequencyMap,
                           const bool isFreqWeighting,
                           const int valenceType,
                           Rcpp::NumericMatrix sentScores)
    : texts(texts), lexiconMap(lexiconMap), valenceMap(valenceMap), how(how), nL(nL), N(N), frequencyMap(frequencyMap), inverseFrequencyMap(inverseFrequencyMap), isFreqWeighting(isFreqWeighting), valenceType(valenceType), sentScores(sentScores) {}

  void operator()(std::size_t begin, std::size_t end) {

    for (std::size_t i = begin; i < end; i++) {

      std::vector< std::string > tokens = texts[i];
      double normalizer = 0.0, maxTokenFrequency = 1.0;
      int nTokens = tokens.size(), nPuncts = 0, punctPosition = 0, lB = 0, nB = 5, nA = 2;
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
        double tokenFrequency = 1.0, tokenInverseFrequency = 1.0;
        if (isFreqWeighting) {
          update_token_frequency(tokenFrequency, freqMap, token);
          update_token_inverse_frequency(tokenInverseFrequency, inverseFrequencyMap, token, how);
        }
        if (lexiconMap.find(token) != lexiconMap.end()) { // hit
          tokenScores[j] = lexiconMap.at(token);

          if (how != "proportional" && how != "counts" && how != "proportionalSquareRoot") {
            update_token_weights(tokenWeights, normalizer, nPolarized, j, nTokens, how, nL, tokenScores, tokenFrequency, tokenInverseFrequency, maxTokenFrequency, N);
          }

          if (valenceType == 1) { // bigrams approach
            int k = std::max(0, j - 1);
            if (valenceMap.find(tokens[k]) != valenceMap.end()) { // bigram valence shifting
              tokenShifters[j] = valenceMap.at(tokens[k]);
            }
          } else if (valenceType == 2) { // clusters approach
            check_for_commas(token, nPuncts, punctPosition, j, tokens);
            int st = 0, en = 0;
            std::vector<int> shifters(4);
            set_cluster_bounds(st, en, nPuncts, punctPosition, nTokens, tokens, lB, nB, nA, j);
            for (int k = st; k < en; k++) {
              if (k == j) continue;
              std::string token_k = tokens[k];
              if (lexiconMap.find(token_k) != lexiconMap.end()) {
                tokenScores[k] = lexiconMap.at(token_k);
                if (how != "proportional" && how != "counts" && how != "proportionalSquareRoot") {
                  update_token_weights(tokenWeights, normalizer, nPolarized, k, nTokens, how, nL, tokenScores, tokenFrequency, tokenInverseFrequency, maxTokenFrequency, N);
                }
              } else if (valenceMap.find(token_k) != valenceMap.end()) {
                double valType = valenceMap.at(token_k);
                int position = 0; // initialize
                if (valType == 4) {
                  if (k > j) position = 1; // adv. conjunction after hit (position = 0 if before)
                }
                update_primary_shifters_sentence(shifters, valType, position);
              }
            }
            tokenShifters[j] = compute_sentence_impact(shifters);
          }
        }
      }
      update_token_scores(scores, tokenScores, normalizer, nPolarized, tokenShifters, tokenWeights, nL, nTokens, how, nPuncts);

      sentScores(i, 0) = nTokens - nPuncts;
      for (int m = 0; m < nL; m++) {
        sentScores(i, m + 1) = scores[m];
      }

    }
  }

};

#endif

