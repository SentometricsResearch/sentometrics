
#ifndef SENTIMENT_CLUSTERS
#define SENTIMENT_CLUSTERS

struct SentimentScorerClusters : public RcppParallel::Worker {

  // thread-safe input
  const std::vector< std::vector< std::string > > texts;
  const std::unordered_map< std::string, std::vector< double > > lexiconMap;
  const std::unordered_map< std::string, double > valenceMap;
  const std::string how;
  const int nL;
  std::unordered_map< int, std::unordered_map< std::string, double> > frequencyMap;
  std::unordered_map< std::string, double > inverseFrequencyMap;
  const bool isFreqWeighting;

  // output
  RcppParallel::RMatrix<double> sentScores;

  SentimentScorerClusters(const std::vector< std::vector< std::string > > texts,
                          const std::unordered_map< std::string, std::vector< double > > lexiconMap,
                          const std::unordered_map< std::string, double > valenceMap,
                          const std::string how,
                          int nL,
                          std::unordered_map<int, std::unordered_map< std::string, double> > frequencyMap,
                          std::unordered_map< std::string, double > inverseFrequencyMap,
                          const bool isFreqWeighting,
                          Rcpp::NumericMatrix sentScores)
    : texts(texts), lexiconMap(lexiconMap), valenceMap(valenceMap), how(how), nL(nL), frequencyMap(frequencyMap), inverseFrequencyMap(inverseFrequencyMap), isFreqWeighting(isFreqWeighting), sentScores(sentScores) {}

  void operator()(std::size_t begin, std::size_t end) {

    for (std::size_t i = begin; i < end; i++) {

      std::vector< std::string > tokens = texts[i];
      std::vector< double > scores(nL, 0.0);
      std::vector< double > nPolarized(nL, 0.0);
      double normalizer = 0.0;
      int nTokens = tokens.size();
      std::vector< std::vector< double > > tokenScores(nTokens,std::vector< double >(nL, 0.0));

      std::vector< double > tokenWeights(nTokens,0.0);
      std::vector< double > tokenShifters(nTokens, 1.0);
      int lB = 0, nB = 4, nA = 2; // polarity cluster: [max(lB, hit - nB); min(hit + nA, nTokens)]
      std::unordered_map< std::string, double > freqMap;
      double maxTokenFrequency = 1.0;
      if (isFreqWeighting) {
        update_frequency_map(freqMap, frequencyMap, i);
        update_max_token_frequency(maxTokenFrequency, freqMap, how);
      }

      for (int j = 0; j < nTokens; j++) {
        std::string token = tokens[j];
        double tokenFrequency = 1.0;
        double tokenInverseFrequency = 1.0;
        if (isFreqWeighting) {
          update_token_frequency(tokenFrequency, freqMap, token);
          update_token_inverse_frequency(tokenInverseFrequency, inverseFrequencyMap, token, how);
        }
          if (lexiconMap.find(token) != lexiconMap.end()) { // hit
          tokenScores[j] = lexiconMap.at(token);
          std::vector<int> shifters(3);
          int st = std::max(lB, j - nB);
          int en = std::min(nTokens, j + nA + 1);

          for (int k = st; k < en; k++) {
            if (how != "proportional"  && how != "counts") {
              update_token_weights(tokenWeights, normalizer, nPolarized, j, nTokens, how, nL, tokenScores, tokenFrequency, tokenInverseFrequency, maxTokenFrequency); //step 3 and 4: get token Weights
            }
            if (k == j) continue;
            std::string token_k = tokens[k];
            if (lexiconMap.find(token_k) != lexiconMap.end()) {
              tokenScores[k] = lexiconMap.at(token_k);
            } else if (valenceMap.find(token_k) != valenceMap.end()) {
              double valType = valenceMap.at(token_k);
              update_primary_shifters(shifters, valType);
            }
          }
           //step 3 and 4: get token Weights
          tokenShifters[j] = compute_cluster_impact(shifters);
          update_token_scores(scores, tokenScores, normalizer, nPolarized, tokenShifters, tokenWeights, nL, nTokens, how);

          lB = en + 1; // reset index such that polarity clusters are not overlapping
          j = en; // updated to j + 1 immediately after

        }
      }

      sentScores(i, 0) = nTokens;
      for (int m = 0; m < nL; m++) {
        sentScores(i, m + 1) = scores[m];
      }

    }
  }

};

#endif

