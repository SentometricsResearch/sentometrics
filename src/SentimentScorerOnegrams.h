
#ifndef SENTIMENT_ONEGRAMS
#define SENTIMENT_ONEGRAMS

struct SentimentScorerOnegrams : public RcppParallel::Worker {

  // thread-safe input
  const std::vector< std::vector< std::string > > texts;
  const std::unordered_map< std::string, std::vector< double> > lexiconMap;
  const std::string how;
  const int nL;
  std::unordered_map< int, std::unordered_map< std::string, double > > frequencyMap;
  std::unordered_map< std::string, double > inverseFrequencyMap;
  const bool isFreqWeighting;

  // output
  RcppParallel::RMatrix<double> sentScores;

  SentimentScorerOnegrams(const std::vector< std::vector< std::string > > texts,
                          const std::unordered_map< std::string, std::vector< double > > lexiconMap,
                          const std::string how,
                          int nL,
                          std::unordered_map< int, std::unordered_map< std::string, double > > frequencyMap,
                          std::unordered_map< std::string, double > inverseFrequencyMap,
                          const bool isFreqWeighting,
                          Rcpp::NumericMatrix sentScores)
    : texts(texts), lexiconMap(lexiconMap), how(how), nL(nL), frequencyMap(frequencyMap), inverseFrequencyMap(inverseFrequencyMap), isFreqWeighting(isFreqWeighting), sentScores(sentScores) {}

  void operator()(std::size_t begin, std::size_t end) {

    for (std::size_t i = begin; i < end; i++) {

      std::vector< std::string > tokens = texts[i];
      std::vector< double > scores(nL, 0.0); // scores for 1 texts for different lexicons
      std::vector< double > nPolarized(nL, 0.0);
      double normalizer = 0.0;
      int nTokens = tokens.size();
      std::vector< std::vector< double > > tokenScores(nTokens,std::vector< double >(nL, 0.0));
      std::vector< double > tokenWeights(nTokens, 0.0);
      std::vector< double > tokenShifters(nTokens, 1.0);
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

        if (lexiconMap.find(token) != lexiconMap.end()) {
          tokenScores[j] = lexiconMap.at(token); // get value of token for each lexico
        }
        if (how != "proportional"  && how != "counts" && how != "squareRootCounts") {
          update_token_weights(tokenWeights, normalizer, nPolarized, j, nTokens, how, nL, tokenScores, tokenFrequency, tokenInverseFrequency, maxTokenFrequency); //step 3 and 4: get token Weights
        }
      }
      update_token_scores(scores, tokenScores, normalizer, nPolarized, tokenShifters, tokenWeights, nL, nTokens, how);

      sentScores(i, 0) = nTokens;
      for (int m = 0; m < nL; m++) {
        sentScores(i, m + 1) = scores[m];
      }

    }
  }

};

#endif

