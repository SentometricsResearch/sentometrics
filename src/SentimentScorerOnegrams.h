
#ifndef SENTIMENT_ONEGRAMS
#define SENTIMENT_ONEGRAMS

struct SentimentScorerOnegrams : public RcppParallel::Worker {

  // thread-safe input
  const std::vector< std::vector<std::string> > texts;
  const std::unordered_map< std::string, std::vector<double> > lexiconMap;
  const std::string how;
  const int nL;

  // output
  RcppParallel::RMatrix<double> sentScores;

  SentimentScorerOnegrams(const std::vector< std::vector<std::string> > texts,
                          const std::unordered_map< std::string, std::vector<double> > lexiconMap,
                          const std::string how,
                          int nL,
                          Rcpp::NumericMatrix sentScores)
    : texts(texts), lexiconMap(lexiconMap), how(how), nL(nL), sentScores(sentScores) {}

  void operator()(std::size_t begin, std::size_t end) {

    for (std::size_t i = begin; i < end; i++) {

      std::vector<std::string> tokens = texts[i];
      std::vector<double> scores(nL, 0.0);
      std::vector<double> nPolarized(nL, 0.0);
      int nTokens = tokens.size();

      for (int j = 0; j < nTokens; j++) {
        std::string token = tokens[j];
        if (lexiconMap.find(token) != lexiconMap.end()) {
          std::vector<double> lexScores = lexiconMap.at(token);
          update_scores(scores, lexScores, nPolarized, 1.0);
        }
      }

      if (how == "proportional") rescale_scores_proportional(scores, nTokens);
      else if (how == "proportionalPol") rescale_scores_proportionalPol(scores, nPolarized);

      sentScores(i, 0) = nTokens;
      for (int m = 0; m < nL; m++) {
        sentScores(i, m + 1) = scores[m];
      }

    }
  }

};

#endif

