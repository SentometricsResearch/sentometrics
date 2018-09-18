
#ifndef SENTIMENT_CLUSTERS
#define SENTIMENT_CLUSTERS

struct SentimentScorerClusters : public RcppParallel::Worker {

  // thread-safe input
  const std::vector< std::vector<std::string> > texts;
  const std::unordered_map< std::string, std::vector<double> > lexiconMap;
  const std::unordered_map<std::string, double> valenceMap;
  const std::string how;
  const int nL;

  // output
  RcppParallel::RMatrix<double> sentScores;

  SentimentScorerClusters(const std::vector< std::vector<std::string> > texts,
                          const std::unordered_map< std::string, std::vector<double> > lexiconMap,
                          const std::unordered_map<std::string, double> valenceMap,
                          const std::string how,
                          int nL,
                          Rcpp::NumericMatrix sentScores)
    : texts(texts), lexiconMap(lexiconMap), valenceMap(valenceMap), how(how), nL(nL), sentScores(sentScores) {}

  void operator()(std::size_t begin, std::size_t end) {

    for (std::size_t i = begin; i < end; i++) {

      std::vector<std::string> tokens = texts[i];
      std::vector<double> scores(nL, 0.0);
      std::vector<double> nPolarized(nL, 0.0);
      int nTokens = tokens.size();
      int lB = 0, nB = 4, nA = 2; // polarity cluster: [max(lB, hit - nB); min(hit + nA, nTokens)]

      for (int j = 0; j < nTokens; j++) {
        std::string token = tokens[j];

        if (lexiconMap.find(token) != lexiconMap.end()) { // hit

          std::vector<double> lexScores = lexiconMap.at(token);
          std::vector<int> shifters(3);
          int st = std::max(lB, j - nB);
          int en = std::min(nTokens, j + nA + 1);

          for (int k = st; k < en; k++) {
            if (k == j) continue;
            std::string token_k = tokens[k];
            if (lexiconMap.find(token_k) != lexiconMap.end()) {
              std::vector<double> lexScores = lexiconMap.at(token_k);
              update_scores(scores, lexScores, nPolarized, 1.0); // add other polarity scores (w/o modification)
            } else if (valenceMap.find(token_k) != valenceMap.end()) {
              double valType = valenceMap.at(token_k);
              update_primary_shifters(shifters, valType);
            }
          }

          double valShifter = compute_cluster_impact(shifters);
          update_scores(scores, lexScores, nPolarized, valShifter);

          lB = en + 1; // reset index such that polarity clusters are not overlapping
          j = en; // updated to j + 1 immediately after

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

