
#ifndef UTILS_H
#define UTILS_H

inline Rcpp::CharacterVector prepare_column_names(Rcpp::CharacterVector x, int n) {
  Rcpp::CharacterVector names(n + 1);
  names[0] = "word_count";
  for (int i = 0; i < n; i++) {
    names[i + 1] = x[i]; // add lexicon names
  }
  return(names);
}

inline std::unordered_map< std::string, std::vector<double> > make_lexicon_map(Rcpp::List lexicons,
                                                                               int nL) {
  std::unordered_map< std::string, std::vector<double> > lexiconMap;

  for (int l = 0; l < nL; l++) {
    Rcpp::List lexicon = lexicons[l];
    std::vector<std::string> words = Rcpp::as< std::vector<std::string> >(lexicon["x"]);
    Rcpp::NumericVector scores = lexicon["y"];
    int nWords = words.size();

    for (int k = 0; k < nWords; k++) { // fill up lexiconMap
      if (lexiconMap.find(words[k]) != lexiconMap.end()) {
        std::vector<double> values = lexiconMap.at(words[k]);
        values[l] = scores[k];
        lexiconMap[words[k]] = values;
      } else {
        std::vector<double> values(nL, 0.0);
        values[l] = scores[k];
        lexiconMap[words[k]] = values;
      }
    }
  }

  return(lexiconMap);
}

inline std::unordered_map< std::string, double > make_valence_map(Rcpp::List valence) {
  std::unordered_map<std::string, double> valenceMap;

  std::vector<std::string> wordsVal = Rcpp::as< std::vector<std::string> >(valence["x"]);
  Rcpp::NumericVector values = valence[1]; // second column (either "y" or "t")
  int nVals = wordsVal.size();

  for (int v = 0; v < nVals; v++) { // fill up valenceMap
    valenceMap[wordsVal[v]] = values[v];
  }

  return(valenceMap);
}

// inline bool is_pause_character(std::string token) {
//   bool pause;
//   if (token == ".") pause = true;
//   else if (token == ",") pause = true;
//   else if (token == ":") pause = true;
//   else if (token == ";") pause = true;
//   else pause = false;
//   return(pause);
// }

inline void update_scores(std::vector<double>& scores,
                          std::vector<double> lexScores,
                          std::vector<double>& nPolarized,
                          double shifter) {
  int n = scores.size();
  for (int i = 0; i < n; i++) {
    double score = lexScores[i];
    if (score != 0) { // even if the score is zero in the lexicon, we do not consider it as polarized
      scores[i] += shifter * score;
      nPolarized[i] += 1.0;
    }
  }
}

inline void rescale_scores_proportional(std::vector<double>& scores,
                                        int nTokens) {
  int n = scores.size();
  for (int j = 0; j < n; j++) {
    if (nTokens > 0) scores[j] /= nTokens;
  }
}

inline void rescale_scores_proportionalPol(std::vector<double>& scores,
                                           std::vector<double> nPolarized) {
  int n = scores.size();
  for (int j = 0; j < n; j++) {
    if (nPolarized[j] > 0) scores[j] /= nPolarized[j];
  }
}

inline void update_primary_shifters(std::vector<int>& shifters,
                                    double valType) {
  if (valType == 1) shifters[0] += 1; // negators
  else if (valType == 2) shifters[1] += 1; // amplifiers
  else if (valType == 3) shifters[2] += 1; // deamplifiers
}

inline double compute_cluster_impact(std::vector<int> shifters) {
  int n = shifters[0] % 2; // 0 if even number of negators, 1 if odd number of negators
  double wA = (1 - n) * shifters[1]; // amplification impact
  double wD = n * shifters[1] + shifters[2]; // deamplification impact
  double impact = 1 + std::max(0.8 * (wA - wD), -1.0);
  if (n == 1) impact *= -1.0; // apply negation
  return(impact);
}

#endif

