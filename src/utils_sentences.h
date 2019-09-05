
#include <algorithm>

using namespace Rcpp;
using namespace std;

#ifndef UTILS_SENTENCE_H
#define UTILS_SENTENCE_H

inline void update_primary_shifters_sentence(std::vector< int >& shifters,
                                             double& valType,
                                             int& position) {
  if (valType == 1) shifters[0] += 1; // negators
  else if (valType == 2) shifters[1] += 1; // amplifiers
  else if (valType == 3) shifters[2] += 1; // deamplifiers
  else if (valType == 4) { // adversative conjunction
    if (position == 0) {
      shifters[3] += 1;
    }
  }
}

inline double compute_sentence_impact(std::vector<int>& shifters, int& position) {
  int n = shifters[0] % 2; // 0 if even number of negators, 1 if odd number of negators
  double wA = (1 - n) * shifters[1] * 0.8; // amplification impact
  double wD = (-n * shifters[1] - shifters[2]) * 0.8; // deamplification impact

  if (position != -1) {
    double b = (1 + shifters[3] * 0.25);
    if (b > 1) {
      wA += b;
    } else {
      wD -= b;
      if (wD <= -1 ) wD = -0.999;
    }
  }

  double impact = (1 + (wA + wD));
  if (n == 1){
    impact *= -1.0;
  }

  return(impact);
}

inline void update_token_scores_sentences(std::vector< double >& scores,
                                std::vector <std::vector< double > >& tokenScores,
                                double& normalizer,
                                std::vector< double >& nPolarized,
                                std::vector< double >& tokenShifters,
                                std::vector< double >& tokenWeights,
                                int nL,
                                int& nTokens,
                                std::string how,
                                int& nPuncts) {
  if (how != "proportional" && how != "counts" && how != "squareRootCounts") {
    scale_token_weights(tokenWeights, normalizer, nPolarized, how, nTokens, nL);
  }

  for (int i = 0; i < nTokens; i ++) {
    // std:::cout << "token loop " << i << "\n";
    for (int j = 0; j < nL; j++) {
      // std:::cout << "Lexicon loop " << j << "\n";
      if (tokenScores[i].size() != 0) {
        double score = tokenScores[i][j];
        // std::cout << "score within lexicon loop: " << score << "\n";
        if (score != 0) {
          if (how == "counts") {
            // std:::cout << "token shifter: " << tokenShifters[i] << " & score: " << score << "\n";
            // std:::cout << "score before: " << scores[j] << "\n";
            scores[j] += (tokenShifters[i] * score);
            // std:::cout << "score after: " << scores[j] << "\n";
          } else if (how == "squareRootCounts") {
            scores[j] += (tokenShifters[i] * score / std::sqrt(nTokens - nPuncts));
          } else if (how == "proportional") {
            scores[j] += (tokenShifters[i] * score / nTokens);
          } else if (how == "proportionalPol") {
            if (nPolarized[j] > 0)  scores[j] += (tokenShifters[i] * score) * (tokenWeights[i] / nPolarized[j]);
          } else {
            scores[j] += (tokenShifters[j] * score) * tokenWeights[i];
          }
        }
      }
    }
  }
}

inline void check_for_commas(std::string& token,
            int& nPuncts,
            int& punctPosition,
            int& j,
            std::vector< std::string >& tokens) {
  if (token == "c_c") {
    nPuncts += 1;
    punctPosition = j;
  }
}

inline void set_cluster_bounds(int& st,
                               int& en,
                               int& nPuncts,
                               int& punctPosition,
                               int& nTokens,
                               std::vector< std::string >& tokens,
                               int& lB,
                               int& nB,
                               int& nA,
                               int& j) {
  if (nPuncts != 0 ){
    st = punctPosition;
  } else {
    st = std::max(lB, j - nB);
  }
  en = std::min(nTokens, j + nA + 1);
  for (int m = en; m < nTokens; m++) {
    if (tokens[m] == "c_c") {
      en = m;
      break;
    }
  }
}

#endif

