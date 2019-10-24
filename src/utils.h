
#include <algorithm>

using namespace Rcpp;
using namespace std;

#ifndef UTILS_H
#define UTILS_H

inline bool is_frequency_weighting(std::string how) {
  return (how == "TF" || how == "logarithmicTF" || how == "augmentedTF" || how == "IDF"
            || how == "TFIDF" || how == "logarithmicTFIDF"
            || how == "augmentedTFIDF");
}

inline Rcpp::CharacterVector prepare_column_names(Rcpp::CharacterVector x, int n) {
  Rcpp::CharacterVector names(n + 1);
  names[0] = "word_count";
  for (int i = 0; i < n; i++) {
    names[i + 1] = x[i]; // add lexicon names
  }
  return(names);
}

inline std::unordered_map< std::string, std::vector< double > > make_lexicon_map(Rcpp::List lexicons, int nL) {

  std::unordered_map< std::string, std::vector< double > > lexiconMap;

  for (int l = 0; l < nL; l++) {
    Rcpp::List lexicon = lexicons[l];
    std::vector< std::string > words = Rcpp::as< std::vector< std::string > >(lexicon["x"]);
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
  std::unordered_map< std::string, double > valenceMap;

  std::vector< std::string > wordsVal = Rcpp::as< std::vector< std::string > >(valence["x"]);
  Rcpp::NumericVector values = valence[1]; // second column (either "y" or "t")
  int nVals = wordsVal.size();

  for (int v = 0; v < nVals; v++) { // fill up valenceMap
    valenceMap[wordsVal[v]] = values[v];
  }

  return(valenceMap);
}

inline void update_frequency_map(std::unordered_map< std::string, double >& freqMap,
                                 std::unordered_map< int, std::unordered_map< std::string, double > >& frequencyMap,
                                 int i) {
  freqMap = frequencyMap.at(i);
}

inline void update_max_token_frequency(double& maxTokenFrequency,
                                       std::unordered_map< std::string, double >& frequencyMap,
                                       std::string how) {
  if (how == "augmentedTF" || how == "augmentedTFIDF") {
    int globalMax = 0;
    int localMax = 0;
    for (auto& it: frequencyMap) {
      localMax = it.second;
      if (localMax > globalMax) globalMax = localMax;
    }
    maxTokenFrequency = globalMax;
  }
}

inline void update_token_frequency(double& tokenFrequency,
                                     std::unordered_map< std::string, double >& freqMap,
                                     std::string& token) {
  tokenFrequency = freqMap[token];
}

inline void update_token_inverse_frequency(double& tokenInverseFrequency,
                                           std::unordered_map< std::string, double >& inverseFrequencyMap,
                                           std::string& token,
                                           std::string how) {
  if (how == "IDF" || how == "TFIDF" || how == "logarithmicTFIDF" || how == "augmentedTFIDF") {
    tokenInverseFrequency = inverseFrequencyMap[token];
  }
}

inline void update_token_weights(std::vector < double >& tokenWeights,
                                 double& normalizer,
                                 std::vector< double >& nPolarized,
                                 int& j,
                                 int& nTokens,
                                 std::string how,
                                 int nL,
                                 std::vector< std::vector< double > >& tokenScores,
                                 double& frequency,
                                 double& inverseFrequency,
                                 double& maxTokenFrequency,
                                 int N) {

  if (how == "proportionalPol") {
    if (tokenScores.size() != 0) {
      for (int i = 0; i < nL; i++) {
        if (tokenScores[j][i] != 0.0) {
          nPolarized[i] += 1.0;
        }
      }
    }
  } else {

    double token_weight = 0.0;
    double x = (double) j + 1; // because 0-based indexing
    double y = (double) nTokens;

    if (how == "UShaped") {
      token_weight = std::pow(x - ((y + 1) / 2), 2);
    } else if (how == "inverseUShaped") {
      token_weight = 0.25 + (-std::pow(x - ((y + 1) / 2), 2)) / std::pow(y, 2);
    } else if (how == "exponential") {
      token_weight = std::exp(5.0 * (x / y - 1));
    } else if (how == "inverseExponential") {
      token_weight = std::exp(5.0 * (1 - x / y));
    } else if (how == "TF") {
      token_weight = frequency / nTokens;
    } else if (how == "logarithmicTF") {
      token_weight = std::log(1 + frequency / nTokens);
    } else if (how == "augmentedTF") {
      token_weight = 1 + (frequency / maxTokenFrequency);
    } else if (how == "IDF") {
      token_weight = std::log(N / (1 + inverseFrequency));
    } else if (how == "TFIDF") {
      token_weight = std::log(N / (1 + inverseFrequency)) * (frequency / nTokens);
    } else if (how == "logarithmicTFIDF") {
      token_weight = std::log(N / (1 + inverseFrequency)) * (std::log(1 + frequency / nTokens));
    } else if (how == "augmentedTFIDF") {
      token_weight = std::log(N / (1 + inverseFrequency)) * (1 + (frequency / maxTokenFrequency));
    }
    // std::cout << "freq.: " << frequency << " & inv. freq.: " << inverseFrequency << "\n";
    // std::cout << "weight: " << token_weight << "\n";
    normalizer += token_weight;
    tokenWeights[j] = token_weight;
  }

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

inline void scale_token_weights(std::vector < double >& tokenWeights,
                                double& normalizer,
                                int& nTokens) {
  for (int i = 0; i < nTokens; i++) {
    tokenWeights[i] /= normalizer;
  }
}

inline void update_token_scores(std::vector< double >& scores,
                                std::vector <std::vector< double > >& tokenScores,
                                double& normalizer,
                                std::vector< double >& nPolarized,
                                std::vector< double >& tokenShifters,
                                std::vector< double >& tokenWeights,
                                int nL,
                                int& nTokens,
                                std::string how,
                                int& nPuncts) {
  if (how != "proportional" && how != "proportionalPol"
        && how != "counts" && how != "proportionalSquareRoot") {
    scale_token_weights(tokenWeights, normalizer, nTokens);
  }

  for (int i = 0; i < nTokens; i++) {
    // std::cout << "token: " << i << "\n";
    for (int j = 0; j < nL; j++) {
      // std::cout << "lexicon: " << j << "\n";
      if (tokenScores[i].size() != 0) {
        double score = tokenScores[i][j];
        // std::cout << "score within lexicon: " << score << "\n";
        if (score != 0) {
          if (how == "counts") {
            // std::cout << "token shifter: " << tokenShifters[i] << " & score: " << score << "\n";
            // std::cout << "score before: " << scores[j] << "\n";
            scores[j] += tokenShifters[i] * score;
            // std::cout << "score after: " << scores[j] << "\n";
          } else if (how == "proportional") {
            scores[j] += (tokenShifters[i] * score) / (nTokens - nPuncts);
          } else if (how == "proportionalPol") {
            if (nPolarized[j] > 0) scores[j] += (tokenShifters[i] * score) / nPolarized[j];
          } else if (how == "proportionalSquareRoot") {
            scores[j] += (tokenShifters[i] * score) / std::sqrt(nTokens - nPuncts);
          } else {
            scores[j] += (tokenShifters[j] * score) * tokenWeights[i];
          }
        }
      }
    }
  }
}

inline void update_primary_shifters(std::vector< int >& shifters,
                                    double& valType) {
  if (valType == 1) shifters[0] += 1; // negators
  else if (valType == 2) shifters[1] += 1; // amplifiers
  else if (valType == 3) shifters[2] += 1; // deamplifiers
}

inline double compute_cluster_impact(std::vector<int>& shifters) {
  int n = shifters[0] % 2; // 0 if even number of negators, 1 if odd number of negators
  double wA = (1 - n) * shifters[1]; // amplification impact
  double wD = n * shifters[1] + shifters[2]; // deamplification impact
  double impact = 1 + std::max(0.8 * (wA - wD), -1.0) ;
  if (n == 1) impact *= -1.0; // apply negation
  return(impact);
}

inline void make_frequency_maps(std::unordered_map< int, std::unordered_map< std::string, double > >& frequencyMap,
                                std::unordered_map< std::string, double >& inverseFrequencyMap,
                                std::vector< std::vector< std::string > >& texts) {
  int nTexts = texts.size();
  for (int i = 0; i < nTexts; i++) {
    int nTokens = texts[i].size();
    std::unordered_map< std::string, double > textFreq;
    for (int j = 0; j < nTokens; j++) {
      std::string token = texts[i][j];
      textFreq[token] += 1.0;
      frequencyMap[nTexts][token] += 1.0; // count total and store in last element
      if (textFreq[token] == 1.0) {
        inverseFrequencyMap[token] += 1.0; // count number of docs or sentences where token occurs
      }
    }
    frequencyMap[i] = textFreq;
  }
}

inline void update_primary_shifters_sentence(std::vector< int >& shifters,
                                             double& valType,
                                             int& position) {
  if (valType == 1) shifters[0] += 1; // negators
  else if (valType == 2) shifters[1] += 1; // amplifiers
  else if (valType == 3) shifters[2] += 1; // deamplifiers
  else if (valType == 4) { // adversative conjunction
    if (position == 0) { // before hit
      shifters[3] += 1;
    } else if (position == 1) { // after hit
      shifters[3] -= 1;
    }
  }
}

inline double compute_sentence_impact(std::vector<int>& shifters) {
  int n = shifters[0] % 2; // 0 if even number of negators, 1 if odd number of negators
  double wA = (1 - n) * shifters[1] * 0.8; // amplification impact
  double wD = (-n * shifters[1] - shifters[2]) * 0.8; // deamplification impact
  if (wD < -1) wD = -1;
  double b = (1 + shifters[3] * 0.25);

  double impact = (1 + (wA + wD)) * b;
  if (n == 1) {
    impact *= -1.0;
  }

  return(impact);
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
  if (nPuncts != 0 ) {
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

