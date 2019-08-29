
#include <algorithm>

using namespace Rcpp;
using namespace std;

#ifndef UTILS_H
#define UTILS_H

inline bool is_frequency_weighting(std::string how) {
  return (how == "TF" || how =="logarithmicTF" || how == "augmentedTF" ||how == "IDF"
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
    if (how =="augmentedTF" || how == "augmentedTFIDF" ) {
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
                                 double& maxTokenFrequency) {
  double token_weight = 0.0;
  double x = (double) j + 1;
  double y = (double) nTokens;

  if (how == "proportionalPol") {
    if (tokenScores.size() != 0) {
      token_weight = 1.0; // all the others need to be scaled in scale function
      for (int i = 0; i < nL; i++) {
        if (tokenScores[j][i] != 0.0) {
          nPolarized[i] += 1.0;
        }
      }
    }
  } else {
    if (how == "UShaped") {
      token_weight = std::pow(x - y / 2, 2) / std::pow(y, 2);
    } else if (how == "invertedUShaped") {
      token_weight = 0.25 + (- std::pow(x - y / 2, 2)) / std::pow(y, 2);
    } else if (how == "exponential") {
      token_weight = std::exp(x / y) - 1.0;
    } else if (how == "invertedExponential") {
      token_weight =  std::exp(1.0 - x / y) - 1.0;
    } else if (how == "TF") {
      token_weight =  frequency / nTokens;
    } else if (how == "logarithmicTF") {
      token_weight = std::log( 1 + frequency / nTokens);
    } else if (how =="augmentedTF") {
      token_weight = (0.5 + 0.5 * frequency / maxTokenFrequency) / nTokens;
    } else if (how == "IDF") {
      token_weight =  std::log(frequency / inverseFrequency);
    } else if (how == "TFIDF") {
      token_weight =  std::log(frequency / inverseFrequency) * (frequency / nTokens);
    } else if (how == "logarithmicTFIDF") {
      token_weight =  std::log(frequency / inverseFrequency) * (std::log( 1 + frequency / nTokens));
    } else if (how == "augmentedTFIDF") {
      token_weight =  std::log(frequency / inverseFrequency) * (0.5 + 0.5 * frequency / maxTokenFrequency);
    }
    normalizer += token_weight;
  }
  tokenWeights[j] = token_weight;
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
                                std::vector< double >& nPolarized,
                                std::string how,
                                int& nTokens,
                                int nL) {
   if (how !="proportionalPol") {
    for (int i = 0; i < nTokens; i++) {// token loop
        tokenWeights[i] /= normalizer;
    }
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
                                std::string how) {
  if (how != "proportional" && how != "counts" && how != "squareRootCounts") {
    scale_token_weights(tokenWeights, normalizer, nPolarized, how, nTokens, nL);
  }

  for (int i = 0; i < nTokens; i ++) {
    // std::cout<< "Tokenloop " << i << "\n";
    for (int j = 0; j < nL; j++) {
      // std::cout<< "Lexicon loop " << j << "\n";
      if (tokenScores[i].size() != 0) {
        double score = tokenScores[i][j];
       // std::cout<< "score within lexicon loop: " << score << "\n";
        if (score != 0) {
          if (how == "counts") {
            // std::cout<< "tokenshifter: " << tokenShifters[i] << " & score: " << score << "\n";
            // std::cout<< "score before: " <<scores[j]<< "\n";
            scores[j] += (tokenShifters[i] * score);
            // std::cout<< "score after: " <<scores[j]<< "\n";
          } else if (how == "squareRootCounts") {
            scores[j] += (tokenShifters[i] * score / std::sqrt(nTokens));
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
        inverseFrequencyMap[token] += 1.0; // count number of docs where token occurs
      }
    }
    frequencyMap[i] = textFreq;
  }
}

#endif

