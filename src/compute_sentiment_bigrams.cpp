
#include <Rcpp.h>
#include <RcppParallel.h>
#include "utils.h"
// [[Rcpp::depends(RcppParallel)]]

using namespace std;
using namespace Rcpp;
using namespace RcppParallel;

struct SentimentScorerBigrams : public Worker {

  // thread-safe input
  const std::vector< std::vector<std::string> > texts;
  const std::unordered_map< std::string, std::vector<double> > lexiconMap;
  const std::unordered_map<std::string, double> valenceMap;
  const std::string how;
  const int nL;

  // output
  RMatrix<double> sentScores;

  SentimentScorerBigrams(const std::vector< std::vector<std::string> > texts,
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
      int valPos = tokens.size() - 1; // this initialization avoids valPos + 1 equal to j even when no valence shifter
      int nTokens = tokens.size();

      for (int j = 0; j < nTokens; j++) {
        std::string token = tokens[j];
        if (valenceMap.find(token) != valenceMap.end()) { // assumes no duplicates across valence shifters
          valPos = j;
        } else if (lexiconMap.find(token) != lexiconMap.end()) { // assumes no duplicates across lexicon
          std::vector<double> lexScores = lexiconMap.at(token);
          if ((valPos + 1) == j) { // bigram valence shifting
            double valShifter = valenceMap.at(tokens[valPos]);
            update_scores(scores, lexScores, nPolarized, valShifter);
          } else {
            update_scores(scores, lexScores, nPolarized, 1.0);
          }
        }
      }

      if (how == "proportional") rescale_scores_proportional(scores, nTokens);
      else if (how == "proportionalPol") rescale_scores_proportionalPol(scores, nPolarized);

      for (int m = 0; m < nL; m++) {
        sentScores(i, m) = scores[m];
      }

    }
  }

};

// [[Rcpp::export]]
Rcpp::NumericMatrix compute_sentiment_bigrams(std::vector< std::vector<std::string> > texts,
                                              Rcpp::List lexicons,
                                              std::string how) {

  int nTexts = texts.size(); // already tokenized texts
  int nL = lexicons.size() - 1;
  Rcpp::CharacterVector lexNames = get_lexicon_names(lexicons.names(), nL);

  Rcpp::List valence = lexicons[nL];
  std::vector<std::string> wordsVal = as< std::vector<std::string> >(valence["x"]);
  int nVals = wordsVal.size();
  Rcpp::NumericVector scoresVal = valence["y"];
  std::unordered_map<std::string, double> valenceMap;
  for (int v = 0; v < nVals; v++) { // fill up valenceMap
    valenceMap[wordsVal[v]] = scoresVal[v];
  }

  std::unordered_map< std::string, std::vector<double> > lexiconMap = make_lexicon_map(lexicons, nL);

  Rcpp::NumericMatrix sentScores(nTexts, nL); // output matrix of sentiment scores
  SentimentScorerBigrams sentimentScorer(texts, lexiconMap, valenceMap, how, nL, sentScores);
  parallelFor(0, nTexts, sentimentScorer); // parallelized across texts

  colnames(sentScores) = lexNames;

  return(sentScores);
}

