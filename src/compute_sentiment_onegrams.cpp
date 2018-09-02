
#include <Rcpp.h>
#include <RcppParallel.h>
#include "utils.h"
// [[Rcpp::depends(RcppParallel)]]

using namespace std;
using namespace Rcpp;
using namespace RcppParallel;

struct SentimentScorerOnegrams : public Worker {

  // thread-safe input
  const std::vector< std::vector<std::string> > texts;
  const std::unordered_map< std::string, std::vector<double> > lexiconMap;
  const std::string how;
  const int nL;

  // output
  RMatrix<double> sentScores;

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

// [[Rcpp::export]]
Rcpp::NumericMatrix compute_sentiment_onegrams(std::vector< std::vector<std::string> > texts,
                                               Rcpp::List lexicons,
                                               std::string how) {

  int nTexts = texts.size(); // already tokenized texts
  int nL = lexicons.size();
  Rcpp::CharacterVector colNames = prepare_column_names(lexicons.names(), nL);

  std::unordered_map< std::string, std::vector<double> > lexiconMap = make_lexicon_map(lexicons, nL);

  Rcpp::NumericMatrix sentScores(nTexts, nL + 1); // output matrix of word count and sentiment scores
  SentimentScorerOnegrams sentimentScorer(texts, lexiconMap, how, nL, sentScores);
  parallelFor(0, nTexts, sentimentScorer); // parallelized across texts

  colnames(sentScores) = colNames;

  return(sentScores);
}

