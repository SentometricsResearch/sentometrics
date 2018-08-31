
#include <Rcpp.h>
#include <RcppParallel.h>
#include "utils.h"

using namespace Rcpp;
using namespace RcppParallel;
using namespace std;

// [[Rcpp::depends(RcppParallel)]]

struct SentimentScorerOnegrams : public Worker {

  // input
  const std::vector< std::vector<std::string> > texts;
  const std::unordered_map<std::string, double> lexiconMap;
  const std::string how;

  // output
  RVector<double> sentScores;

  SentimentScorerOnegrams(const std::vector< std::vector<std::string> > texts,
                          const std::unordered_map<std::string, double> lexiconMap,
                          const std::string how,
                          Rcpp::NumericVector sentScores)
  : texts(texts), lexiconMap(lexiconMap), how(how), sentScores(sentScores) {}

  void operator()(std::size_t begin, std::size_t end) {

    for (std::size_t i = begin; i < end; i++) {

      std::vector<std::string> tokens = texts[i];
      float score = 0;
      int nTokens = tokens.size();
      int nPolarized = 0;

      for (int j = 0; j < nTokens; j++) {
        std::string token = tokens[j];
        if (lexiconMap.find(token) != lexiconMap.end()) { // assumes no duplicates across lexicon
          score += lexiconMap.at(token);
          nPolarized += 1;
        }
      }

      if (how == "counts") {
        sentScores[i] = score;
      } else if (how == "proportional") {
        sentScores[i] = score / nTokens;
      } else if (how == "proportionalPol") {
        sentScores[i] = score / nPolarized;
      }

    }

  }
};

// [[Rcpp::export]]
Rcpp::List compute_sentiment_onegrams(std::vector< std::vector<std::string> > texts,
                                      Rcpp::List lexicons,
                                      std::string how) {

  int nTexts = texts.size(); // already tokenized texts
  int L = lexicons.size();
  Rcpp::CharacterVector lexNames = get_seq_names(lexicons.names(), L);
  Rcpp::List out(L);

  for (int l = 0; l < L; l++) {
    Rcpp::List lexicon = lexicons[l];
    std::vector<std::string> words = as< std::vector<std::string> >(lexicon["x"]);
    Rcpp::NumericVector scores = lexicon["y"];
    int nWords = words.size();

    std::unordered_map<std::string, double> lexiconMap;
    for (int k = 0; k < nWords; k++) { // fill up lexiconMap
      lexiconMap[words[k]] = scores[k];
    }

    Rcpp::NumericVector sentScores(nTexts); // output vector of sentiment scores
    SentimentScorerOnegrams sentimentScorer(texts, lexiconMap, how, sentScores);
    parallelFor(0, nTexts, sentimentScorer); // parallelized across texts

    out[l] = sentScores;
  }
  out.attr("names") = lexNames;

  return out;
}

