
#include <Rcpp.h>
#include <RcppParallel.h>
#include "utils.h"
#include "SentimentScorerOnegrams.h"

// [[Rcpp::depends(RcppParallel)]]
using namespace std;
using namespace Rcpp;
using namespace RcppParallel;

// [[Rcpp::export]]
Rcpp::NumericMatrix compute_sentiment_onegrams(std::vector< std::vector<std::string>> texts,
                                               Rcpp::List lexicons,
                                               std::string how) {

  int nTexts = texts.size(); // already tokenized texts
  int nL = lexicons.size();
  bool isFreqWeighting = is_frequency_weighting(how);
  Rcpp::CharacterVector colNames = prepare_column_names(lexicons.names(), nL);

  std::unordered_map< std::string, std::vector< double> > lexiconMap = make_lexicon_map(lexicons, nL);
  std::unordered_map< int, std::unordered_map< std::string, double > > frequencyMap;
  std::unordered_map< std::string, double > inverseFrequencyMap;

  if (isFreqWeighting) {
    make_frequency_maps(frequencyMap, inverseFrequencyMap, texts);
  }
  Rcpp::NumericMatrix sentScores(nTexts, nL + 1); // output matrix of word count and sentiment scores
  SentimentScorerOnegrams sentimentScorer(texts, lexiconMap, how, nL, frequencyMap, inverseFrequencyMap, isFreqWeighting, sentScores);
  parallelFor(0, nTexts, sentimentScorer);
  colnames(sentScores) = colNames;

  return(sentScores);
}

