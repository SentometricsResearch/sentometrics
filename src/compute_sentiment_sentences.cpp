
#include <Rcpp.h>
#include <RcppParallel.h>
#include "utils.h"
#include "utils_sentences.h"
#include "SentimentScorerSentences.h"

// [[Rcpp::depends(RcppParallel)]]

using namespace std;
using namespace Rcpp;
using namespace RcppParallel;

// [[Rcpp::export]]
Rcpp::NumericMatrix compute_sentiment_sentences(std::vector<std::vector<std::string>> texts,
                                                Rcpp::List lexicons,
                                                std::string how,
                                                bool hasValenceShifters) {

  int nTexts = texts.size(); // already tokenized texts
  int nL = 0;
  if (hasValenceShifters){
    nL = lexicons.size() - 1;
  } else {
    nL = lexicons.size();
  }
  bool isFreqWeighting = is_frequency_weighting(how);

  Rcpp::CharacterVector colNames = prepare_column_names(lexicons.names(), nL);

  std::unordered_map< std::string, std::vector< double> > lexiconMap = make_lexicon_map(lexicons, nL);
  std::unordered_map< int, std::unordered_map< std::string, double > > frequencyMap;
  std::unordered_map< std::string, double > inverseFrequencyMap;
  if (isFreqWeighting) {
    make_frequency_maps(frequencyMap, inverseFrequencyMap, texts);
  }
  Rcpp::List valenceList;
  Rcpp::CharacterVector valenceCols;
  std::unordered_map< std::string, double > valenceMap;
  if (hasValenceShifters) {
    valenceList = lexicons[nL];
    valenceCols = valenceList.names();
    valenceMap = make_valence_map(valenceList);
  }

  Rcpp::NumericMatrix sentScores(nTexts, nL + 1);

  SentimentScorerSentences sentimentScorer(texts, lexiconMap, valenceMap, how, nL, frequencyMap, inverseFrequencyMap, isFreqWeighting, hasValenceShifters, sentScores);
  parallelFor(0, nTexts, sentimentScorer);

  colnames(sentScores) = colNames;

  return(sentScores);

}

