
#include <Rcpp.h>
#include <RcppParallel.h>
#include "utils.h"
#include "SentimentScorerSentences.h"

// [[Rcpp::depends(RcppParallel)]]

using namespace std;
using namespace Rcpp;
using namespace RcppParallel;

// [[Rcpp::export]]
Rcpp::NumericMatrix compute_sentiment_sentences(std::vector<std::vector<std::string>> texts,
                                                Rcpp::List lexicons,
                                                std::string how,
                                                int valenceType) {

  int N = texts.size(); // already tokenized texts
  int nL = lexicons.size();
  if (valenceType != 0) {
    nL = lexicons.size() - 1; // the last one has the valence shifters
  }
  bool isFreqWeighting = is_frequency_weighting(how);

  Rcpp::CharacterVector colNames = prepare_column_names(lexicons.names(), nL);

  std::unordered_map< std::string, std::vector< double> > lexiconMap = make_lexicon_map(lexicons, nL);
  std::unordered_map< int, std::unordered_map< std::string, double > > frequencyMap;
  std::unordered_map< std::string, double > inverseFrequencyMap;
  if (isFreqWeighting) {
    make_frequency_maps(frequencyMap, inverseFrequencyMap, texts);
  }
  std::unordered_map< std::string, double > valenceMap;
  if (valenceType != 0) {
    Rcpp::List valenceList = lexicons[nL];
    valenceMap = make_valence_map(valenceList);
  }

  Rcpp::NumericMatrix sentScores(N, nL + 1);

  SentimentScorerSentences sentimentScorer(texts, lexiconMap, valenceMap, how, nL, N, frequencyMap, inverseFrequencyMap, isFreqWeighting, valenceType, sentScores);
  parallelFor(0, N, sentimentScorer);

  colnames(sentScores) = colNames;

  return(sentScores);

}

