
#include <Rcpp.h>
#include <RcppParallel.h>

using namespace Rcpp;
using namespace RcppParallel;
using namespace std;

// [[Rcpp::depends(RcppParallel)]]

Rcpp::CharacterVector get_seq_names(Rcpp::CharacterVector x, int end) {
  Rcpp::IntegerVector idx = Rcpp::seq(0, end-1);
  return x[idx];
}

struct SentimentScorerBigrams : public Worker {

  // input (thread-safe versions, as one cannot safely call R or Rcpp API; non-R variables are copied once)
  const std::vector< std::vector<std::string> > texts;
  const std::map<std::string, double> lexiconMap;
  const std::map<std::string, double> valenceMap;
  const std::string how;

  // output
  RVector<double> sentScores;

  SentimentScorerBigrams(const std::vector< std::vector<std::string> > texts,
                         const std::map<std::string, double> lexiconMap,
                         const std::map<std::string, double> valenceMap,
                         const std::string how,
                         Rcpp::NumericVector sentScores)
  : texts(texts), lexiconMap(lexiconMap), valenceMap(valenceMap), how(how), sentScores(sentScores) {}

  void operator()(std::size_t begin, std::size_t end) {

    for (std::size_t i = begin; i < end; i++) {

      std::vector<std::string> tokens = texts[i];
      float score = 0;
      int valPos = tokens.size() - 1; // this initialization avoids valPos + 1 equal to j even when no valence shifter
      int nTokens = tokens.size();
      int nPolarized = 0;

      for (int j = 0; j < nTokens; j++) {
        std::string token = tokens[j];
        if (valenceMap.find(token) != valenceMap.end()) { // assumes no duplicates across valence shifters
          valPos = j;
        } else if (lexiconMap.find(token) != lexiconMap.end()) { // assumes no duplicates across lexicon
          if ((valPos + 1) == j) { // bigram valence shifting
            score += valenceMap.at(tokens[valPos]) * lexiconMap.at(token);
          } else {
            score += lexiconMap.at(token);
          }
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
Rcpp::List compute_sentiment_bigrams(std::vector< std::vector<std::string> > texts,
                                     Rcpp::List lexicons,
                                     std::string how) {

  int nTexts = texts.size(); // already tokenized texts
  int L = lexicons.size() - 1;
  Rcpp::CharacterVector lexNames = get_seq_names(lexicons.names(), L);
  Rcpp::List out(L);
  Rcpp::List valence = lexicons[L];
  std::vector<std::string> wordsVal = as< std::vector<std::string> >(valence["x"]);
  int nVals = wordsVal.size();
  Rcpp::NumericVector scoresVal = valence["y"];

  std::map<std::string, double> valenceMap;
  for (int v = 0; v < nVals; v++) { // fill up valenceMap
    valenceMap[wordsVal[v]] = scoresVal[v];
  }

  for (int l = 0; l < L; l++) {
    Rcpp::List lexicon = lexicons[l];
    std::vector<std::string> words = as< std::vector<std::string> >(lexicon["x"]);
    Rcpp::NumericVector scores = lexicon["y"];
    int nWords = words.size();

    std::map<std::string, double> lexiconMap;
    for (int k = 0; k < nWords; k++) { // fill up lexiconMap
      lexiconMap[words[k]] = scores[k];
    }

    Rcpp::NumericVector sentScores(nTexts); // output vector of sentiment scores
    SentimentScorerBigrams sentimentScorer(texts, lexiconMap, valenceMap, how, sentScores);
    parallelFor(0, nTexts, sentimentScorer); // parallelized across texts

    out[l] = sentScores;
  }
  out.attr("names") = lexNames;

  return out;
}

