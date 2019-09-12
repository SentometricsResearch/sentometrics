
#include <Rcpp.h>
#include "utils.h"
using namespace Rcpp;

// [[Rcpp::export]]
List make_frequency_maps(std::vector<std::vector<std::string>> texts,
                         std::vector<std::string> ids, bool byText) {

  std::unordered_map< int, std::unordered_map< std::string, double > > frequencyMap;
  std::unordered_map< std::string, double > inverseFrequencyMap;
  int nTexts = texts.size();
  make_frequency_maps(frequencyMap, inverseFrequencyMap, texts);
  Rcpp::List freqMapList;

  if (byText) {
    for (int i = 0; i < nTexts; i++) {
      freqMapList.push_back( frequencyMap.at(i), ids.at(i));
    }
  }
  freqMapList.push_back(frequencyMap.at(nTexts ), "Corpus");
  return List::create(Named("DF") = inverseFrequencyMap, Named("TF") = freqMapList);
}

