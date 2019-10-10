
#include <Rcpp.h>
#include "utils.h"
using namespace Rcpp;

// [[Rcpp::export]]
List get_dtf_vectors(std::vector<std::vector<std::string>> texts) {
  std::unordered_map< int, std::unordered_map< std::string, double > > tokenMap;
  std::unordered_map< std::string, double > docMap;
  make_frequency_maps(tokenMap, docMap, texts);
  return List::create(Named("DF") = docMap, Named("TF") = tokenMap);
}

