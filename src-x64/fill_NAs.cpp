
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::NumericMatrix fill_NAs(Rcpp::NumericMatrix x) {
  int n = x.nrow();
  int m = x.ncol();
  for (int i = 0; i < m; i++) {
    int k = 0; // current index of fill value
    Rcpp::NumericVector col = x(_, i);
    for (int j = 0; j < n; j++) {
      if (NumericVector::is_na(col[j])) { 
        col[j] = col[k]; // add in fill value
      } else {
        k = j; // update index
      }
    }
    x(_, i) = col;
  }
  return(x);
}

