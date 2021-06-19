
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace arma;
using namespace Rcpp;

// elastic net degrees of freedom estimator (Tibshirani and Taylor, 2012)

// [[Rcpp::export]]
Rcpp::NumericVector compute_df(double alpha,
                               Rcpp::NumericVector lambda,
                               Rcpp::List xA) {
  int nLambda = lambda.size();
  Rcpp::NumericVector dfA(nLambda);
  for (int i = 0; i < nLambda; i++) {
    arma::mat matr = xA[i];
    double nA = matr.n_cols;
    if (nA == 0) {
      dfA[i] = 1L;
    } else if (alpha == 0) { // ridge df
      arma::vec s;
      bool pass = arma::svd(s, matr);
      if (pass == true) {
        arma::vec ss = arma::pow(s, 2);
        double estimate = arma::sum(ss / (ss + lambda[i]));
        dfA[i] = estimate;
      } else {
        dfA[i] = NumericVector::get_na();
      }
    } else if (alpha == 1) { // lasso df
      dfA[i] = nA;
    } else { // elastic net df
      arma::mat inverted;
      arma::mat toInvert = matr.t() * matr + (1 - alpha) * lambda[i] * arma::eye<arma::mat>(nA, nA);
      bool pass = arma::inv(inverted, toInvert);
      if (pass == true) {
        double estimate = arma::sum(arma::diagvec(matr * inverted * matr.t()));
        dfA[i] = estimate;
      } else {
        dfA[i] = NumericVector::get_na();
      }
    }
  }
  return dfA;
}

