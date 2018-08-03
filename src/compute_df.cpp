
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace arma;
using namespace Rcpp;

// elastic net degrees-of-freedom estimator (Tibshirani and Taylor, 2012)

// [[Rcpp::export]]
Rcpp::NumericVector compute_df(double alpha, Rcpp::NumericVector lambda, Rcpp::List xA) {
  int nLambda = lambda.size();
  Rcpp::NumericVector dfA(nLambda);
  for (int i = 0; i < nLambda; i++) {
    arma::mat matr = xA[i];
    double nA = matr.n_cols;
    if (nA == 0) {
      dfA[i] = 1L;
    } else if (alpha == 1) { // lasso df
      dfA[i] = nA;
    } else if (alpha == 0) { // ridge df
      arma::vec s = arma::svd(matr);
      double estimate = arma::sum(arma::sqrt(s) / (arma::sqrt(s) + lambda[i]));
      dfA[i] = estimate;
    } else { // elastic net df
      arma::mat inverted = (matr * inv(matr.t() * matr + (1 - alpha) * lambda[i] * arma::eye<arma::mat>(nA, nA))) * matr.t();
      double estimate = arma::sum(arma::diagvec(inverted));
      dfA[i] = estimate; // potential need for error handling in case no inversion feasible
    }
  }
  return dfA;
}

