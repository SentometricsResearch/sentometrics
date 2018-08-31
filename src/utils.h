
#ifndef UTILS_H
#define UTILS_H

inline Rcpp::CharacterVector get_seq_names(Rcpp::CharacterVector x, int end) {
  Rcpp::IntegerVector idx = Rcpp::seq(0, end - 1);
  return x[idx];
}

#endif // UTILS_H

