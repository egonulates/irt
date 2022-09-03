#ifndef EST_ABILITY_ML_H
#define EST_ABILITY_ML_H

#include <Rcpp.h>
double est_ability_4pm_nr_itempool_cpp(
  Rcpp::NumericVector resp,
  Rcpp::S4 ip,
  Rcpp::NumericVector theta_range = Rcpp::NumericVector::create(-5, 5),
  double criterion = 0.001,
  Rcpp::Nullable<Rcpp::NumericVector> initial_estimates = R_NilValue);
#endif
