#ifndef EST_ABILITY_MAP_H
#define EST_ABILITY_MAP_H

#include <Rcpp.h>
Rcpp::List est_ability_map_single_examinee_cpp(
  Rcpp::NumericVector resp,
  Rcpp::S4 ip,
  std::string prior_dist = "norm",
  Rcpp::NumericVector prior_par = Rcpp::NumericVector::create(0, 1),
	Rcpp::NumericVector theta_range = Rcpp::NumericVector::create(-5, 5),
  double initial_theta = 0,
  double tol = 0.00001);
#endif
