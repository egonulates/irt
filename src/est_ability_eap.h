#ifndef EST_ABILITY_EAP_H
#define EST_ABILITY_EAP_H

#include <Rcpp.h>
Rcpp::List est_ability_eap_single_examinee_cpp(
  Rcpp::NumericVector resp,
  Rcpp::S4 ip,
  Rcpp::NumericVector theta_range = Rcpp::NumericVector::create(-5, 5),
  int no_of_quadrature = 41,
  std::string prior_dist = "norm",
  Rcpp::NumericVector prior_par = Rcpp::NumericVector::create(0, 1));
#endif
