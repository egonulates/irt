#ifndef PROB_H
#define PROB_H

#include <Rcpp.h>
// prob

double prob_4pm_bare_cpp(double theta, Rcpp::S4 item, int derivative = 0,
                         double resp = -9);
Rcpp::NumericVector prob_4pm_item_cpp(Rcpp::NumericVector theta, Rcpp::S4 item,
                                      int derivative = 0);
Rcpp::NumericVector prob_grm_bare_cpp(double theta, Rcpp::S4 item,
                                      int derivative = 0);
Rcpp::NumericVector prob_gpcm_bare_cpp(double theta, Rcpp::S4 ip,
                                       int derivative = 0,
                                       double resp = -9);
Rcpp::NumericVector prob_poly_bare_cpp(double theta, Rcpp::S4 item,
                                       int derivative = 0,
                                       double resp = -9,
                                       bool expected_value = false);
Rcpp::NumericVector prob_bare_item_cpp(Rcpp::NumericVector theta,
                                       Rcpp::S4 item,
                                       int derivative = 0,
                                       double resp = -9,
                                       bool expected_value = false);
#endif
