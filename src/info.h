#ifndef INFO_H
#define INFO_H

#include <Rcpp.h>
// info
double info_item_bare_cpp(double theta, Rcpp::S4 item, bool observed, double resp);
Rcpp::NumericVector info_itempool_bare_cpp(double theta, Rcpp::S4 ip, 
  bool observed = false, Rcpp::Nullable<Rcpp::NumericVector> resp = R_NilValue);
double info_itempool_bare_tif_cpp(double theta, Rcpp::S4 ip, bool observed = false,
  Rcpp::Nullable<Rcpp::NumericVector> resp = R_NilValue);
double info_response_tif_cpp(double theta, Rcpp::S4 ip, Rcpp::S4 resp, 
  bool observed = false);

#endif
