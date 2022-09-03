#ifndef MISC_H
#define MISC_H

#include <Rcpp.h>

bool check_item_model(Rcpp::S4 item,
                      bool is_dichotomous = true,
                      bool is_unidimensional = true);
double integrate(Rcpp::NumericVector x, Rcpp::NumericVector fx);

#endif
