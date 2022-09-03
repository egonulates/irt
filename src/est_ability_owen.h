#ifndef EST_ABILITY_OWEN_H
#define EST_ABILITY_OWEN_H

#include <Rcpp.h>
Rcpp::List est_ability_owen_cpp(Rcpp::S4 ip, Rcpp::NumericVector resp,
                                double m0, double v0);
#endif
