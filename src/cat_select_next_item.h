#ifndef CATSELECTNEXTITEM_H
#define CATSELECTNEXTITEM_H

#include <Rcpp.h>
double loglik_est_history_cpp(Rcpp::List est_history, double theta,
                          bool calculate_loglik = true);
Rcpp::List select_next_item_cpp(Rcpp::List cd, Rcpp::List est_history,
                                Rcpp::List additional_args);

#endif
