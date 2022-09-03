#ifndef ITEMPOOLCLASSMETHODS_H
#define ITEMPOOLCLASSMETHODS_H

#include <Rcpp.h>

Rcpp::IntegerVector get_itempool_size(Rcpp::S4 ip);
Rcpp::StringVector get_ids_itempool_cpp(Rcpp::S4 ip);
Rcpp::StringVector get_item_ids_itempool_cpp(Rcpp::S4 ip);
Rcpp::StringVector get_testlet_ids_itempool_cpp(Rcpp::S4 ip);
Rcpp::Nullable<Rcpp::StringVector> get_slot_itempool_cpp(Rcpp::S4 ip, std::string slotName);
Rcpp::NumericMatrix get_parameters_itempool_cpp(Rcpp::S4 ip);
Rcpp::List flatten_itempool_cpp(Rcpp::S4 ip);
// get_maximum_possible_score
Rcpp::NumericVector get_max_possible_score_itempool_cpp(Rcpp::S4 ip);
int get_max_possible_score_item_cpp(Rcpp::S4 item);
#endif
