#ifndef RESPSETMETHODS_H
#define RESPSETMETHODS_H

#include <Rcpp.h>
// Respond Log-Likelihood

bool check_validity_response_set_cpp(Rcpp::S4 resp_set, Rcpp::S4 ip);
Rcpp::Nullable<Rcpp::StringVector> get_examinee_id_response_set_cpp(
    Rcpp::S4 resp_set);
#endif
