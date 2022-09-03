#include <Rcpp.h>
#include "itempool_class_methods.h"
#include "response_set_methods.h"
#include "prob.h"
#include "resp_loglik.h"
using namespace Rcpp;


//##############################################################################
//##############################################################################
//########################### info_kl_item_bare_cpp ############################
//##############################################################################
//##############################################################################
// The formula for Kullback-Leibler information is:
// \deqn{
// KL(\theta_{true} || \hat \theta) = \sum_{k=1}^{m}P_k(\hat \theta)
//       \ln\left( \frac{P_k(\hat \theta)}{P_k(\theta_{True})} \right)
//       }
// where \eqn{m} is the number of categories (0 and 1 for dichotomous items),
// \eqn{P_k(\hat \theta)} is the probability of an examinee with ability
// \eqn{\hat \theta} endorsing category \eqn{k}.
//
//

// [[Rcpp::export]]
double info_kl_item_bare_cpp(Rcpp::NumericVector true_theta,
                             Rcpp::NumericVector theta_hat,
                             Rcpp::S4 item)
{
  Rcpp::NumericVector p_true = prob_bare_item_cpp(true_theta, item);
  Rcpp::NumericVector p_hat = prob_bare_item_cpp(theta_hat, item);
  double result = 0;
  for(unsigned int i = 0; i < p_true.size(); i++) {
    result = result + p_true[i] * log(p_true[i]/p_hat[i]);
  }
  return result;
}



//##############################################################################
//########################### info_kl_itempool_bare_cpp ########################
//##############################################################################

// // [[Rcpp::export]]
// Rcpp::NumericVector info_kl_itempool_bare_cpp(Rcpp::NumericVector true_theta,
//                                               Rcpp::NumericVector theta_hat,
//                                               Rcpp::S4 ip,
//                                               bool tif = false)

