#include <Rcpp.h>
#include "misc.h"
#include "itempool_class_methods.h"
#include "response_set_methods.h"
#include "resp_lik.h"
#include "resp_loglik.h"
#include "info.h"
#include "gh.h"

using namespace Rcpp;



//#############################################################################@
//#############################################################################@
//########################### est_ability_map ##################################
//#############################################################################@
//#############################################################################@

//#############################################################################@
//########################### est_ability_map_single_examinee_cpp ##############
//#############################################################################@
//' This function will be used in CAT simulations
//'
//' Note that this function needs to be tested for psychometric models other
//' than unidimensional dichotomous models (Rasch, 1PL, 2PL, 3PL)
//'
//' @noRd

// [[Rcpp::export]]
Rcpp::List est_ability_map_single_examinee_cpp(
    Rcpp::NumericVector resp,
    Rcpp::S4 ip,
	std::string prior_dist = "norm",
    Rcpp::NumericVector prior_par = Rcpp::NumericVector::create(0, 1),
	Rcpp::NumericVector theta_range = Rcpp::NumericVector::create(-5, 5),
    double initial_theta = 0,
    double tol = 0.00001)
{
  Rcpp::List output;

  if (prior_dist != "norm") {
    stop("Invalid prior distribution. MAP is only available for 'norm'.");
  }
  double prior_mean = prior_par[0];
  double prior_sd = prior_par[1];

  double theta_est = initial_theta;
  double diff = 999;
  double d1_ll;
  double d2_ll = 0;
  double d1_ll_pre = 999; // previous first derivative of log likelihood

  while (diff > tol) {
    // first derivative of log-likelihood of responses
	d1_ll = resp_loglik_bare_itempool_cpp(resp, theta_est, ip, 1);
    // add the first derivative of prior to the first der. of log-likelihood
    d1_ll = d1_ll - (theta_est - prior_mean) / pow(prior_sd, 2);

    // second derivative of log-likelihood of responses
    d2_ll = resp_loglik_bare_itempool_cpp(resp, theta_est, ip, 2);
    // add the second derivative of prior to the second der. of log-likelihood
    d2_ll = d2_ll - 1 / pow(prior_sd, 2);

    theta_est = theta_est - (d1_ll / d2_ll);
    diff = std::fabs(d1_ll - d1_ll_pre);
    d1_ll_pre = d1_ll;
  }
  if (theta_est < theta_range[0]) theta_est = theta_range[0];
  if (theta_est > theta_range[1]) theta_est = theta_range[1];
  output["est"] = theta_est;
  output["se"] = 1/pow(info_itempool_bare_tif_cpp(theta_est, ip) +
    1 / pow(prior_sd, 2), 0.5);
  return output;
}




//#############################################################################@
//########################### est_ability_map_response_cpp #####################
//#############################################################################@
//' Estimate Ability using MAP (Bayes Modal Estimation)
//'
//' @description Estimate the ability using Bayes Modal (or MAP) estimation
//'   via Newton-Raphson algorighm.
//' @param resp A Response object.
//' @param ip An Itempool object
//' @param prior_dist A string for the name of the prior distribution.
//'   Currently, only normal distribution is available ("norm")
//' @param prior_par A vector specifying the prior parameters
//' @param initial_theta The starting point of theta estimate for the
//'   Newton-Raphson algorighm.
//' @param tol The tolerance level. The difference between the two subsequent
//'   first derivatives of response log-likelihoods should be smaller than
//'   this number.
//' @noRd

// [[Rcpp::export]]
Rcpp::List est_ability_map_response_cpp(
    Rcpp::S4 resp,
    Rcpp::S4 ip,
    std::string prior_dist = "norm",
    Rcpp::NumericVector prior_par = Rcpp::NumericVector::create(0, 1),
	Rcpp::NumericVector theta_range = Rcpp::NumericVector::create(-5, 5),
    double initial_theta = 0,
    double tol = 0.00001)
{
  Rcpp::List output;

  if (prior_dist != "norm") {
    stop("Invalid prior distribution. MAP is only available for 'norm'.");
  }
  double prior_mean = prior_par[0];
  double prior_sd = prior_par[1];

  double theta_est = initial_theta;
  double diff = 999;
  double d1_ll;
  double d2_ll = 0;
  double d1_ll_pre = 999; // previous first derivative of log likelihood

  while (diff > tol) {
    // first derivative of log-likelihood of responses
    d1_ll = resp_loglik_response_cpp(theta_est, resp, ip, 1);
    // add the first derivative of prior to the first der. of log-likelihood
    d1_ll = d1_ll - (theta_est - prior_mean) / pow(prior_sd, 2);

    // second derivative of log-likelihood of responses
    d2_ll = resp_loglik_response_cpp(theta_est, resp, ip, 2);
    // add the second derivative of prior to the second der. of log-likelihood
    d2_ll = d2_ll - 1 / pow(prior_sd, 2);

    theta_est = theta_est - (d1_ll / d2_ll);
    diff = std::fabs(d1_ll - d1_ll_pre);

    // Rcout << "d1 = " << d1_ll << "  -  d2 = " << d2_ll <<
    //   " - theta_est = " << theta_est << "  -  diff = " << diff << std::endl;

    d1_ll_pre = d1_ll;
  }
  if (theta_est < theta_range[0]) theta_est = theta_range[0];
  if (theta_est > theta_range[1]) theta_est = theta_range[1];
  output["est"] = theta_est;
  // Make sure to add 'resp' below so that missing responses will not
  // contribute to the total information.
  output["se"] = 1/pow(info_response_tif_cpp(theta_est, ip, resp, false) +
    1 / pow(prior_sd, 2), 0.5);
  return output;
}

//#############################################################################@
//########################### est_ability_map_response_set_cpp #################
//#############################################################################@
//' @param resp_set A Response_set object.
//' @param ip An Itempool object.
//' @noRd

// [[Rcpp::export]]
Rcpp::List est_ability_map_response_set_cpp(
    Rcpp::S4 resp_set,
    Rcpp::S4 ip,
    std::string prior_dist = "norm",
    Rcpp::NumericVector prior_par = Rcpp::NumericVector::create(0, 1),
    Rcpp::NumericVector theta_range = Rcpp::NumericVector::create(-5, 5),
    double initial_theta = 0,
    double tol = 0.00001
	) {

  // Check whether resp_set is Response_set object. If it is a Response object
  // convert it to a Response_set object with one Response.
  if (resp_set.inherits("Response")) {
    Rcpp::S4 response_set("Response_set");
    Rcpp::List response_list(1);
    response_list[0] = resp_set;
    if (resp_set.slot("examinee_id") == R_NilValue) {
      response_list.attr("names") = "Ex-1";
    } else {
      response_list.attr("names") = as<Rcpp::StringVector>(
        resp_set.slot("examinee_id"));
    }
    response_set.slot("response_list") = response_list;
    resp_set = response_set;
  } else if (!resp_set.inherits("Response_set")) {
    stop("Invalid 'resp_set' argument. 'resp_set' should be a Response_set "
           "object.");
  }
  // Make sure resp_set and ip are valid and compatible
  check_validity_response_set_cpp(resp_set, ip);
  // Rcpp::List ip_list = flatten_itempool_cpp(ip);

  Rcpp::List resp_list = as<Rcpp::List>(resp_set.slot("response_list"));
  int num_of_resp = resp_list.size();

  Rcpp::NumericVector est(num_of_resp);
  Rcpp::NumericVector se(num_of_resp);
  Rcpp::S4 temp_resp;
  Rcpp::List output, temp_output;
  for (int i = 0; i < num_of_resp; i++) {
    temp_resp = as<Rcpp::S4>(resp_list[i]);
  	temp_output = est_ability_map_response_cpp(temp_resp, ip, prior_dist,
  	  prior_par, theta_range, initial_theta, tol);
  	est[i] = temp_output["est"];
    se[i] = temp_output["se"];
  }
  // Estimate standard error
  output["est"] = est;
  output["se"] = se;
  return output;
}

