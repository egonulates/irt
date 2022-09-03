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
//########################### est_ability_eap_cpp ##############################
//#############################################################################@
//#############################################################################@

//#############################################################################@
//########################### est_ability_eap_single_examinee_decprecated_cpp ##
//#############################################################################@
//' This function will be used in CAT simulations
//' @noRd

Rcpp::List est_ability_eap_single_examinee_decprecated_cpp(
    Rcpp::NumericVector resp,
    Rcpp::S4 ip,
	Rcpp::NumericVector theta_range = Rcpp::NumericVector::create(-5, 5),
    int no_of_quadrature = 41,
    std::string prior_dist = "norm",
    Rcpp::NumericVector prior_par = Rcpp::NumericVector::create(0, 1))
{
  // Rcout << "  (est_ability_eap_single_examinee_cpp) -- 1 -- Beginning" <<
  // std::endl;

  Rcpp::List item_list = as<List>(ip.slot("item_list"));
  unsigned int noItem = item_list.size();
  double L; // This will hold the likelihood of a response string
  Rcpp::NumericVector x(no_of_quadrature);
  Rcpp::NumericVector fx_numerator(no_of_quadrature);
  Rcpp::NumericVector fx_denominator(no_of_quadrature);
  Rcpp::NumericVector fx_std_error(no_of_quadrature);
  double prior; // The value of the prior distribution
  // this will hold numerator of the posterior mean of theta
  double posterior_numerator;
  double posterior_denominator; // this will hold denominator of the posterior
  double est;
  double se;
  S4 temp_item("Item");
  Rcpp::List output;

  // Calculate the theta point vector
  for (int i = 0; i < no_of_quadrature; i++)
    x[i] = theta_range[0] + i * (theta_range[1] -
      theta_range[0]) / (no_of_quadrature - 1);
  for (int i = 0; i < no_of_quadrature; i++)
  {
    // Calculate the likelihood
    L = 1;
    for (unsigned int j = 0; j < noItem; j++)
    {
      // Only use non-missing, i.e. non-NA items.
      if (!NumericVector::is_na(resp[j])) {
        L = L * resp_lik_bare_item_cpp(resp[j], x[i],
                                       as<Rcpp::S4>(item_list[j]));
      }
    }
    // Rcout << "      (est_ability_eap_single_examinee_cpp) -- 3.1.3" <<
    // std::endl;
    // Calculate the prior distribution value
    if (prior_dist == "norm") {
      // mean = prior_par[0], sd = prior_par[1]
      prior = R::dnorm(x[i], prior_par[0], prior_par[1], false);
    } else if (prior_dist == "unif") {
      // min = prior_par[0], max = prior_par[1], log = false
      prior = R::dunif(x[i], prior_par[0], prior_par[1], false);
    } else if (prior_dist == "lnorm") {
      // meanlog = prior_par[0], sdlog = prior_par[1], log = false
      prior = R::dlnorm(x[i], prior_par[0], prior_par[1], false);
    } else if (prior_dist == "gamma") {
      // shape = prior_par[0], scale = prior_par[1], log = false
      prior = R::dgamma(x[i], prior_par[0], prior_par[1], false);
    } else if (prior_dist == "t") {
      // df = prior_par[0], log = false
      prior = R::dt(x[i], prior_par[0], false);
    } else if (prior_dist == "cauchy") {
      // location = prior_par[0], scale = prior_par[1], log = false
      prior = R::dcauchy(x[i], prior_par[0], prior_par[1], false);
    } else {
      // Todo: Double check this default value
      prior = 1;
    }
    fx_numerator[i] = L * prior * x[i];
    fx_denominator[i] = L * prior;
  }

  posterior_numerator = integrate(x, fx_numerator);
  posterior_denominator = integrate(x, fx_denominator);
  est = posterior_numerator/posterior_denominator;
  for (int i = 0; i < no_of_quadrature; i++)
    fx_std_error[i] = (x[i] - est) * (x[i] - est) * fx_denominator[i];
  se = sqrt(integrate(x, fx_std_error) / posterior_denominator);
  // Rprintf("r: %d; est: %.3f\n", r, est[r]);
  // Estimate standard error
  output["est"] = est;
  output["se"] = se;
  return output;
}

//#############################################################################@
//########################### est_ability_eap_single_examinee_cpp ##############
//#############################################################################@
// [[Rcpp::export]]
Rcpp::List est_ability_eap_single_examinee_cpp(
    Rcpp::NumericVector resp,
    Rcpp::S4 ip,
    Rcpp::NumericVector theta_range = Rcpp::NumericVector::create(-5, 5),
    int no_of_quadrature = 61,
    std::string prior_dist = "norm",
    Rcpp::NumericVector prior_par = NumericVector::create(0, 1))
{
  double temp;
  Rcpp::NumericVector fx_denominator(no_of_quadrature);
  double L; // This will hold the likelihood of a response string
  double prior; // The value of the prior distribution
  double numerator = 0;
  double denominator = 0;
  double est;
  double se = 0;
  Rcpp::List output;
  
  // Rcpp::List item_list = as<List>(ip.slot("item_list"));
  Rcpp::List item_list = flatten_itempool_cpp(ip);	
  unsigned int noItem = item_list.size();

  // Get Gauss-Hermite quadrature nodes and weights
  Rcpp::List gh = gauss_hermite(no_of_quadrature);
  Rcpp::NumericVector x = gh["nodes"];
  Rcpp::NumericVector gh_weights = gh["weights"];
  for (int i = 0; i < no_of_quadrature; i++) {

    // Calculate the likelihood
    L = 1;
    for (unsigned int j = 0; j < noItem; j++)
    {
      // Only use non-missing, i.e. non-NA items.
      if (!Rcpp::NumericVector::is_na(resp[j])) {
        L = L * resp_lik_bare_item_cpp(resp[j], x[i],
                                       as<Rcpp::S4>(item_list[j]));
      }
    }

    if (prior_dist == "norm") {
      // mean = prior_par[0], sd = prior_par[1]
      prior = R::dnorm(x[i], prior_par[0], prior_par[1], false);
    } else if (prior_dist == "unif") {
      // min = prior_par[0], max = prior_par[1], log = false
      prior = R::dunif(x[i], prior_par[0], prior_par[1], false);
    } else if (prior_dist == "lnorm") {
      // meanlog = prior_par[0], sdlog = prior_par[1], log = false
      prior = R::dlnorm(x[i], prior_par[0], prior_par[1], false);
    } else if (prior_dist == "gamma") {
      // shape = prior_par[0], scale = prior_par[1], log = false
      prior = R::dgamma(x[i], prior_par[0], prior_par[1], false);
    } else if (prior_dist == "t") {
      // df = prior_par[0], log = false
      prior = R::dt(x[i], prior_par[0], false);
    } else if (prior_dist == "cauchy") {
      // location = prior_par[0], scale = prior_par[1], log = false
      prior = R::dcauchy(x[i], prior_par[0], prior_par[1], false);
    } else {
      // Todo: Double check this default value
      prior = 1;
    }
    fx_denominator[i] = L * prior;
    //Rcout << "(est_ability_eap_single_examinee_cpp) -- " << L << " -- " << prior << std::endl;
    temp = gh_weights[i] * fx_denominator[i] * std::exp(pow(x[i], 2));
    numerator +=  temp * x[i];
    denominator +=  temp;
  }
  est = numerator / denominator;
  if (est < theta_range[0]) {
    est = theta_range[0];
  } else if (est > theta_range[1]) {
    est = theta_range[1];
  }

  // Calculate standard error
  for (int i = 0; i < no_of_quadrature; i++)
    se += gh_weights[i] * pow(x[i] - est, 2) * fx_denominator[i] *
      std::exp(pow(x[i], 2));
  output["est"] = est;
  output["se"] = sqrt(se / denominator);
  return output;
}


//#############################################################################@
//########################### est_ability_eap_cpp ##############################
//#############################################################################@
// Estimate ability for a group of subjects, in 'resp' rows represents
// examinees, and columns represent items.
// [[Rcpp::export]]
Rcpp::List est_ability_eap_cpp(
  Rcpp::NumericMatrix resp,
  Rcpp::S4 ip,
  Rcpp::NumericVector theta_range = Rcpp::NumericVector::create(-5, 5),
  int no_of_quadrature = 41,
  std::string prior_dist = "norm",
  Rcpp::NumericVector prior_par = Rcpp::NumericVector::create(0, 1))
{
  int no_of_examinees = resp.nrow();
  Rcpp::NumericVector est(no_of_examinees, NA_REAL);
  Rcpp::NumericVector se(no_of_examinees, NA_REAL);
  Rcpp::List output, temp_output;
  Rcpp::NumericVector temp_resp;

  for (int r = 0; r < no_of_examinees; r++) {
    temp_resp = resp(r, _);
    // when all responses are NA, return NA as se and est
    if (!all(is_na(temp_resp)).is_true()) {
      temp_output = est_ability_eap_single_examinee_cpp(
        temp_resp, ip, theta_range, no_of_quadrature, prior_dist, prior_par);
      est[r] = temp_output["est"];
      se[r] = temp_output["se"];
    }
  }
  // Estimate standard error
  output["est"] = est;
  output["se"] = se;
  return output;
}


//#############################################################################@
//########################### est_ability_eap_response_cpp #####################
//#############################################################################@
//' @param resp A Response object.
//' @param ip An Itempool object.
//' @noRd

// [[Rcpp::export]]
Rcpp::List est_ability_eap_response_cpp(
    Rcpp::S4 resp,
    Rcpp::S4 ip,
    Rcpp::NumericVector theta_range = Rcpp::NumericVector::create(-5, 5),
    int no_of_quadrature = 61,
    std::string prior_dist = "norm",
    Rcpp::NumericVector prior_par = NumericVector::create(0, 1))
{
  double temp;
  Rcpp::NumericVector fx_denominator(no_of_quadrature);
  double prior; // The value of the prior distribution
  double numerator = 0;
  double denominator = 0;
  double est;
  double se = 0;
  Rcpp::List output;

  // Get Gauss-Hermite quadrature nodes and weights
  Rcpp::List gh = gauss_hermite(no_of_quadrature);
  Rcpp::NumericVector x = gh["nodes"];
  Rcpp::NumericVector gh_weights = gh["weights"];
  for (int i = 0; i < no_of_quadrature; i++) {
    if (prior_dist == "norm") {
      // mean = prior_par[0], sd = prior_par[1]
      prior = R::dnorm(x[i], prior_par[0], prior_par[1], false);
    } else if (prior_dist == "unif") {
      // min = prior_par[0], max = prior_par[1], log = false
      prior = R::dunif(x[i], prior_par[0], prior_par[1], false);
    } else if (prior_dist == "lnorm") {
      // meanlog = prior_par[0], sdlog = prior_par[1], log = false
      prior = R::dlnorm(x[i], prior_par[0], prior_par[1], false);
    } else if (prior_dist == "gamma") {
      // shape = prior_par[0], scale = prior_par[1], log = false
      prior = R::dgamma(x[i], prior_par[0], prior_par[1], false);
    } else if (prior_dist == "t") {
      // df = prior_par[0], log = false
      prior = R::dt(x[i], prior_par[0], false);
    } else if (prior_dist == "cauchy") {
      // location = prior_par[0], scale = prior_par[1], log = false
      prior = R::dcauchy(x[i], prior_par[0], prior_par[1], false);
    } else {
      // Todo: Double check this default value
      prior = 1;
    }
    temp = resp_lik_response_cpp(x[i], resp, ip);
    fx_denominator[i] = resp_lik_response_cpp(x[i], resp, ip) * prior;
    //Rcout << "(est_ability_eap_response_cpp) -- " << temp << " -- " << prior << std::endl;
    
    temp = gh_weights[i] * fx_denominator[i] * std::exp(pow(x[i], 2));
    numerator +=  temp * x[i];
    denominator +=  temp;
  }
  est = numerator / denominator;
  if (est < theta_range[0]) {
    est = theta_range[0];
  } else if (est > theta_range[1]) {
    est = theta_range[1];
  }

  // Calculate standard error
  for (int i = 0; i < no_of_quadrature; i++)
    se += gh_weights[i] * pow(x[i] - est, 2) * fx_denominator[i] *
      std::exp(pow(x[i], 2));
  output["est"] = est;
  output["se"] = sqrt(se / denominator);
  return output;
}



//#############################################################################@
//########################### est_ability_eap_response_cpp_deprecated ##########
//#############################################################################@
//' This function calculates the EAP ability estimate.
//'
//' 2022-06-05: This function has been deprecated. The new function uses
//' Gausss-Hermite instead for the rectangle method of integration.
//'
//' @param resp A Response object.
//' @param ip An Itempool object.
//' @noRd

Rcpp::List est_ability_eap_response_cpp_deprecated(
    Rcpp::S4 resp,
    Rcpp::S4 ip,
    Rcpp::NumericVector theta_range = Rcpp::NumericVector::create(-5, 5),
    int no_of_quadrature = 61,
    std::string prior_dist = "norm",
    Rcpp::NumericVector prior_par = NumericVector::create(0, 1))
{
  double L; // This will hold the likelihood of a response string
  Rcpp::NumericVector x(no_of_quadrature);
  Rcpp::NumericVector fx_numerator(no_of_quadrature);
  Rcpp::NumericVector fx_denominator(no_of_quadrature);
  Rcpp::NumericVector fx_std_error(no_of_quadrature);
  double prior; // The value of the prior distribution
  // this will hold numerator of the posterior mean of theta
  double posterior_numerator;
  double posterior_denominator; // this will hold denominator of the posterior
  double est;
  double se;
  Rcpp::S4 temp_item("Item");
  Rcpp::List output;

  // Calculate the theta point vector
  for (int i = 0; i < no_of_quadrature; i++)
    x[i] = theta_range[0] + i * (theta_range[1] -
      theta_range[0]) / (no_of_quadrature - 1);

  for (int i = 0; i < no_of_quadrature; i++) {
    // Calculate the likelihood
    L = resp_lik_response_cpp(x[i], resp, ip);
    // Calculate the prior distribution value
    if (prior_dist == "norm") {
      // mean = prior_par[0], sd = prior_par[1]
      prior = R::dnorm(x[i], prior_par[0], prior_par[1], false);
    } else if (prior_dist == "unif") {
      // min = prior_par[0], max = prior_par[1], log = false
      prior = R::dunif(x[i], prior_par[0], prior_par[1], false);
    } else if (prior_dist == "lnorm") {
      // meanlog = prior_par[0], sdlog = prior_par[1], log = false
      prior = R::dlnorm(x[i], prior_par[0], prior_par[1], false);
    } else if (prior_dist == "gamma") {
      // shape = prior_par[0], scale = prior_par[1], log = false
      prior = R::dgamma(x[i], prior_par[0], prior_par[1], false);
    } else if (prior_dist == "t") {
      // df = prior_par[0], log = false
      prior = R::dt(x[i], prior_par[0], false);
    } else if (prior_dist == "cauchy") {
      // location = prior_par[0], scale = prior_par[1], log = false
      prior = R::dcauchy(x[i], prior_par[0], prior_par[1], false);
    } else {
      // Todo: Double check this default value
      prior = 1;
    }
    fx_numerator[i] = L * prior * x[i];
    fx_denominator[i] = L * prior;
  }

  posterior_numerator = integrate(x, fx_numerator);
  posterior_denominator = integrate(x, fx_denominator);
  est = posterior_numerator/posterior_denominator;
  // Estimate standard error
  for (int i = 0; i < no_of_quadrature; i++)
    fx_std_error[i] = (x[i] - est) * (x[i] - est) * fx_denominator[i];
  se = sqrt(integrate(x, fx_std_error) / posterior_denominator);
  output["est"] = est;
  output["se"] = se;
  return output;
}


//#############################################################################@
//########################### est_ability_eap_response_set_cpp #################
//#############################################################################@
//' @param resp_set A Response_set object.
//' @param ip An Itempool object.
//' @noRd

// [[Rcpp::export]]
Rcpp::List est_ability_eap_response_set_cpp(
    Rcpp::S4 resp_set,
    Rcpp::S4 ip,
    Rcpp::NumericVector theta_range = Rcpp::NumericVector::create(-5, 5),
    int no_of_quadrature = 61,
    std::string prior_dist = "norm",
    Rcpp::NumericVector prior_par = NumericVector::create(0, 1)) {

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
    temp_output = est_ability_eap_response_cpp(
      temp_resp, ip, theta_range, no_of_quadrature, prior_dist, prior_par);
    est[i] = temp_output["est"];
    se[i] = temp_output["se"];
  }
  // Estimate standard error
  output["est"] = est;
  output["se"] = se;
  return output;
}

