#include <Rcpp.h>
#include "misc.h"
#include "itempool_class_methods.h"
#include "response_set_methods.h"
#include "prob.h"
using namespace Rcpp;

//#############################################################################@
//#############################################################################@
//########################### resp_lik #########################################
//#############################################################################@
//#############################################################################@


//#############################################################################@
//########################### resp_lik_bare_item_cpp ###########################
//#############################################################################@
// This function calculates the response likelihood of an item for a
// given response.

// [[Rcpp::export]]
double resp_lik_bare_item_cpp(double resp, double theta, Rcpp::S4 item) {
  // Deal with missing responses, return NA directly
  if (NumericVector::is_na(resp))
    return NA_REAL;

  // Get the Psychometric Model name
  std::string model = as<std::string>(item.attr("class"));

  if (model == "GPCM" || model == "GPCM2" || model == "PCM" || model == "GRM") {
    Rcpp::NumericVector P;
    if (model == "GPCM" || model == "PCM" || model == "GPCM2") {
      P = prob_gpcm_bare_cpp(theta, item, 0, resp);
      return P[0];
    } else if (model == "GRM") {
      P = prob_grm_bare_cpp(theta, item);
      return P[resp];
    }
    // The following line assumes that the resp goes from 0 to maximum number
    // of categories.
    return P[resp];
  } else if (check_item_model(item, true, true)) {
    return prob_4pm_bare_cpp(theta, item, 0, resp);
    // // The following line is important (instead of second line) because
    // // it accounts for resp values that are not 0 or 1.
    // return pow(P, resp) * pow(1.0-P, 1.0-resp);
    // return resp * P + (1 - resp) * (1 - P);
  }
  return NA_REAL;
}


//##############################################################################
//########################### resp_lik_item_cpp ################################
//##############################################################################
// [[Rcpp::export]]
Rcpp::NumericVector resp_lik_item_cpp(Rcpp::NumericVector resp,
                                      Rcpp::NumericVector theta, Rcpp::S4 item)
{
  // Calculate response likelihood for one item and multiple theta's (and
  // responses)
  unsigned int num_of_theta = theta.size();
  Rcpp::NumericVector output(num_of_theta);
  for(unsigned int i = 0; i < num_of_theta; i++)
    output[i] = resp_lik_bare_item_cpp(resp[i], theta[i], item);
  return output;
}


//##############################################################################
//########################### resp_lik_bare_testlet_cpp ########################
//##############################################################################
//' Find the response likelihood of a testlet
//' 
//' @param resp A numeric vector of responses. The length of this vector 
//'   should have the same length as the number of standalone items in the 
//'   Testlet object.
//' @param theta A single numeric value representing the ability of the examinee
//' @param testlet A Testlet object.
//' 
//' 
//' 
//' @noRd
//' 
//' 
// [[Rcpp::export]]
double resp_lik_bare_testlet_cpp(Rcpp::NumericVector resp, double theta,
                                 Rcpp::S4 testlet)
{
  // Calculate response log-likelihood for a testlet and single examinee
  Rcpp::List item_list = as<List>(testlet.slot("item_list"));
  unsigned int num_of_items = item_list.size();
  if (resp.size() != num_of_items) {
    stop("Invalid 'resp'. The size of the resp should be equal to the size of the number of items in the testlet.");
  }
  double output = 1;
  Rcpp::S4 item; // This will be item
  for(unsigned int i = 0; i < num_of_items; i++) {
    item = as<Rcpp::S4>(item_list(i));
    if (!Rcpp::NumericVector::is_na(resp[i]))
        output = output * resp_lik_bare_item_cpp(
          resp(i), theta, as<Rcpp::S4>(item_list(i)));
  }
  return output;
}


//##############################################################################
//########################### resp_lik_testlet_cpp #############################
//##############################################################################
// [[Rcpp::export]]
Rcpp::NumericVector resp_lik_testlet_cpp(Rcpp::NumericMatrix resp,
                                         Rcpp::NumericVector theta,
                                         Rcpp::S4 testlet)
{
  // Calculate response log-likelihood for an Itempool and multiple
  // theta's (and response strings)
  unsigned int num_of_theta = theta.size();
  NumericVector output(num_of_theta);
  for(unsigned int i = 0; i < num_of_theta; i++) {
    // Get the row belong to the examinee. It is assumed that each row represents
    // an examinee.
    NumericVector resp_vector = resp(i, _);
    output[i] = resp_lik_bare_testlet_cpp(resp_vector, theta[i], testlet);
  }
  return output;
}


//' Get full response vector from potentially partial testlet responses.
//' 
//' If the testlet involves five items: i1, i2, i3, i4, i5 and examinee
//' only responsed three of them: i2, i3 and i5 with responses c(0, 1, 0). 
//' The input will be "resp = c(0, 1, 1)"; "item_ids = c("i2". "i3". "i5")". 
//' This function will plug NA for the places of missing items and return 
//' the following response vector: c(NA, 0, 1, NA, 1)
//' 
//' 
//' @noRd
//' 
Rcpp::NumericVector get_testlet_full_resp_cpp(Rcpp::NumericVector resp, 
                                              Rcpp::StringVector item_ids,
                                              Rcpp::S4 testlet) {
  if (!testlet.inherits("Testlet")) {
    stop("Invalid testlet. testlet should be a Testlet object.");
  }
  int num_items_resp = resp.size() ;
  if (num_items_resp != item_ids.size()) {
    stop("Invalid item_ids. resp and item_ids should have the same length.");
  }
  Rcpp::S4 testlet_ip = as<Rcpp::S4>(testlet.slot("item_list"));
  Rcpp::List testlet_item_list = testlet_ip.slot("item_list");
  int num_of_all_testlet_items = testlet_item_list.size();
  Rcpp::NumericVector output(num_of_all_testlet_items, Rcpp::NumericVector::get_na());
  std::string temp_item_id;
  Rcpp::S4 temp_item("Item");
  for (int i = 0; i < num_items_resp; i++) {
    for (int j = 0; j < num_of_all_testlet_items; j++) {
      temp_item = as<S4>(testlet_item_list[j]);
      temp_item_id = as<std::string>(temp_item.slot("item_id"));
      if (item_ids[i] == temp_item_id) {
        output[j] = resp[i];
      }
    }
  }
  return output;
}

//#############################################################################@
//########################### resp_lik_bare_itempool_cpp ######################
//#############################################################################@

// [[Rcpp::export]]
double resp_lik_bare_itempool_cpp(Rcpp::NumericVector resp, double theta,
                                  Rcpp::S4 ip) {
  // This function calculates the response likelihood of an item pool for a
  // given response string and one theta.

  // Assuming that theta.size() ==  resp.size(), though it may not be the case
  // always. There might be an instance where more responses can happen if
  // item pool has testlets.
  int no_items = resp.size();
  double result = 1;
  Rcpp::S4 item;
  // Indicator variable for whether all responses are missing (true) or at
  // least there is one non-missing response (false).
  bool resp_all_na = true;
  // Rcpp::List item_list = ip.slot("item_list");
  Rcpp::List item_list = flatten_itempool_cpp(ip);
  for (int i = 0; i < no_items; i++) {
    // iterate over non-missing responses
    if (!R_IsNA(resp[i])) {
      resp_all_na = false; // one non-na observed
      item = as<Rcpp::S4>(item_list[i]);
      result = result * resp_lik_bare_item_cpp(resp[i], theta, item);
    }
  }
  if (resp_all_na) result = NA_REAL;   // should it return 0 or NA?
  return result;
}


//##############################################################################
//########################### resp_lik_itempool_cpp ############################
//##############################################################################
// [[Rcpp::export]]
Rcpp::NumericVector resp_lik_itempool_cpp(Rcpp::NumericMatrix resp,
                                          Rcpp::NumericVector theta,
                                          Rcpp::S4 ip)
{
  // Calculate response log-likelihood for an Itempool and multiple
  // theta's (and response strings)
  unsigned int num_of_theta = theta.size();
  NumericVector output(num_of_theta);
  for(unsigned int i = 0; i < num_of_theta; i++) {
    // Get the row belong to the examinee. It is assumed that each row represents
    // an examinee.
    NumericVector resp_vector = resp(i, _);
    output[i] = resp_lik_bare_itempool_cpp(resp_vector, theta[i], ip);
  }
  return output;
}


//' Calculate the likelihood of a Response object
//' 
//' @description It is assumed that the testlet items are administered together.
//'   In other words, it is assumed that there are no items administered 
//'   between any two items within the same testlet that does not belong to
//'   that testlet.
//' 
//' 
//' @param theta A single numeric value representing the ability of the examinee
//' @param resp A Response object.
//' @param ip An Itempool object.
//' 
//' @noRd
//' 
// [[Rcpp::export]]
double resp_lik_response_cpp(double theta, Rcpp::S4 resp, Rcpp::S4 ip)
{
  Rcpp::NumericVector scores = as<Rcpp::NumericVector>(resp.slot("score"));
  Rcpp::StringVector item_ids = as<Rcpp::StringVector>(resp.slot("item_id"));

  double output = 1;
  Rcpp::S4 item; // This will be an item or testlet
  std::string item_id;
  Rcpp::List ip_list = as<Rcpp::List>(ip.slot("item_list"));
  int num_of_items = scores.size();

  // check if testlet_id is NULL, TYPEOF(R_NilValue)
  if (TYPEOF(resp.slot("testlet_id")) == 0) {  // No Testlets
    for (int i = 0; i < num_of_items; i++) {
      item_id = item_ids[i];
      // item = as<Rcpp::S4>(ip_list[item_id]);
      output = output * resp_lik_bare_item_cpp(scores[i], theta, 
                                               as<Rcpp::S4>(ip_list[item_id]));
    }
  } else { // There are testlet items
    int item_no = 0; // this will track the item number of the scores
    int temp_counter;
    Rcpp::S4 testlet("Testlet");
    std::string testlet_id;
    // The vector holding testlet_ids column of resp.
    Rcpp::StringVector testlet_ids = as<Rcpp::StringVector>(
      resp.slot("testlet_id"));
    
    // Create a large enough empty vectors that can hold all temporary scores
    // and testlet standalone item ids    
    Rcpp::StringVector temp_testlet_item_ids(num_of_items, 
                                             Rcpp::StringVector::get_na());
    Rcpp::NumericVector temp_testlet_item_scores(num_of_items, 
                                                 Rcpp::NumericVector::get_na());
    while (item_no < num_of_items) {
      // Rcout << "item_no = " << item_no << std::endl;
      // check if the item belongs to a testlet
      if (Rcpp::StringVector::is_na(testlet_ids[item_no])) { // not a testlet item
        
        item_id = item_ids[item_no];
        output = output * resp_lik_bare_item_cpp(scores[item_no], theta, 
                                                 as<Rcpp::S4>(ip_list[item_id]));
        // Rcout << "    Not a testlet item: " << item_id << " - " << output << std::endl;                                         
        item_no++;                                         
      } else { // current item is a testlet item
        // get all of the item ids that were administered from this testlet and
        // scores
        testlet_id = testlet_ids[item_no];
        // Rcout << "    A testlet item: " << testlet_id << std::endl;  
        testlet = as<Rcpp::S4>(ip_list[testlet_id]);
        temp_counter = 0;
        while (testlet_ids[item_no] == testlet_id) {
          temp_testlet_item_ids[temp_counter] = item_ids[item_no];
          temp_testlet_item_scores[temp_counter] = scores[item_no];
          // Rcout << "        Testlet item: " << item_ids[item_no] << " - score = " << scores[item_no] << "  -  temp_counter = " << temp_counter << std::endl;   
          item_no++;
          temp_counter++;
        }
        output = output * resp_lik_bare_testlet_cpp(
          get_testlet_full_resp_cpp(
            temp_testlet_item_scores[Rcpp::Range(0, temp_counter-1)], 
            temp_testlet_item_ids[Rcpp::Range(0, temp_counter-1)], testlet), 
          theta, testlet);
        // Rcout << "    Testlet Item is DONE. output = " << output << std::endl;  
      }
    }
  }
  return output;
}


//#############################################################################@
//########################### resp_lik_response_set_cpp ########################
//#############################################################################@
// @param resp_set A Response_set object.

// [[Rcpp::export]]
Rcpp::NumericVector resp_lik_response_set_cpp(
    Rcpp::S4 resp_set,
    Rcpp::NumericVector theta,
    Rcpp::S4 ip) {

  // Make sure resp_set and ip are valid and compatible
  check_validity_response_set_cpp(resp_set, ip);
  // Rcpp::List ip_list = flatten_itempool_cpp(ip);

  Rcpp::List resp_list = as<Rcpp::List>(resp_set.slot("response_list"));
  int num_of_resp = resp_list.size();

  if (theta.size() != num_of_resp)
    stop("Incompatible 'theta' and 'resp_set'. Their length should be equal.");

  Rcpp::NumericVector output(num_of_resp);
  Rcpp::S4 temp_resp;
  for (int i = 0; i < num_of_resp; i++) {
    temp_resp = as<Rcpp::S4>(resp_list[i]);
    output[i] = resp_lik_response_cpp(theta[i], temp_resp, ip);
  }
  return output;
}






