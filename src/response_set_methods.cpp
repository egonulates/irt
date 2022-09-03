#include <Rcpp.h>
#include "itempool_class_methods.h"
using namespace Rcpp;



//#############################################################################@
//########################### check_validity_response_set_cpp ##################
//#############################################################################@
// Check validity of Response_set objects.
// [[Rcpp::export]]
bool check_validity_response_set_cpp(Rcpp::S4 resp_set, Rcpp::S4 ip) {

  if (!resp_set.inherits("Response_set"))
    stop("Invalid 'resp_set'. 'resp_set' should be a Response_set object.");
  if (!ip.inherits("Itempool"))
    stop("Invalid 'ip'. 'ip' should be an Itempool object.");

  Rcpp::List response_list = resp_set.slot("response_list");
  // a list holding individual items
  Rcpp::List ip_list = flatten_itempool_cpp(ip);
  int num_of_items = ip_list.size(); // item pool size
  int num_of_resp = response_list.size(); // number of responses

  // Get item IDs
  Rcpp::S4 item;
  Rcpp::StringVector ip_item_ids(num_of_items);
  for (int i = 0; i < num_of_items; i++) {
    item = as<S4>(ip_list[i]);
    ip_item_ids[i] = as<std::string>(item.slot("item_id"));
  }

  Rcpp::S4 response;
  Rcpp::NumericVector scores;
  Rcpp::StringVector resp_item_ids;
  for (int i = 0; i < num_of_resp; i++) {
    response = as<S4>(response_list[i]);
    if (response.slot("score")==R_NilValue)
      stop("Invalid 'score'. 'score' cannot be NULL.");
    scores = as<Rcpp::NumericVector>(response.slot("score"));

    // if (!Rf_isNumeric(response.slot("score")))
    //   stop("Invalid 'score'. 'score' vector should be numeric.")
    if (any(is_na(scores)))
      stop("Invalid 'score'. 'score' vector should not contain NA.");
    // Rcpp::NumericVector scores = as<Rcpp::NumericVector>(response_)
    if (response.slot("item_id")==R_NilValue)
      stop("Invalid 'item_id'. 'item_id' cannot be NULL.");
    resp_item_ids = as<Rcpp::StringVector>(response.slot("item_id"));

    // The size of scores and ip_item_ids should be the same
    if (scores.size() != resp_item_ids.size()) {
      stop("Incompatible 'item_id' and 'score'. The size of 'item_id' and "
             "'score' should be the same.");
    }

    for (int j = 0; j < resp_item_ids.size(); j++) {
      if (std::find(ip_item_ids.begin(), ip_item_ids.end(),
                    resp_item_ids[j]) == ip_item_ids.end()) {
        stop("Invalid 'ip'. All of the items in the response data should be in the item pool, ip.");
      }
    }
  }

  // Make sure there is a score slot and none of the values are NA
  return true;
}


//#############################################################################@
//########################### get_examinee_id_response_set_cpp #################
//#############################################################################@
// Get examinee_id slot of a Response_set object. It will return a NULL or a
// Rcpp::StringVector object. If a Response's examinee_id field is NULL, then
// that element will be NA. If all examinee_id fields are NULL, then a NULL
// object will be returned.

// [[Rcpp::export]]
Rcpp::Nullable<Rcpp::StringVector> get_examinee_id_response_set_cpp(
    Rcpp::S4 resp_set)
{
  if (!resp_set.inherits("Response_set"))
    stop("Invalid 'resp_set' argument. 'resp_set' should be a Response_set "
           "object. ");
  Rcpp::List resp_list = as<Rcpp::List>(resp_set.slot("response_list"));
  int num_of_resp = resp_list.size();
  Rcpp::StringVector examinee_ids(num_of_resp);
  bool set_examinee_ids = false;
  SEXP temp_id;
  for (int i = 0; i < num_of_resp; i++) {
    temp_id = as<Rcpp::S4>(resp_list[i]).slot("examinee_id");
    if (temp_id != R_NilValue) {
      if (Rf_isInteger(temp_id)) {
        examinee_ids[i] = std::to_string(as<int>(temp_id));
      } else if (Rf_isString(temp_id)) {
        examinee_ids[i] = as<std::string>(temp_id);
      } else {
        examinee_ids[i] = R_NaString;
      }
      set_examinee_ids = true;
    } else {
      examinee_ids[i] = R_NaString;
    }
  }
  if (set_examinee_ids) {
    return examinee_ids;
  } else return R_NilValue;
}


//#############################################################################@
//########################### max_score_response_cpp ###########################
//#############################################################################@
// Finds the maximum possible score of a Response object
double max_score_response_cpp(Rcpp::S4 resp, Rcpp::List ip_list) {
  Rcpp::NumericVector scores = as<Rcpp::NumericVector>(resp.slot("score"));
  Rcpp::StringVector item_ids = as<Rcpp::StringVector>(resp.slot("item_id"));
  int num_of_items = scores.size();

  double output = 0;
  std::string item_id;
  for (int i = 0; i < num_of_items; i++) {
    item_id = item_ids[i];
    output = output + get_max_possible_score_item_cpp(
      as<Rcpp::S4>(ip_list[item_id]));
  }
  return output;
}


//#############################################################################@
//########################### max_score_response_set_cpp #######################
//#############################################################################@
// Finds the maximum possible scores of all Responses of a ResponseSet object
// [[Rcpp::export]]
Rcpp::NumericVector max_score_response_set_cpp(Rcpp::S4 resp_set, Rcpp::S4 ip)
{
  // Make sure resp_set and ip are valid and compatible
  check_validity_response_set_cpp(resp_set, ip);
  Rcpp::List ip_list = flatten_itempool_cpp(ip);

  Rcpp::List resp_list = as<Rcpp::List>(resp_set.slot("response_list"));
  int num_of_resp = resp_list.size();

  Rcpp::NumericVector output(num_of_resp);
  Rcpp::S4 temp_resp;
  for (int i = 0; i < num_of_resp; i++) {
    temp_resp = as<Rcpp::S4>(resp_list[i]);
    output[i] = max_score_response_cpp(temp_resp, ip_list);
  }
  output.attr("names") = get_examinee_id_response_set_cpp(resp_set);
  return output;
}
