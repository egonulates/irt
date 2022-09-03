#include <Rcpp.h>
#include "misc.h"
#include "itempool_class_methods.h"
#include "response_set_methods.h"
#include "prob.h"

using namespace Rcpp;

//##############################################################################
//########################### lz_response_cpp ##################################
//##############################################################################
// Using Eqn. 3, 4, 5, 6 of Sinharay (2015) Assessment of Person Fit of Mixed-
// format tests.

// [[Rcpp::export]]
double lz_response_cpp(Rcpp::S4 resp, double theta,
                       Rcpp::List ip_list) {

  Rcpp::NumericVector scores = as<Rcpp::NumericVector>(resp.slot("score"));
  Rcpp::StringVector item_ids = as<Rcpp::StringVector>(resp.slot("item_id"));

  int num_of_items = scores.size();

  double l0 = 0;
  double E = 0;  // Expected value of l0
  double V = 0;  // Variance of l0

  Rcpp::S4 item; // This will be item
  std::string item_id;
  Rcpp::NumericVector p; // will hold probability

  for (int k = 0; k < num_of_items; k++) {
    item_id = item_ids[k];
    item = as<Rcpp::S4>(ip_list[item_id]);
    p = prob_bare_item_cpp(Rcpp::NumericVector::create(theta), item, 0, -9, false);

    // Rcout << "p = " << p.size() << " - p: " << p << "  - item_id: " <<
    //  item_id << "  -  item_id: " << as<std::string>(item.slot("item_id")) <<  std::endl;

    // Rcout << "item_id: " << as<std::string>(item.slot("id")) <<
    //   "; a = " << as<double>(item.slot("a")) <<
    //   "; b = " << as<double>(item.slot("b")) <<
    //   "; c = " << as<double>(item.slot("c")) <<
    //   "; D = " << as<double>(item.slot("D")) <<
    //   "; p = " <<  prob_4pm_bare_cpp(theta, item, 0) <<
    //     std::endl;

    // Caculate l0

    if (check_item_model(item, false, true)) {
      l0 = l0 + log(p[scores[k]]);
      
      for (int j = 0; j < p.size(); j++) {
        E = E + p[j] * log(p[j]);
        // Calculate Variance of l0
        for (int h = 0; h < p.size(); h++) {
          V = V + p[j] * p[h] * log(p[j]) * log(p[j]/p[h]);
        }
      }
      // Rcout << item_id << " -- scores[k] = " << scores[k] << " -- p[scores[k]] = " 
      //   << p[scores[k]] << " -- log(p[scores[k]]) = " << log(p[scores[k]]) << std::endl;
      // Rcout << item_id << " -- l0 = " << l0 << "  E = " << E << "  V = " << V 
      //   << "  -  lz = " << (l0 - E) / sqrt(V) << std::endl << std::endl;
    } else if ( check_item_model(item, true, true)) {
      l0 = l0 + scores[k] * log(p[1]) + (1 - scores[k]) * log(p[0]);
      E =  E + p[1] * log(p[1]) + p[0] * log(p[0]);
      V = V + p[1] * p[0] * pow(log(p[1] / p[0]), 2);
      // Rcout << item_id << " -- l0 = " << l0 << "  E = " << E << "  V = " << V 
      //  << "  -  lz = " << (l0 - E) / sqrt(V) << std::endl << std::endl;
    } else {
      stop("Invalid item model. lz cannot be calculated for this item.");
    }
  }
  // Rcout << "l0 = " << l0 << "  E = " << E << "  V = " << V << std::endl;
  return (l0 - E) / sqrt(V); // calculate lz
}


//##############################################################################
//########################### lz_response_set_cpp ##############################
//##############################################################################

// [[Rcpp::export]]
Rcpp::NumericVector lz_response_set_cpp(
    Rcpp::S4 resp_set, Rcpp::NumericVector theta, Rcpp::S4 ip)
{
  // Make sure resp_set and ip are valid and compatible
  check_validity_response_set_cpp(resp_set, ip);
  Rcpp::List ip_list = flatten_itempool_cpp(ip);

  Rcpp::List resp_list = as<Rcpp::List>(resp_set.slot("response_list"));
  int num_of_resp = resp_list.size();

  if (theta.size() != num_of_resp)
    stop("Incompatible 'theta' and 'resp_set'. Their length should be equal.");

  Rcpp::NumericVector output(num_of_resp);
  for (int i = 0; i < num_of_resp; i++) {
    output[i] = lz_response_cpp(as<Rcpp::S4>(resp_list[i]), theta[i], ip_list);
  }
  output.attr("names") = get_examinee_id_response_set_cpp(resp_set);
  return output;
}
