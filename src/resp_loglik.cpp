#include <Rcpp.h>
#include "misc.h"
#include "itempool_class_methods.h"
#include "response_set_methods.h"
#include "prob.h"
#include "resp_lik.h"
using namespace Rcpp;


//#############################################################################@
//#############################################################################@
//########################### resp_loglik ######################################
//#############################################################################@
//#############################################################################@

//##############################################################################
//########################### resp_loglik_bare_item_cpp ########################
//##############################################################################
// [[Rcpp::export]]
double resp_loglik_bare_item_cpp(double resp, double theta, Rcpp::S4 item,
                                 int derivative = 0)
{
  // Calculate response log-likelihood for one item and one theta's (and
  // responses)

  // Deal with missing responses, return NA directly
  if (NumericVector::is_na(resp))
    return NA_REAL;

  if (derivative == 0) { // Response log-likelihood
      double resp_lik = resp_lik_bare_item_cpp(resp, theta, item);
      // Here is an assumption that resp_lik is not NA. It can be if the model
      // is not implemented yet in "resp_lik_bare_cpp" function.
      return log(resp_lik);
      // // Get the Psychometric Model name
      // std::string model = as<std::string>(item.slot("model"));
      // if (model == "GPCM" || model == "GRM") {
      //   Rcpp::NumericVector P(resp);
      //   if (model == "GPCM") {
      //     P = prob_gpcm_bare_cpp(theta, item);
      //   } else if (model == "GRM") {
      //     P = prob_grm_bare_cpp(theta, item);
      //   }
      //   return log(P[resp]);
      // } else if (model == "1PL" || model == "2PL" || model == "3PL" ||
      //   model == "4PL") {
      //   double P = prob_4pm_bare_cpp(theta, item);
      //   return resp * log(P) + (1 - resp) * log(1 - P);
      // }
      // // This should return a more sensible value if model is none of the
      // above.
      // return 0;
  } else if (derivative == 1) { // First derivative of response log-likelihood
      std::string model = as<std::string>(item.attr("class"));
      if (model == "GPCM" || model == "PCM" || model == "GPCM2") {
        // This function calculates the first derivative for one item and one
        // theta for Generalized Partial Credit Model.
        // Calculations use formula Penfield and Bergeron (2005), p. 220,
        // Eq. (2)

        // Item discrimination, if PCM, set them to 1, else if GPCM get them
        double a = 1;
        double D = 1;
        // Item difficulty
        Rcpp::NumericVector b;
        unsigned int no_choices;
        if (model == "GPCM2") {
          Rcpp::NumericVector d = as<Rcpp::NumericVector>(item.slot("d"));
          no_choices = d.size() + 1;
          double b_loc = as<double>(item.slot("b"));
          b = clone(d);
          for (unsigned int i = 0; i < no_choices; i++)
            b[i] = b_loc - d[i];
        } else { // "GPCM" or "PCM"
          b = as<Rcpp::NumericVector>(item.slot("b"));
          no_choices = b.size() + 1;
        }
        if (model == "GPCM" || model == "GPCM2") {
          a = as<double>(item.slot("a"));
          D = as<double>(item.slot("D"));
        }

        Rcpp::NumericVector P = prob_gpcm_bare_cpp(theta, item, 0);
        double lambda1 = 0;
        for(unsigned int i = 0; i < no_choices; i++) {
          lambda1 = lambda1 + i * P[i];
        }
        double result = 0;
        for(unsigned int i = 0; i < no_choices; i++) {
          // It is assumed that scoring function U_j and j are the same.
          result = result + i * D * a * (i - lambda1);
        }
        return result;
      } else if (check_item_model(item, true, true)) {
        double P = prob_4pm_bare_cpp(theta, item, 0);
        double dP =prob_4pm_bare_cpp(theta, item, 1);
        return  dP * (resp - P) / (P * (1 - P));
      } else {
        stop("This model has not been implemented yet.");
      }
      return NA_REAL;
  } else if (derivative == 2) { // Second derivative of response log-likelihood
      // Get the Psychometric Model name
      std::string model = as<std::string>(item.attr("class"));
      if (model == "GPCM" || model == "GPCM2" || model == "PCM") {
        // This function calculates the second derivative for one item and one
        // theta for Generalized Partial Credit Model.
        // Calculations use formula Penfield and Bergeron (2005), p. 220,
        // Eq. (3)
        // Alternative formula can be found in Donoghue (1994), p.309, second
        // equation from the last

        std::string model = as<std::string>(item.attr("class"));

        // Item discrimination, if PCM, set them to 1, else if GPCM get them
        double a = 1;
        double D = 1;
        // Item difficulty
        Rcpp::NumericVector b;
        unsigned int no_choices;
        if (model == "GPCM2") {
          Rcpp::NumericVector d = as<Rcpp::NumericVector>(item.slot("d"));
          no_choices = d.size() + 1;
          double b_loc = as<double>(item.slot("b"));
          b = clone(d);
          for (unsigned int i = 0; i < no_choices; i++)
            b[i] = b_loc - d[i];
        } else { // "GPCM" or "PCM"
          b = as<Rcpp::NumericVector>(item.slot("b"));
          no_choices = b.size() + 1;
        }
        if (model == "GPCM" || model == "GPCM2") {
          a = as<double>(item.slot("a"));
          D = as<double>(item.slot("D"));
        }

        Rcpp::NumericVector P = prob_gpcm_bare_cpp(theta, item, 0);
        double lambda1 = 0;
        double lambda2 = 0;
        for(unsigned int i = 0; i < no_choices; i++) {
          lambda1 = lambda1 + i * P[i];
          lambda2 = lambda2 + i*i * P[i];
        }
        return D*D * a*a * (lambda1*lambda1 - lambda2);
      } else if (check_item_model(item, true, true)) {
        // This function calculates the second derivative of the log
        // likelihood of a response string for one theta.
        double P = prob_4pm_bare_cpp(theta, item, 0);
        double dP = prob_4pm_bare_cpp(theta, item, 1);
        double d2P = prob_4pm_bare_cpp(theta, item, 2);
        return  (1 / (P * (1 - P))) * (d2P * (resp - P) - dP * dP *
          (1+(resp - P) * (1- 2*P) / (P * (1-P)) ) );
        // return  pow(P * (1 - P), -1) * (d2P * (resp - P) - pow(dP, 2) *
        //   (1+(resp - P) * (1- 2*P) / (P * (1-P)) ) );
      } else {
        stop("This model has not been implemented yet.");
      }
      return NA_REAL;
  } else
      stop("'derivative' value can take only values 0, 1 or 2.");
}



//##############################################################################
//########################### resp_loglik_item_cpp #############################
//##############################################################################
//' Calculate the response log-likelihood of a response string.
//' @param resp Response vector.
//' @param theta Theta value.
//' @param item An \code{Item-class} object.
//' @param derivative An integer indicating which derivative to calculate:
//'    0 = No derivative
//'    1 = First derivative
//'    2 = Second derivative
//'
//' @noRd
//'
// [[Rcpp::export]]
Rcpp::NumericVector resp_loglik_item_cpp(Rcpp::NumericVector resp,
                                         Rcpp::NumericVector theta,
                                         Rcpp::S4 item,
                                         int derivative = 0)
{
  // Calculate response log-likelihood for one item and multiple theta's (and
  // responses)
  unsigned int num_of_theta = theta.size();
  Rcpp::NumericVector output(num_of_theta);
  for(unsigned int i = 0; i < num_of_theta; i++)
    output[i] = resp_loglik_bare_item_cpp(resp[i], theta[i], item, derivative);
  return output;
}

// du: density of u
// [[Rcpp::export]]
double resp_loglik_btm_integral_cpp(double u, double mu, double sigma,
                                    Rcpp::NumericVector resp,
                                    double theta, Rcpp::List& item_list) {
  double output = 0;

  unsigned int num_of_items = item_list.size();
  for(unsigned int i = 0; i < num_of_items; i++) {
    if (!NumericVector::is_na(resp[i]))
      output = output + resp_loglik_bare_item_cpp(
            resp[i], theta + u, as<Rcpp::S4>(item_list(i)), 0);
  }
  return exp(output) *  R::dnorm(u, mu, sigma, false);
}


//##############################################################################
//########################### resp_loglik_bare_testlet_cpp #####################
//##############################################################################
//' Calculate response log-likelihood for a testlet and a single theta (and a
//' response string)
//' @param resp Response vector.
//' @param theta Theta value.
//' @param testlet A \code{Testlet-class} object.
//' @param derivative An integer indicating which derivative to calculate:
//'    0 = No derivative
//'    1 = First derivative
//'    2 = Second derivative
//'
//' @noRd
//'
// [[Rcpp::export]]
double resp_loglik_bare_testlet_cpp(Rcpp::NumericVector resp, double theta,
                                    Rcpp::S4& testlet, int derivative = 0)
{
  double output = 0;
  std::string model = as<std::string>(testlet.slot("model"));

  Rcpp::S4 temp_s4 = as<Rcpp::S4>(testlet.slot("item_list"));
  Rcpp::List item_list = as<Rcpp::List>(temp_s4.slot("item_list"));

  if (model == "BTM") {
    // Calculate response log-likelihood for a testlet and single examinee
    unsigned int num_of_items = item_list.size();
    for(unsigned int i = 0; i < num_of_items; i++) {
      if (!NumericVector::is_na(resp[i]))
        output = output + resp_loglik_bare_item_cpp(
              resp[i], theta, as<Rcpp::S4>(item_list(i)), derivative);
    }
  } else if (model == "RTM") {
    Rcpp::List par_list = as<Rcpp::List>(testlet.slot("parameters"));
    double mu = as<double>(par_list("mean"));
    double sigma = as<double>(par_list("sd"));

    // if sigma is very close to 0, then there is no testlet effect and
    // effectively ignore testlet effects and treat items as independent items.
    if (sigma < 0.001) {
      unsigned int num_of_items = item_list.size();
      for(unsigned int i = 0; i < num_of_items; i++) {
        if (!NumericVector::is_na(resp[i]))
          output = output + resp_loglik_bare_item_cpp(
                resp[i], theta, as<Rcpp::S4>(item_list(i)), derivative);
      }
    } else {
      Rcpp::NumericVector u;
      double start = -4.5;
      double end = 4.5;
      double step = 0.1;
      while (start < end) {
        start += step;
        u.push_back(start);
      }
      int n_quad = u.size();

      Rcpp::NumericVector du(n_quad);
      for (int i = 0; i < n_quad; i++) {
        du[i] = resp_loglik_btm_integral_cpp(u[i], mu, sigma,
                                             resp, theta, item_list);
      }
      output = log(integrate(u, du));
    }
  } else stop("This model has not been implemented yet.");
  return output;
}



//##############################################################################
//########################### resp_loglik_testlet_cpp ##########################
//##############################################################################
// [[Rcpp::export]]
Rcpp::NumericVector resp_loglik_testlet_cpp(Rcpp::NumericMatrix resp,
                                            Rcpp::NumericVector theta,
                                            Rcpp::S4 testlet,
                                            int derivative = 0)
{
  // Calculate response log-likelihood for an Itempool and multiple
  // theta's (and response strings)
  unsigned int num_of_theta = theta.size();
  NumericVector output(num_of_theta);
  for(unsigned int i = 0; i < num_of_theta; i++) {
    // Get the row belong to the examinee. It is assumed that each row
    // represents an examinee.
    NumericVector resp_vector = resp(i, _);
    output[i] = resp_loglik_bare_testlet_cpp(resp_vector, theta[i], testlet,
                                             derivative);
  }
  return output;
}



//##############################################################################
//########################### resp_loglik_bare_itempool_cpp ####################
//##############################################################################
// [[Rcpp::export]]
double resp_loglik_bare_itempool_cpp(Rcpp::NumericVector resp, double theta,
                                     Rcpp::S4 ip, int derivative = 0)
{
  // Calculate response log-likelihood for an Itempool and single examinee
  Rcpp::List item_list = as<List>(ip.slot("item_list"));
  unsigned int num_of_items = item_list.size();
  // The variable that will hold the number of items in a testlet:
  unsigned int testlet_size;
  double output = 0;
  Rcpp::S4 item; // This can be item or a testlet
  // an index that tracks the column number to read for the resp vector
  int resp_index = 0;
  for(unsigned int i = 0; i < num_of_items; i++) {
    item = as<Rcpp::S4>(item_list(i));
    if (item.inherits("Testlet")) {
      // Find the number of items within the testlet
      testlet_size = as<List>(as<S4>(item.slot(
        "item_list")).slot("item_list")).size();
      output = output + resp_loglik_bare_testlet_cpp(
        resp[Rcpp::Range(resp_index, resp_index + testlet_size - 1)], theta,
        item, derivative);
      resp_index += testlet_size;
    // } else if (item.inherits("Item")) {
    } else {
      // Check the class of the item, if it is "Item"
      if (!NumericVector::is_na(resp[resp_index]))
        output = output + resp_loglik_bare_item_cpp(resp(resp_index), theta,
                                                    item, derivative);
      resp_index += 1;
    }
  }
  return output;
}


//##############################################################################
//########################### resp_loglik_itempool_cpp #########################
//##############################################################################
// [[Rcpp::export]]
Rcpp::NumericVector resp_loglik_itempool_cpp(Rcpp::NumericMatrix resp,
                                             Rcpp::NumericVector theta,
                                             Rcpp::S4 ip, int derivative = 0)
{
  // Calculate response log-likelihood for an Itempool and multiple
  // theta's (and response strings)
  unsigned int num_of_theta = theta.size();
  NumericVector output(num_of_theta);
  for(unsigned int i = 0; i < num_of_theta; i++) {
    // Get the row belong to the examinee. It is assumed that each row
    // represents an examinee.
    NumericVector resp_vector = resp(i, _);
    output[i] = resp_loglik_bare_itempool_cpp(resp_vector, theta[i], ip,
                                               derivative);
  }
  return output;
}


// //##############################################################################
// //########################### resp_loglik_response_cpp #########################
// //##############################################################################
// // resp_loglik_response_cpp functions require 'Response' objects to have
// // valid item_id slot.
//
// double resp_loglik_response_cpp(double theta,
//                                 Rcpp::S4 resp,
//                                 Rcpp::List ip_list,
//                                 int derivative = 0)
// {
//   // // resp_loglik_response_cpp functions require 'Response' objects to have
//   // // valid item_id slot.
//   // if (resp.slot("item_id") == R_NilValue)
//   //   stop("Invalid 'resp'. 'resp' should have valid 'item_id' slot.");
//
//   Rcpp::NumericVector scores = as<Rcpp::NumericVector>(resp.slot("score"));
//   Rcpp::StringVector item_ids = as<Rcpp::StringVector>(resp.slot("item_id"));
//
//   int num_of_items = scores.size();
//
//   double output = 0;
//   Rcpp::S4 item; // This will be item
//   std::string item_id;
//   for (int i = 0; i < num_of_items; i++) {
//     item_id = item_ids[i];
//     item = as<Rcpp::S4>(ip_list[item_id]);
//     output = output + resp_loglik_bare_item_cpp(scores[i], theta, item,
//                                                 derivative);
//   }
//   return output;
// }


//##############################################################################
//########################### resp_loglik_response_cpp #########################
//##############################################################################
// resp_loglik_response_cpp functions require 'Response' objects to have
// valid item_id slot.
// [[Rcpp::export]]
double resp_loglik_response_cpp(double theta,
                                Rcpp::S4& resp,
                                Rcpp::S4& ip,
                                int derivative = 0)
{
  Rcpp::NumericVector scores = as<Rcpp::NumericVector>(resp.slot("score"));
  Rcpp::StringVector item_ids = as<Rcpp::StringVector>(resp.slot("item_id"));

  // Rcout << std::endl << "- resp_loglik_response_cpp -" << theta << std::endl;
  double output = 0;
  Rcpp::S4 item; // This will be an item or testlet
  std::string item_id;
  Rcpp::List ip_list = as<Rcpp::List>(ip.slot("item_list"));
  int num_of_items = scores.size();

  // Rcout << TYPEOF(resp.slot("testlet_id") << std::endl;

  // check if testlet_id is NULL, TYPEOF(R_NilValue)
  if (TYPEOF(resp.slot("testlet_id")) == 0) {
    // Rcout << " -- resp_loglik_response_cpp - no testlets -- " << std::endl;
    for (int i = 0; i < num_of_items; i++) {
      item_id = item_ids[i];
      item = as<Rcpp::S4>(ip_list[item_id]);
      output = output + resp_loglik_bare_item_cpp(scores[i], theta, item,
                                                  derivative);
    }
  } else {
    // Rcout << " -- resp_loglik_response_cpp - testlet -- " << std::endl;
    int item_no = 0;
    std::string testlet_id;
    // The vector holding testlet_ids column of resp.
    Rcpp::StringVector testlet_ids = as<Rcpp::StringVector>(
      resp.slot("testlet_id"));
    Rcpp::StringVector calculated_teslelet_ids;
    Rcpp::NumericVector selected_testlet_item_scores;
    Rcpp::S4 temp_s4;

    // holds whether a testlet administered before or not
    bool administered = false;
    while (item_no < num_of_items) { // Standalone item
      item_id = item_ids[item_no];
      if (Rcpp::StringVector::is_na(testlet_ids[item_no])) {
        item = as<Rcpp::S4>(ip_list[item_id]);
        output = output + resp_loglik_bare_item_cpp(scores[item_no], theta,
                                                    item, derivative);
        // Rcout << item_no << " -- item -- " << item_id << " -- " << output <<
        //   std::endl;
      } else { // Testlet item
        testlet_id = testlet_ids[item_no];
        // Check if the likelihood of this testlet has already been calculated
        administered = false;
        for (int j = 0; j < calculated_teslelet_ids.size(); j++) {
          if (testlet_id == as<std::string>(calculated_teslelet_ids[j]))
            administered = true;
        }
        // Find all of the testlet items.
        if (!administered) { // Calculate the likelihood of all items in the
                             // testlet
          // Get testlet object:
          item = as<Rcpp::S4>(ip_list[testlet_id]);
          // item pool of the testlet
          temp_s4 = as<Rcpp::S4>(item.slot("item_list"));
          Rcpp::NumericVector selected_testlet_item_scores(
              get_itempool_size(temp_s4)[0], Rcpp::NumericVector::get_na());

          selected_testlet_item_scores.attr("names") = get_ids_itempool_cpp(
            temp_s4);
          // Get the scores the testlet items in the same order that appears
          // in item pool. If there is an unadministered item, let it be NA
          // testlet_scores
          for (int j = item_no; j < num_of_items; j++) {
            if (testlet_ids[j] ==  testlet_id) { // if item belongs to this
                                                     // testlet..
              item_id = item_ids[j];
              selected_testlet_item_scores[item_id] = scores[j];
            }
          }
          // Calculate likelihood of the testlet
          output = output + resp_loglik_bare_testlet_cpp(
            selected_testlet_item_scores, theta, item, derivative);
          // Rcout << item_no << " -- testlet -- " << testlet_id << " -- "<<
          //   output << std::endl;
          calculated_teslelet_ids.push_back(testlet_id);
        } // else -> likelihood of testlet item has already been calculated
      }
      item_no++;
    }
  }
  return output;
}



//##############################################################################
//########################### resp_loglik_response_set_cpp #####################
//##############################################################################

// [[Rcpp::export]]
Rcpp::NumericVector resp_loglik_response_set_cpp(
    Rcpp::S4 resp_set,
    Rcpp::NumericVector theta,
    Rcpp::S4 ip,
    int derivative = 0)
{
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
    output[i] = resp_loglik_response_cpp(theta[i], temp_resp, ip,
                                         derivative);
  }
  return output;
}

