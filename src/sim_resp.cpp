#include <Rcpp.h>
#include "itempool_class_methods.h"
#include "misc.h"
#include "prob.h"
using namespace Rcpp;


//##############################################################################
//########################### sim_resp_4pm_bare_cpp ############################
//##############################################################################

// [[Rcpp::export]]
int sim_resp_4pm_bare_cpp(double theta, Rcpp::S4 item) {
  double p = prob_4pm_bare_cpp(theta, item);
  if (p > as<double>(runif(1, 0, 1))) {
    return 1;
  } else return 0;
}


//##############################################################################
//########################### sim_resp_poly_bare_cpp ###########################
//##############################################################################

// [[Rcpp::export]]
int sim_resp_poly_bare_cpp(double theta, Rcpp::S4 item) {
  // This method is based on De Ayala (1994) The Influence of
  // Multidimensionality on the, p. 158-159: "The generation of an examinee’s
  // polytomous response string was accomplished by calculating the
  // probability of responding to each item alternative according to the
  // MGRM; the scaling factor D was set to 1.0. Based on the probability for
  // each alternative, cumulative probabilities were obtained for each
  // alternative. A random error component was incorporated into each
  // response by selecting a random number from a uniform distribution [0, 1]
  // and comparing it to the cumulative probabilities. The ordinal position
  // of the first cumulative probability that was greater than the random
  // number was taken as the examinee’s response to the item."
  double u = as<double>(runif(1, 0, 1));
  // Cumulative Probability
  Rcpp::NumericVector cp = cumsum(prob_poly_bare_cpp(theta, item));
  int cp_size = cp.size();
  for (int i=cp_size-2; i > -1; i--)
    // Rcout << i << " - u = " << u << "  , cp = " << cp[i] << "\n";
    if (u > cp[i]) return(i+1);
  return 0;
}


//##############################################################################
//########################### sim_resp_bare_cpp ################################
//##############################################################################

// [[Rcpp::export]]
int sim_resp_bare_cpp(double theta, Rcpp::S4 item) {
  if (check_item_model(item, false, true)) {
    return sim_resp_poly_bare_cpp(theta, item);
    // The default value is 4pm.
  } else if (check_item_model(item, true, true)) {
    return sim_resp_4pm_bare_cpp(theta, item);
  } else stop("Invalid item. This model has not been implemented yet.");
}


// This function sets a raw response for dichotomous items
Rcpp::StringVector set_raw_response(Rcpp::S4 item, int score) {

   // check if item is dichotomous item and misc slot is not empty
   if (check_item_model(item, true, true) &&
       !Rf_isNull(item.slot("misc"))) {
     Rcpp::List misc = as<Rcpp::List>(item.slot("misc"));
     // if there is no "key", no raw response will be created.
     if (misc.containsElementNamed("key")) {
       Rcpp::StringVector key = as<Rcpp::StringVector>(misc["key"]);
       if (score > 0) return(key);
       // Check if 'item@misc$possible_options' exists, if yes, extract it
       Rcpp::StringVector possible_options = {"A", "B", "C", "D"};
       if (misc.containsElementNamed("possible_options")) {
         possible_options = as<Rcpp::StringVector>(misc["possible_options"]);
       }
       possible_options = Rcpp::setdiff(possible_options, key);
       return Rcpp::sample(possible_options, 1);
     }
   }
   return {NA_STRING};
}

//##############################################################################
//########################### sim_resp_response_cpp ############################
//##############################################################################
//' @title Create a Response class object for a single examinee for an item pool
//'
//' @param theta A value representing theta
//' @param ip An item pool objec.
//' @param examinee_id A string representing examinee ID.
//' @param ip_size the size of the item pool.
//' @param prop_missing proportion of missing
//'
//' NOTE: if the prop_missing value is close to 1, unexpected behavior can be
//' observed.
//'
//' @noRd
//'
//' @examples
//' ip <- c(generate_testlet(item_id_preamble = "t1"),
//'         generate_ip(n = 5, model = c("2PL", "3PL", "GPCM", "PCM", "GRM")),
//'         generate_testlet(item_id_preamble = "t2"))
//' irt:::sim_resp_response_cpp(theta = 1, ip = ip, examinee_id = "abc",
//'                             prop_missing = 0.5)


// [[Rcpp::export]]
Rcpp::S4 sim_resp_response_cpp(double theta,
                               Rcpp::S4 ip,
                               Rcpp::StringVector examinee_id = "",
                               Rcpp::NumericVector ip_size = NA_INTEGER,
                               double prop_missing = 0) {
  Rcpp::S4 response("Response");

  if (NumericVector::is_na(ip_size[0])) {
    ip_size = get_itempool_size(ip);
  }
  int num_of_elements = ip_size[0];
  int num_of_testlets = ip_size[1];
  int num_of_items = ip_size[2];
  bool raw_resp_exists = false; // holds whether there will be any raw
                                // responses within items
  if ((prop_missing < 0) || (prop_missing >= 1) ||
      // make sure prop_missing is not unreasonable high
      ((num_of_items * (1-prop_missing)) < 1)) {
    prop_missing = 0;
    Rcpp::warning("'prop_missing' is inadmissable or too low. It's value "
                    "will be set to 0.");
  }
  Rcpp::List item_list = as<List>(ip.slot("item_list"));
  Rcpp::IntegerVector scores(num_of_items);
  Rcpp::StringVector item_ids(num_of_items);
  Rcpp::StringVector testlet_ids(num_of_items);
  Rcpp::StringVector raw_responses(num_of_items, NA_STRING);
  Rcpp::IntegerVector item_order(num_of_items);

  Rcpp::S4 element;
  Rcpp::S4 item;
  Rcpp::StringVector temp_str;
  Rcpp::List testlet_item_list;
  int testlet_size;
  int io = 0; //io: keeps tally of individual items within testlet or standalone
  do { // make sure there is at least one response created
    for (int i = 0; i < num_of_elements; i++) {
      element = as<Rcpp::S4>(item_list(i));
      // Rcout << "Element " << i << std::endl;
      if (element.inherits("Testlet")) {
        // Find the number of items within the testlet
        testlet_item_list = as<List>(as<S4>(
          element.slot("item_list")).slot("item_list"));
        testlet_size = testlet_item_list.size();
        for (int j = 0; j < testlet_size; j++) {
          // Check whether this item is missing
          if (as<double>(runif(1, 0, 1)) >= prop_missing) {
            item = as<S4>(testlet_item_list[j]);
            scores[io] = sim_resp_bare_cpp(theta, item);
            item_ids[io] = as<std::string>(item.slot("item_id"));
            testlet_ids[io] = as<std::string>(element.slot("testlet_id"));
            temp_str = set_raw_response(item, scores[io]);
            raw_resp_exists = raw_resp_exists ||
              (!Rcpp::StringVector::is_na(raw_responses[io]));
            item_order[io] = io + 1;
            io++;
          }
        }
      // } else if (element.inherits("Item")) {
      } else {
        // Check whether this item is missing
        if (as<double>(runif(1, 0, 1)) >= prop_missing) {
          scores[io] = sim_resp_bare_cpp(theta, element);
          item_ids[io] = as<std::string>(element.slot("item_id"));
          testlet_ids[io] = NA_STRING;
          raw_responses[io] = set_raw_response(element, scores[io])[0];
          raw_resp_exists = raw_resp_exists ||
            (!Rcpp::StringVector::is_na(raw_responses[io]));
          item_order[io] = io + 1;
          io++;
        }
      }
    }
  } while (io == 0);

  if ((examinee_id.size() == 1)  &&  (examinee_id[0] != "")) {
    response.slot("examinee_id") = examinee_id ;
  }

  response.slot("item_id") = item_ids[Rcpp::Range(0, io - 1)];
  if (num_of_testlets > 0)
    response.slot("testlet_id") = testlet_ids[Rcpp::Range(0, io - 1)];

  if (raw_resp_exists)
    response.slot("raw_response") = raw_responses[Rcpp::Range(0, io - 1)];

  response.slot("score") = scores[Rcpp::Range(0, io - 1)];
  response.slot("order") = item_order[Rcpp::Range(0, io - 1)];
  return(response);
}


//##############################################################################
//########################### sim_resp_response_set_cpp ########################
//##############################################################################
//' @title Create a Response_set class object for multiple examinees for an item
//'   pool
//'
//' @param theta A vector representing thetas
//' @param ip An item pool objec.
//' @param examinee_id A vector representing examinee_id's
//' @param prop_missing proportion of missing
//'
//' NOTE: if the prop_missing value is close to 1, unexpected behavior can be
//' observed.
//'
//' @noRd
//'
//' @examples
//' ip <- c(generate_testlet(item_id_preamble = "t1"),
//'         generate_ip(n = 5, model = c("2PL", "3PL", "GPCM", "PCM", "GRM")),
//'         generate_testlet(item_id_preamble = "t2"))
//' irt:::sim_resp_response_set_cpp(theta = rnorm(3), ip = ip,
//'                                 prop_missing = 0.5)

// [[Rcpp::export]]
Rcpp::S4 sim_resp_response_set_cpp(Rcpp::NumericVector theta,
                                   Rcpp::S4 ip,
                                   Rcpp::StringVector examinee_id = "",
                                   double prop_missing = 0) {
  Rcpp::S4 response_set("Response_set");
  Rcpp::S4 response("Response");
  int n_theta = theta.size();
  Rcpp::List response_list(n_theta);  
  Rcpp::NumericVector ip_size;
  ip_size = get_itempool_size(ip);
  Rcpp::StringVector unique_item_ids, response_item_ids, item_ids, testlet_ids;
  Rcpp::StringVector ip_item_ids = get_item_ids_itempool_cpp(ip);
  Rcpp::StringVector ip_testlet_ids = get_testlet_ids_itempool_cpp(ip);
  bool add_testlet_id_slot = false;
  // Raise warning if prop_missing is too high.
  if ((prop_missing < 0) || (prop_missing >= 1) ||
      // make sure prop_missing is not unreasonable high
      ((ip_size[2] * (1-prop_missing)) < 1)) {
    prop_missing = 0;
    Rcpp::warning("'prop_missing' is inadmissable or too low. It's value "
                    "will be set to 0.");
  }

  // Set examinee ids
  if (((examinee_id.size() == 1)  &&  (examinee_id[0] != "")) ||
      examinee_id.size() != n_theta) {
    examinee_id = Rcpp::StringVector(n_theta);
    for (int i = 0; i < n_theta; i++) {
      examinee_id[i] = "S" + std::to_string(i+1);
    }
  }

  for (int i = 0; i < n_theta; i++) {
    response = sim_resp_response_cpp(
      theta[i], ip, as<StringVector>(examinee_id[i]), ip_size, prop_missing);
    response_list[i] = response;
    // Create @item_id slot. Stop when unique item id's size is equal to the
    // size of the item pool. 
    if (unique_item_ids.size() < ip_size[2]) {
      response_item_ids = as<Rcpp::StringVector>(response.slot("item_id"));
      Rcpp::StringVector temp_unique_item_ids = no_init(response_item_ids.size() + 
                                                        unique_item_ids.size());
      std::merge(unique_item_ids.begin(), unique_item_ids.end(), 
                 response_item_ids.begin(), response_item_ids.end(), 
                 temp_unique_item_ids.begin()) ;
      unique_item_ids = Rcpp::unique(temp_unique_item_ids);
    }
  }
  // if the following is true, then some items in the item pool is not 
  // present in responses.
  if (ip_item_ids.size() == unique_item_ids.size()) {
    item_ids = ip_item_ids;
    testlet_ids = ip_testlet_ids;
  } else {
    for (int i = 0; i < ip_item_ids.size(); i++) {
      if (std::find(unique_item_ids.begin(), unique_item_ids.end(), 
                    ip_item_ids[i]) != unique_item_ids.end()) {
        // unique_item_ids[counter] = ip_item_ids[i];
        item_ids.push_back(ip_item_ids[i]);
        if (Rcpp::traits::is_na<STRSXP>(ip_testlet_ids[i])) {
            testlet_ids.push_back(NA_STRING);
        } else { 
          testlet_ids.push_back(ip_testlet_ids[i]);
          add_testlet_id_slot = true;
        }        
      }
    }
  } 
  response_list.attr("names") = examinee_id;

  response_set.slot("response_list") = response_list;
  response_set.slot("item_id") = item_ids;
  if (add_testlet_id_slot) response_set.slot("testlet_id") = testlet_ids;
  
  return(response_set);
}
