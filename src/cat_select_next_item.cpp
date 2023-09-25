#include <Rcpp.h>
#include <numeric>
#include "itempool_class_methods.h"
#include "info.h"
#include "resp_lik.h"
#include "resp_loglik.h"
#include "est_ability_eap.h"
#include "est_ability_map.h"
#include "est_ability_ml.h"
#include "est_ability_owen.h"
//#include <Rcpp/Benchmark/Timer.h>
using namespace Rcpp;


// Forward Declarations
Rcpp::List return_select_next_item_output(Rcpp::List cd, Rcpp::List est_history,
                                          Rcpp::List remaining_ip_list,
                                          Rcpp::List additional_args);
Rcpp::List process_testlet_cat_cpp(Rcpp::List cd, Rcpp::List est_history,
                                   Rcpp::List additional_args);
bool terminate_testlet_cat_cpp(Rcpp::S4 testlet,
                               Rcpp::List cd,
                               Rcpp::List est_history,
                               Rcpp::List additional_args);


class CompareDecr
{
public:
    CompareDecr (const Rcpp::NumericVector &v) : m_v (v) {}
    bool operator () (int i, int j) const { return m_v [i] > m_v [j]; }

private:
    const Rcpp::NumericVector &m_v;
};

class CompareIncr
{
public:
    CompareIncr (const Rcpp::NumericVector &v) : m_v (v) {}
    bool operator () (int i, int j) const { return m_v [i] < m_v [j]; }

private:
    const Rcpp::NumericVector &m_v;
};


Rcpp::IntegerVector order_decreasing(Rcpp::NumericVector &x) {
    Rcpp::IntegerVector idx(x.size());
    std::iota(idx.begin(), idx.end(), 0);
    CompareDecr c (x);
    std::sort(idx.begin(), idx.end(), c);
    return idx;
}

Rcpp::IntegerVector order_increasing(Rcpp::NumericVector &x) {
    Rcpp::IntegerVector idx(x.size());
    std::iota(idx.begin(), idx.end(), 0);
    CompareIncr c (x);
    std::sort(idx.begin(), idx.end(), c);
    return idx;
}



std::string get_s4_id(Rcpp::S4 element) {
  if (element.inherits("Testlet")) { // element is Testlet
    return as<std::string>(element.slot("testlet_id"));
  } else {
    return as<std::string>(element.slot("item_id"));
  }
}


//#############################################################################@
//########################### get_remaining_items_cpp ##########################
//#############################################################################@
//' Extract the remaining items in the item pool.
//'
//' @description This function returns an Itempool object of the remaining items
//' in the item pool after removing all of the items that has been administered.
//' It receives, as an input, CatDesign object which involves an Itempool
//' object (ip) and the estimate history (est_history) and returns an
//' Itempool object of the remaining objects. "est_history" is a list of
//' estimation history and the last element assumed to not have an $item,
//' i.e. the last element's 'item' field is null.
//'
//' If an item from a testlet has been administered, this function will not
//' return that testlet. Consequently, even the non-administered items within
//' the testlet are not included in the output.
//' 
//' @param cd  A \code{cat_design} object that holds the test specifications
//'   of the CAT.
//' @param est_history is a \code{List} that holds each step of the adaptive
//'   test. The first element is "1" which represents the beginning of the test.
//'   The elements are:
//'   \describe{
//'     \item{\code{"est_before"}}{The estimated ability before the item's
//'       administration.}
//'     \item{\code{"se_before"}}{The estimated standard error before the
//'       item's administration.}
//'     \item{\code{"item"}}{The item object that will be administered.}
//'     \item{\code{"testlet"}}{The testlet object that the administered item
//'       belongs to.}
//'     \item{\code{"resp"}}{The response value of the item that is
//'       administered}
//'     \item{\code{"est_after"}}{The estimated ability after the item's
//'       administration.}
//'     \item{\code{"se_after"}}{The estimated standard error after the
//'       item's administration}
//'   }
//' @param additional_args Additional arguments that are passed to functions.
//'   For example, it has a list called "set_aside_item_list". This list will
//'   contain items or testlets that has not been administered during the test
//'   but set aside and cannot be administered in this  particular
//'   administration of the CAT test.
//'
//' @noRd
//'
// [[Rcpp::export]]
Rcpp::S4 get_remaining_items_cpp(Rcpp::List cd, Rcpp::List est_history,
                             Rcpp::List additional_args) {
  // Rcout << "get_remaining_items_cpp 1  " << std::endl;
  // Get the item pool
  Rcpp::S4 ip = cd("ip");
  Rcpp::List aa = clone(additional_args);
  Rcpp::List ip_list = ip.slot("item_list");  // List of items in the item pool
  int est_history_size = est_history.size()-1;  // the last item is null.
  Rcpp::IntegerVector ip_size = get_itempool_size(ip);
  int no_of_elements = ip_size["elements"]; // Number of independent items or
                                            // testlets in the whole item pool
  Rcpp::S4 current_item("Item");
  Rcpp::S4 eh_item("Item"); // The "item" in the est_history["item"]
  Rcpp::S4 eh_testlet("Testlet"); // The "Testlet" in the est_history["testlet"]
  // This can be testlet or independent item of the main ip.
  Rcpp::S4 current_element, temp_s4;
  Rcpp::S4 current_testlet("Testlet");
  Rcpp::S4 temp_testlet("Testlet");
  Rcpp::List est_history_step, temp_est_history_step, temp_list;
  // this list will hold all of the testlets and items
  Rcpp::List remaining_ip_list;
  // This will hold the item/testlet ids of the remaining elements
  Rcpp::StringVector item_ids;
  // The items in "set_aside_item_list" will not be administered.
  List set_aside_item_list;
  if (!aa.containsElementNamed("set_aside_item_list")) {
    set_aside_item_list = List::create();
  } else
    set_aside_item_list = aa["set_aside_item_list"];
  // Number of items/testlets in set_aside_item_list
  int no_of_set_aside_item_list = set_aside_item_list.size();

  bool element_administered; // indicates that element has been administered
  // Rcout << "-- HERE 1 --" << std::endl;
  // Iterate through all items and get the ones that are unadministered or
  // if it is testlet has at least one unadministered item in it.
  for (int i = 0; i < no_of_elements; i++) {
    // Check whether item is in estimate history
    // bool item_in_testlet; // An indicator variable indicating whether the
    //                       // current item is in testlet or not.
    element_administered = false;
    current_element = as<S4>(ip_list[i]); // This can be Testlet or independent item
    // TODO FIX THIS: currently working only for item_ids
    std::string current_element_id = get_s4_id(current_element);

    // Check whether current_element is in set_aside_item_list
    if (no_of_set_aside_item_list > 0) {
      std::string temp_id;
      for (int j = 0; j < no_of_set_aside_item_list; j++) {
        temp_s4 = as<S4>(set_aside_item_list[j]);
        temp_id = get_s4_id(temp_s4);
        if (temp_id == current_element_id) {
          // Item should not be administered and cannot be added to remaining items list
          element_administered = true;
          break;
        }
      }
    }
    // if true, it means that item is in set_aside_item_list, so cannot be added to remaining items list
    if (element_administered) continue;

    // Rcout << "  get_remaining_items_cpp 2.1 - current_element ID = "
    //  << current_element_id << std::endl;
    if (current_element.inherits("Testlet")) { // current element is Testlet
      // Check whether this testlet is in "testlet" elements of estimate
      // history. If it is then check whether all elements of it is administered.
      // If all item elements of the current testlet is administered, then
      // it will not be added to remaining_ip, if at least one item of the
      // testlet has not been administered, then the current testlet will be
      // added to the remaining_ip.
      // Rcout << "  get_remaining_items_cpp 2.1.1  - current element is testlet" << std::endl;

      // iterate through the steps of est_history
      for (int step = 0; step < est_history_size; step++) { 
        est_history_step = est_history[step];
        if (Rf_isNull(est_history_step["testlet"])) continue;
        eh_testlet = as<S4>(est_history_step["testlet"]);
        // if true, this means item has been administered
        if (as<std::string>(eh_testlet.slot("testlet_id")) == current_element_id) {
          // Rcout << "    get_remaining_items_cpp 2.1.1.1  - current testlet is administered!" << std::endl;
          element_administered = true;
          break;
        }
      }

      // for (int step = 0; step < est_history_size; step++) { // iterate through the steps of est_history
      //   // Rcout << "    get_remaining_items_cpp 2.1.1.1  - step = " << step << std::endl;
      //   est_history_step = est_history[step];
      //   // check whether the testlet of this estimate history step is NULL,
      //   // if yes, go to the next step
      //   if (Rf_isNull(est_history_step["testlet"])) {
      //     // Rcout << "      get_remaining_items_cpp 2.1.1.1.1  - testlet of this step is NULL" << std::endl;
      //     continue;
      //   } else {
      //     // if it is not NULL check whether it the same as current_element
      //     // if it is not same, then move to the next estimate history step.
      //
      //     eh_testlet = as<S4>(est_history_step["testlet"]);
      //     // Rcout << "      get_remaining_items_cpp 2.1.1.1.2  - testlet of " <<
      //     // "this step is NOT NULL. eh_testlet_id = "
      //     // << as<std::string>(eh_testlet.slot("testlet_id")) << " ; current_element_id = "
      //     // << as<std::string>(current_element.slot("item_id")) << std::endl;
      //     if (as<std::string>(eh_testlet.slot("testlet_id")) == current_element_id) {
      //       // This means that at least one item from this testlet has been
      //       // administered. Now, check whether all of the items in this
      //       // testlet has been administered or not. If all of them
      //       // administered, then set "element_administered" to "true" and
      //       // break the loop of step and move to the next "i". If not
      //       // of the items has been administered, then, add this testlet
      //       // to the remaining_ip.
      //
      //       // Count how many times does this "testlet" occurred in
      //       // "est_history["testlet"]". If the number of occurrences is equal
      //       // to the number of items in the testlet, then it means this
      //       // testlet has been administered. Starting at "step", because
      //       // until this step this testlet has not been seen.
      //       int count_testlet_items = 0;
      //       // Rcout << "        get_remaining_items_cpp 2.1.1.1.2.1  - "
      //       // << "current_element is equal to eh_testlet;  count_testlet_items = "
      //       // << count_testlet_items << std::endl;
      //       for (int j = step; j < est_history_size; j++) {
      //         // Rcout << "          get_remaining_items_cpp 2.1.1.1.2.1.1  - sub-step = " << j << std::endl;
      //         temp_est_history_step = est_history[j];
      //         if (!Rf_isNull(temp_est_history_step["testlet"])) {
      //           temp_testlet = as<S4>(temp_est_history_step["testlet"]);
      //           if (as<std::string>(temp_testlet.slot("testlet_id")) == current_element_id)
      //             count_testlet_items++;
      //         }
      //       }
      //       // Get the number of items of the testlet
      //       temp_list = current_element.slot("item_list");
      //       // Rcout << "      get_remaining_items_cpp 2.1.1.1.4 -- temp_list size =  "
      //       // << temp_list.size() << "  ; count_testlet_items = " << count_testlet_items << std::endl;
      //       if (count_testlet_items == temp_list.size()) {
      //         // it means that all of the items in the testlet has been administered
      //         // Rcout << "      get_remaining_items_cpp 2.1.1.1.4.1 element has been administered " << std::endl;
      //         element_administered = true;
      //       }
      //       break;
      //     } else continue; // step's testlet is not equal to current_testlet, move to the next step.
      //   }
      // }
    } else { // current element is 'Item' object
    // } else if (current_element.inherits("Item")) { // current element is 'Item' object
      // Check the "item" elements of "est_history" steps. If the item has
      // not been administered (i.e. not in est_history), then add it to the
      // remaining_ip.
      // Rcout << "  get_remaining_items_cpp 2.2.1  - current element is item" << std::endl;
      for (int step = 0; step < est_history_size; step++) { // iterate through the steps of est_history
        est_history_step = est_history[step];
        eh_item = as<S4>(est_history_step["item"]);
        // if true, this means item has been administered
        if (as<std::string>(eh_item.slot("item_id")) == current_element_id) {
          // Rcout << "    get_remaining_items_cpp 2.2.1.1  - current element is administered!" << std::endl;
          element_administered = true;
          break;
        }
      }
    }
    if (!element_administered) { // if element has not been administered, add it to the remaining_ip_list
      // item_ids.push_back(as<std::string>(current_element.slot("item_id")));
      item_ids.push_back(current_element_id);
      remaining_ip_list.push_back(current_element);
    }
  }
  // Create an Itempool from the remaining_ip_list
  Rcpp::S4 ip_new("Itempool");
  remaining_ip_list.attr("names") = item_ids;
  ip_new.slot("item_list") = remaining_ip_list;
  return ip_new;



  // CharacterVector est_history_id(est_history_size); // item ID's in the estimate history
  // CharacterVector ip_id(ip_size); // item ID's in item pool
  // bool item_in_est_history; // true if item is in estimate history.
  // List remaining_ip(ip_size - est_history_size); // list that holds remaining items
  // // Temporary variables
  // S4 temp_s4;
  // std::string temp_id;
  // List temp_list;
  // Rcout << "    get_remaining_items_cpp 2  " << std::endl;
  // // Get the ID's of items in the estimation history
  // for (int i = 0; i < est_history_size; i++)  {
  //   temp_list = est_history[i];
  //   temp_s4 = as<S4>(temp_list("item"));
  //   est_history_id[i] = Rcpp::as<std::string>(temp_s4.slot("item_id"));
  //   //// Rcout << ip_id[i] << "\\n";
  // }
  // Rcout << "    get_remaining_items_cpp 3  " << std::endl;
  // for (int i = 0; i < ip_size; i++)  {
  //   temp_s4 = as<S4>(ip_list[i]);
  //   ip_id[i] = Rcpp::as<std::string>(temp_s4.slot("item_id"));
  // }
  // Rcout << "    get_remaining_items_cpp 4  " << std::endl;
  // int remaining_counter = 0;
  // for (int i = 0; i < ip_size; i++)  {
  //   Rcout << "      get_remaining_items_cpp 4.1  " << std::endl;
  //   temp_s4 = as<S4>(ip_list[i]);
  //   temp_id = Rcpp::as<std::string>(temp_s4.slot("item_id"));
  //   item_in_est_history = false;
  //   for (int j = 0; j < est_history_size; j++) {
  //     if (temp_id == as<std::string>(est_history_id(j))) {
  //       item_in_est_history = true;
  //       break;
  //     }
  //   }
  //   Rcout << "      get_remaining_items_cpp 4.3  " << std::endl;
  //   if (!item_in_est_history) {
  //     Rcout << temp_id << std::endl;
  //     remaining_ip[remaining_counter] = temp_s4;
  //     remaining_counter++;
  //   }
  // }
  //
  // int tempint = remaining_ip.size();
  // Rcout << "    get_remaining_items_cpp 5 - tempint = " << tempint << std::endl;
  // CharacterVector temptemp;
  // for (int i=0; i < tempint; i++) {
  //   temp_s4 = as<S4>(remaining_ip[i]);
  //   temptemp = temp_s4.slot("item_id");
  //   Rcout << "      get_remaining_items_cpp 5.2  - temptemp = " << temptemp << std::endl;
  // }
  //
  // Rcout << "    get_remaining_items_cpp 6  " << std::endl;
  // S4 ip_new("Itempool");
  // ip_new.slot("item_list") = remaining_ip;
  // return ip_new;
}

//#############################################################################@
//########################### get_administered_items_cpp #######################
//#############################################################################@
//' Get administered items from a CAT output
//'
//' @description This function returns an item pool object of the
//'   administered items using the items in estimate history.
//'
//'   NOTE: This function either returns a regular Itempool object and if there
//'         are no administered items, it returns an empty Itempool object.
//'         Consequently, it may not be a valid Itempool object. Use this
//'         function internally because it may cause errors in R.
//'
//' @param est_history is a \code{List} that holds each step of the adaptive
//'   test. The first element is "1" which represents the beginning of the test.
//'   The elements are:
//'   \describe{
//'     \item{\code{"est_before"}}{The estimated ability before the item's
//'       administration.}
//'     \item{\code{"se_before"}}{The estimated standard error before the
//'       item's administration.}
//'     \item{\code{"item"}}{The item object that will be administered.}
//'     \item{\code{"testlet"}}{The testlet object that the administered item
//'       belongs to.}
//'     \item{\code{"resp"}}{The response value of the item that is
//'       administered}
//'     \item{\code{"est_after"}}{The estimated ability after the item's
//'       administration.}
//'     \item{\code{"se_after"}}{The estimated standard error after the
//'       item's administration}
//'   }
//'
//' @noRd
//'
// [[Rcpp::export]]
Rcpp::S4 get_administered_items_cpp(Rcpp::List est_history) {
   // Rcout << "            (get_administered_items_cpp) -- 1 -- Beginning " << std::endl;
  Rcpp::List eh = clone(est_history);
  int item_no = eh.size();  // The stage of the test.
  // Check whether the last item is already selected
  Rcpp::List est_history_last_step = eh[item_no - 1];
   // Rcout << "            (get_administered_items_cpp) -- 2 -- item_no is " << item_no << std::endl;
  // If there is an element named "item" then check whether it is an "item"
  // or "testlet". If it is either of those keep item_no unchanged. Otherwise
  // reduce item_no by one.
  // One weak point of this function is, it assumes that if "item" is S4
  // object, it is either an "item" or "testlet".
  if (!est_history_last_step.containsElementNamed("item") ||
        TYPEOF(est_history_last_step["item"]) != S4SXP )// the element is not an S4
  {
     // Rcout << "              (get_administered_items_cpp) -- 2.2 -- Last element has 'item' TYPEOF:" <<
     // TYPEOF(est_history_last_step["item"]) << std::endl;
    item_no = item_no - 1;
  } // else item_no = item_no - 1;
   // Rcout << "            (get_administered_items_cpp) -- 3 --  item_no is " << item_no << std::endl;
  // Create an item pool of administered items
  Rcpp::List administered_ip_list(item_no);
  Rcpp::S4 temp_item;
  Rcpp::S4 administered_ip("Itempool");
  Rcpp::StringVector item_ids(item_no);
  if (item_no > 0) {
     // Rcout << "              (get_administered_items_cpp) -- 3.1 -- item_no > 0 " << std::endl;
    // S4 administered_ip("Itempool");
     // Rcout << "              (get_administered_items_cpp) -- 3.2 -- " << std::endl;
    for (int i=0; i < item_no; i++) {
       // Rcout << "                (get_administered_items_cpp) -- 3.2.1 -- iteration " << i << std::endl;
      // Get the item and add it to administered items list.
      est_history_last_step = eh[i];
      // Rcout << "                (get_administered_items_cpp) -- 3.2.2 -- iteration " << i << std::endl;
      temp_item = as<S4>(est_history_last_step["item"]);
      // Rcout << "                (get_administered_items_cpp) -- 3.2.3 -- iteration " << i << std::endl;
      // TODO: The list element should be a named list element
      administered_ip_list[i] = temp_item;
      item_ids(i) = as<std::string>(temp_item.slot("item_id"));  // TODO: This may not handle Testlets. Fix this!!!
      // est_history_last_step = eh[i];
      // administered_ip_list[i] = est_history_last_step["item"];
    }
    // Rcout << "                (get_administered_items_cpp) -- 3.3 --" << std::endl;
    administered_ip_list.attr("names") = item_ids;
    administered_ip.slot("item_list") = administered_ip_list;
    // Rcout << "                (get_administered_items_cpp) -- 3.4 --" << std::endl;
    return administered_ip;
  }
    //stop("There are no administered items in 'eh'!");

   // Rcout << "            (get_administered_items_cpp) -- 4 -- Finishing" << std::endl;

  // if administered item returns an 'Item' object it means there is no item
  // administered yet. This is a signifier for methods that use this function
  // to not use administered_ip.
  // S4 administered_ip("item");
  // administered_ip.slot("item_list") = Rf_isNull;
  return administered_ip;
}

//#############################################################################@
//########################### loglik_est_history_cpp ###########################
//#############################################################################@
//' Calculate the likelihood or log-likelihood of the estimate history.
//'
//' @description This function calculates the likelihood or log-likelihood of
//'   the estimate history
//'   for CAT. est_history can be complete, i.e. the "resp" value and the "item"
//'   value of the last element might be valid or not. If any of them is not
//'   valid, the last element of est_history will be ignored and likelihood
//'   will be calculated using the remaining elements.
//' @param est_history is a \code{List} that holds each step of the adaptive
//'   test. The first element is "1" which represents the beginning of the test.
//'   The elements are:
//'   \describe{
//'     \item{\code{"est_before"}}{The estimated ability before the item's
//'       administration.}
//'     \item{\code{"se_before"}}{The estimated standard error before the
//'       item's administration.}
//'     \item{\code{"item"}}{The item object that will be administered.}
//'     \item{\code{"testlet"}}{The testlet object that the administered item
//'       belongs to.}
//'     \item{\code{"resp"}}{The response value of the item that is
//'       administered}
//'     \item{\code{"est_after"}}{The estimated ability after the item's
//'       administration.}
//'     \item{\code{"se_after"}}{The estimated standard error after the
//'       item's administration}
//'   }
//' @param theta The theta estimate where the likelihood or log-likelihood
//'   needs to be calculated.
//' @param calculate_loglik If true, the log-likelihood of the estimate
//'   history will be calculated. If false, likelihood will be calculated.
//'
//' @noRd
//'
// [[Rcpp::export]]
double loglik_est_history_cpp(Rcpp::List est_history, double theta,
                              bool calculate_loglik = true) {

  // Clone the inputs
  Rcpp::List eh = clone(est_history);
  Rcpp::List eh_step;

  // Get Administered item pool and response string
  Rcpp::S4 administered_ip = get_administered_items_cpp(eh);
  // The number of items in the test so far
  int item_no = as<List>(administered_ip.slot("item_list")).size();
  Rcpp::NumericVector resp(item_no);
  for (int i=0; i < item_no; i++) {
    eh_step = eh[i];
    // Make sure both "resp" and "item" exists and they are valid.
    if (eh_step.containsElementNamed("resp") &&
       // Make sure the item is valid.
        eh_step.containsElementNamed("item") && TYPEOF(eh_step["item"]) == S4SXP) {
          // Make sure resp element is numeric or integer
          if (TYPEOF(eh_step["resp"]) == REALSXP || TYPEOF(eh_step["resp"]) == INTSXP) {
            resp[i] = eh_step["resp"];
          } else stop("Inadmissable resp value!");
      // Rcout << "lik_est_history resp[" << i << "] = " << resp[i] << std::endl;
      // Rcout << "lik_est_history typeof ip[" << i << "] = " << TYPEOF(temp_list[i]) << std::endl;
    }
  }
  if (calculate_loglik) {
    return resp_loglik_bare_itempool_cpp(resp, theta, administered_ip);
  } else
    return resp_lik_bare_itempool_cpp(resp, theta, administered_ip);
}



//#############################################################################@
//########################### select_next_item_fisher_max_info_cpp #############
//#############################################################################@
// This function first gets the remaining items list and calculates information
// values of each element (item or testlet). Then it orderst the elements
// from the ones who has the highest information to the lowest ones.
// It returns a sorted list of elements and sorted criteria (which is the
// information values).
//
// [[Rcpp::export]]
Rcpp::List select_next_item_fisher_max_info_cpp(Rcpp::List cd,
                                                Rcpp::List est_history,
                                                Rcpp::List additional_args) {
  Rcpp::List temp_list;
  //Rcout << "    select_next_item_fisher_max_info_cpp -- 1" << std::endl;

  temp_list = process_testlet_cat_cpp(cd, est_history, additional_args);
  //Rcout << "    select_next_item_fisher_max_info_cpp -- 2" << std::endl;
  if (temp_list.size() > 0) return temp_list;
  Rcpp::List eh = clone(est_history); // this may change, hence clone
  Rcpp::List aa = clone(additional_args); // this may change, hence clone
  int item_no = eh.size();  // The stage of the test.
  // Create an item pool of remaining items
  Rcpp::S4 remaining_ip = get_remaining_items_cpp(cd, eh, aa);
  //Rcout << "    select_next_item_fisher_max_info_cpp -- 4" << std::endl;
  Rcpp::List remaining_ip_list = remaining_ip.slot("item_list");
  int num_of_remaining_items = remaining_ip_list.size();
  if (num_of_remaining_items == 0)
    stop("There are no items to select for the next item selection function.");
  // info_values will hold the information values of each element (item
  // or testlet)
  Rcpp::NumericVector info_values(num_of_remaining_items);

  // Get the previous (actually current) estimated ability
  Rcpp::List est_history_last_step = eh[item_no-1];
  double current_ability_est = est_history_last_step("est_before");
  //Rcout << "    select_next_item_fisher_max_info_cpp -- 6" << std::endl;

  // Since there is one theta, the output will be a matrix with one row. So,
  // it can be converted to a NumericVector.
  info_values = info_itempool_bare_cpp(current_ability_est, remaining_ip);
  // Get the sorted (from lowest to the highest) indices of info_values.

  // Rcpp::IntegerVector idx = seq_along(info_values) - 1;
  // std::sort(idx.begin(), idx.end(), [&](int i, int j){return info_values[i] > info_values[j];});
  Rcpp::IntegerVector idx = order_decreasing(info_values);

  // Sort info_values and remaining_ip_list using the sort order of epv_list
  // return Rcpp::List::create(Named("criteria") = info_values[idx],
  //                           Named("remaining_ip_list") = remaining_ip_list[idx]);
  return return_select_next_item_output(cd, eh, remaining_ip_list[idx], aa);
}


// //#############################################################################@
// //########################### select_next_item_fmi_cpp ########################@
// //#############################################################################@
//
// Rcpp::S4 select_next_item_fmi_cpp(
//     double theta, Rcpp::S4 ip, int randomesqueN)
// {
//   int num_of_items = as<List>(ip.slot("item_list")).size();
//   int selected_item_no, i, j;
//   double maxInfoThreshold;
//   Rcpp::NumericMatrix ipMatrix;
//   Rcpp::NumericVector infos;
//   Rcpp::List item_list = ip.slot("item_list");
//
//   ipMatrix = get_parameters_itempool_cpp(ip);
//   //Rcpp::NumericMatrix theta_matrix(1,1);
//   //theta_matrix(0,0) = theta;
//   //infos = info_itempool_cpp(theta_matrix, ip, false, false, R_NilValue);
//   // Since there is one theta, the output will be a matrix with one row. So,
//   // it can be converted to a NumericVector.
//   // infos = as<Rcpp::NumericVector>(info_itempool_cpp(theta, ip, false, false, R_NilValue));
//   infos = info_itempool_bare_cpp(theta, ip);
//
//   if (num_of_items <= randomesqueN) {
//     // selected_item_no = rand() % num_of_items + 1;
//     selected_item_no = as<int>(Rcpp::sample(num_of_items, 1));
//   } else {
//     // this indexes vector will hold the indexes of the maximum information
//     // items found so far.
//     IntegerVector indexes(randomesqueN);
//     // maxInfoThreshold is the minimum information value among the top
//     // randomesqueN items.
//     maxInfoThreshold = infos(0,0);
//     // minIndex will hold the index of the minimum element of the indexes
//     // vector.
//     int minIndex = 0;
//     for (j = 0; j < randomesqueN; j++) {
//       indexes[j] = j;
//       if (infos(0,j) < maxInfoThreshold) {
//         maxInfoThreshold = infos(0,j);
//         minIndex = j;
//       }
//     }
//
//     // Top randomesqueN most informative items will be hold here.
//     for (i = randomesqueN; i < num_of_items; i++)
//     {
//       if (infos(0,i) > maxInfoThreshold)
//       {
//         // Find the indexes which has the minimum information, remove it
//         // and replace it with the newly found value.
//
//         indexes[minIndex] = i;
//         maxInfoThreshold = infos(0,i);
//         for (j = 0; j < randomesqueN; j++)
//         {
//           if (infos(0,indexes[j]) < maxInfoThreshold)
//           {
//             maxInfoThreshold = infos(0,indexes[j]);
//             minIndex = j;
//           }
//         }
//       }
//     }
//     // Randomly select one of the items form the top informative items
//     // selected_item_no = indexes[rand() % randomesqueN] + 1;
//     selected_item_no = indexes[as<int>(Rcpp::sample(randomesqueN, 1)) - 1] + 1;
//   }
//
//   // Rcout << "  Stage (sni-fmi) 1 -  selected_item_no: " << selected_item_no << std::endl;
//   // Rcout << "  Stage (sni-fmi) 2 -  ENDING" << std::endl;
//   return item_list[selected_item_no - 1];
// }

//#############################################################################@
//#############################################################################@
//#############################################################################@
//########################### select_next_item_mepv_cpp ########################
//#############################################################################@
//#############################################################################@
//#############################################################################@


//#############################################################################@
//########################### calculate_epv_cpp ################################
//#############################################################################@

double calculate_epv_cpp(std::string var_calc_method,
                         NumericVector current_resp,
                         Rcpp::NumericVector previous_resp,
                         double current_ability_est,
                         Rcpp::S4 candidate_item,
                         Rcpp::S4 administered_ip,
                         double prior_mean,
                         double prior_var) {
  // This function calculates the expected posterior variance of a candidate
  // item or testlet given the previously administered items or previous prior
  // distribution. It calculates testlet level expected posterior variance by
  // combining all items.

  // Rcout << std::endl << "          (calculate_epv_cpp) -- 1 -- Beginning " << std::endl;
  // Rcout << "    (select_next_item) 1 -  Beginning" << std::endl;

  // Timer timer;      // start the timer //@@@@@@@@@@@@@@ TIMER @@@@@@@@@@@@@@@@@@
  // timer.step("\n\n-- Starting Function");   // record the starting point

  // an indicator for a testlet
  bool item_is_testlet = candidate_item.inherits("Testlet");
  Rcpp::List testlet_item_list;
  Rcpp::S4 testlet_ip("Itempool");
  if (item_is_testlet) {
    testlet_ip = as<Rcpp::S4>(candidate_item.slot("item_list"));
    testlet_item_list = testlet_ip.slot("item_list");
    // Rcout << "          (calculate_epv_cpp) -- 1.1 -- testlet size = " << testlet_size << std::endl;
  } else {
    // Convert the single item to testlet with one item
    testlet_item_list = List::create(candidate_item);
    testlet_ip.slot("item_list") = testlet_item_list;
  }
  // Rcout << "          (calculate_epv_cpp) -- 1.2 -- Beginning - item_is_testlet = "
  // << item_is_testlet << std::endl;

  // timer.step("Calculating P       "); //@@@@@@@@@@@@@@ TIMER @@@@@@@@@@@@@@@@@@

  // The following is the probability of giving response resp to item
  // given the previous responses.
  // TODO: The following line is different than the formula given in Choi
	// and Swatz (2009). In that paper, it is given the other responses,
	// probability of correct response. Also, according to van der Linden
  // and Glas (2010) Elements of Adaptive Testing p. 16 Eq. (1.26) P
  // should be calculated purely in a Bayesian fashion.
  // Here, for now, it is calculate as given the current ability (not
  // previous answers)estimate, which is not very accurate.
  double P = resp_lik_bare_itempool_cpp(current_resp, current_ability_est, testlet_ip);

  Rcpp::List est_list;

  // timer.step("P Calculated        "); //@@@@@@@@@@@@@@ TIMER @@@@@@@@@@@@@@@@@@
  if (var_calc_method == "owen") {
    // Rcout << "            (calculate_epv_cpp) -- 4.1 -- Owen Selected" << std::endl;
    // est_list = est_ability_owen_cpp(all_items, all_resp, prior_mean, prior_var);
    est_list = est_ability_owen_cpp(testlet_ip, current_resp, prior_mean, prior_var);
    // Rcout << "              (calculate_epv_cpp) -- 4.1.1 -- owen -- cand_item_id: "
    // << as<std::string>(candidate_item.slot("item_id")) << " - current_resp "
    // << current_resp << " est = " << as<double>(est_list["est"]) << " - se = "
    // <<  as<double>(est_list["se"]) << std::endl;
	} else { // else it is assumed that var_calc_method = "eap"

	  //// Calculate the posterior variance given all previous items and
	  //// current candidate item
      // All responses including current candidate response
    Rcpp::NumericVector all_resp = previous_resp;
    // Rcout << "          (calculate_epv_cpp) -- 2.1 --  Previous Resp Size = "
    // << previous_resp.size() << std::endl;
    for (int i = 0; i < current_resp.size(); i++)
      all_resp.push_back(current_resp[i]);

    // Rcout << "          (calculate_epv_cpp) -- 2.2 --  All Resp Size = "
    // << all_resp.size() << std::endl;
    Rcpp::List all_items_list;
    //// Create an item pool of all items including the current candidate.
    // If administered_ip is not 'Itempool', it means that it is empty.
    // For the first item, since there are no administered items, previous
    // function assigns "item" as object's attribute. So it basically means,
    // administered_ip is empty.


    // Check first item of administered_ip, if it has ID NULL it means administered_ip is empty.
    all_items_list = administered_ip.slot("item_list");

    // timer.step("Check ip is NULL    "); //@@@@@@@@@@@@@@ TIMER @@@@@@@@@@@@@@@@@@
    if (TYPEOF(all_items_list[0]) == 0) { // this means item pool is empty // Rf_isNull(administered_ip)) {
      // Rcout << "            (calculate_epv_cpp) -- 2.2.2 --  administered_ip is NULL " << std::endl;
      if (item_is_testlet) {
        all_items_list = testlet_item_list;
      } else {
        all_items_list = List::create(candidate_item);
      }
    } else { // else means there are administered items, so item can be added to the end safely.
      // Rcout << "            (calculate_epv_cpp) -- 2.2.3 --  administered_ip is NOT NULL " << std::endl;
      // temp_ip = as<S4>(administered_ip); // Convert to S4 because it can be NULLable as argument
      all_items_list = administered_ip.slot("item_list");
      if (item_is_testlet) {
        int testlet_size = testlet_item_list.size();
        for (int i = 0; i < testlet_size; i++)
          all_items_list.push_back(testlet_item_list[i]);
      } else {
        all_items_list.push_back(candidate_item);
      }
    }
    // timer.step("Checked ip is NULL  "); //@@@@@@@@@@@@@@ TIMER @@@@@@@@@@@@@@@@@@
    // Rcout << "          (calculate_epv_cpp) -- 2.3 --  all_items_list Size = "
    // << all_items_list.size() << std::endl;


    Rcpp::S4 all_items("Itempool");
    all_items.slot("item_list") = all_items_list;

    // Rcout << "          (calculate_epv_cpp) -- 4 -- Before Selection" << std::endl;
    // timer.step("Before Ability Est  "); //@@@@@@@@@@@@@@ TIMER @@@@@@@@@@@@@@@@@@



    // Rcout << "            (calculate_epv_cpp) -- 4.2 -- eap Selected" << std::endl;
		// Temp list hold the output of ability estimation
    NumericVector theta_range = NumericVector::create(-5, 5);
    NumericVector prior_par = NumericVector::create(prior_mean, prior_var);
    // Rcout << "              (calculate_epv_cpp) -- 4.2.1 -- eap Selected - Is all items Itempool? "
    // << all_items.inherits("Itempool") << std::endl;

		est_list = est_ability_eap_single_examinee_cpp(
      all_resp, all_items, theta_range, 50, "norm", prior_par);
    // Rcout << "              (calculate_epv_cpp) -- 4.2.3 -- eap -- cand_item_id: "
    // << as<std::string>(candidate_item.slot("item_id")) << " - current_resp "
    // << current_resp << " est = " << as<double>(est_list["est"]) << " - se = 0"
    // <<  as<double>(est_list["se"]) << std::endl;
	}
  // timer.step("After Ability Est   "); //@@@@@@@@@@@@@@ TIMER @@@@@@@@@@@@@@@@@@
  // Rcout << "          (calculate_epv_cpp) -- 5 -- After Selection" << std::endl;

  // timer.step("Finishing mepv      ");   //@@@@@@@@@@@@@@ TIMER @@@@@@@@@@@@@@@@
  // NumericVector res(timer);
  // StringVector res_names = res.names();
  // for (int i=0; i<res.size(); i++) {
    // Rcout << res_names[i] << ": " << res[i]/1000000 << std::endl;
  // }                                     //@@@@@@@@@@@@@@ TIMER @@@@@@@@@@@@@@@@


  return P * as<double>(est_list["se"]) * as<double>(est_list["se"]);
}


//#############################################################################@
//########################### select_next_item_mepv_cpp ########################
//#############################################################################@
// This function will return a list with two elements, one is "criteria"
// element where the "epv" (expected posterior variances) values are sorted
// from the lowest to the highest. The second one is "remaining_ip_list" for
// which the items remained to be administered in the item pool sorted from
// the best to the worst according to the "criteria" vector.
// "remaining_ip_list" is a list, not an "Itempool" object.
//
// Description of MEPV according to "Murphy, Dodd and Vaughn (2010) A
// comparison of item selection techniques for testlets" p. 427:
// "The MEPV selects the kth item of the CAT that minimizes the expected
// variance of the posterior distribution based on predicted response r. That
// is, for each possible response r to a given item j (i.e., in {0, 1} for
// dichotomous items), the variance of the posterior distribution is evaluated
// at the theta estimate based on the previous responses and predicted response r.
// Given the previous response history, for dichotomous items the expected
// variance will be minimized as the probability of giving response r
// approaches .5 at theta_hat. It follows that the inverse of the variance, known as
// the precision, will be maximized as the variance is minimized. In our
// study, the expected precision of each individual item is computed and
// summed within the corresponding testlets. The MEPV then selects the testlet
// that maximizes the expected precision of theta_hat given the response history."
//
// Function assumes that when a testlet is selected all of the items in that
// testlet will be administered.
Rcpp::List select_next_item_mepv_cpp(Rcpp::List cd, Rcpp::List est_history,
                                     Rcpp::List additional_args) {
// Rcout << "    mepv -- 1 " << std::endl;
  Rcpp::List temp_list;
  temp_list = process_testlet_cat_cpp(cd, est_history, additional_args);
  if (temp_list.size() > 0) return temp_list;
  // Rcout << "    mepv -- 2 " << std::endl;
  
  // Timer timer;           // start the timer   //@@@@@@@@@@@@@@ TIMER @@@@@@@@@@
  // timer.step("\n\n-- Starting Function");   // record the starting point
  Rcpp::List eh = clone(est_history); // this may change, hence clone
  Rcpp::List aa = clone(additional_args); // this may change, hence clone
  int item_no = eh.size();  // The stage of the test.
  // Create an item pool of remaining items
  Rcpp::S4 remaining_ip = get_remaining_items_cpp(cd, eh, aa);
  Rcpp::List remaining_ip_list = remaining_ip.slot("item_list");
  int num_of_remaining_items = remaining_ip_list.size();
  if (num_of_remaining_items == 0)
    stop("There are no items to select from for the next item selection function.");
  // epv_list will hold the expected posterior variances of each element (item
  // or testlet)
  Rcpp::NumericVector epv_list(num_of_remaining_items);
  // Create an item pool of administered items
  Rcpp::S4 administered_ip("Itempool");
  administered_ip = get_administered_items_cpp(eh);

  // Rcout << "    mepv -- 4  -- num_of_remaining_items = " << num_of_remaining_items << std::endl;
  // Get the variance calculation method
  Rcpp::List cd_steps = cd["step"];
  Rcpp::List cdi = cd_steps[item_no-1];
  Rcpp::List next_item_par = cdi["next_item_par"];

  std::string var_calc_method = next_item_par["var_calc_method"];
  // Import R function to expand.grid
  Rcpp::Function expand_grid("expand.grid");
  Rcpp::DataFrame patterns;
  // Number of patterns for that item or testlet; no_items: 1 or number of testlet items
  int no_patterns, no_items;
  Rcpp::S4 candidate_item;
  // The following will hold response categories of an item temporarily
  Rcpp::NumericVector resp_categories;
  double epv; // Expected posterior variance
  // double mepv = std::numeric_limits<double>::max(); // Minimum Expected posterior variance.

  // timer.step("Getting Est History "); //@@@@@@@@@@@@@@ TIMER @@@@@@@@@@@@@@@@@@
  // Get previous responses
  // -1 is for the last item has not been administered yet hence no response
  Rcpp::NumericVector previous_resp(item_no - 1);
  for (int i = 0; i < item_no - 1; i++) {
    temp_list = eh[i];
    previous_resp[i] = as<int>(temp_list["resp"]);
  }
  // Get the previous (actually current) estimated ability
  Rcpp::List est_history_last_step = eh[item_no-1];
  double current_ability_est = est_history_last_step("est_before");

  // If var_calc_method is "owen" prior information regarding previous parameters
  // does not need to be calculated each time. Simply an informative prior works.
  double prior_mean = 0;
  double prior_var = 1;
  // Make sure administered_ip is valid, i.e. it is not the beginning of the test
  if (var_calc_method == "owen" && item_no > 1) {
    temp_list = est_ability_owen_cpp(administered_ip, previous_resp,
                                     prior_mean, prior_var);
    prior_mean =  temp_list["est"];
    // pow(as<double>(temp_list["se"]), 2)
    prior_var =  as<double>(temp_list["se"]) * as<double>(temp_list["se"]);
  }

  // timer.step("Before loops        "); //@@@@@@@@@@@@@@ TIMER @@@@@@@@@@@@@@@@@@
  // Rcout << "    mepv -- 6" << std::endl;
  // iterate through all of the remaining items
  for (int i = 0; i < num_of_remaining_items; i++) {
    // timer.step("Loop " + std::to_string(i) + "             "); //@@@@@@@@@@@@@@ TIMER @@@@@@@@@@@@@@@@@@
    epv = 0;
    // Get the item or testlet
    candidate_item = as<Rcpp::S4>(remaining_ip_list[i]);
    // Rcout << std::endl << std::endl << std::endl << "      mepv -- 6.1 -- Starting Simulation with Item: " << candidate_item_id << std::endl;
    if (candidate_item.inherits("Testlet")) {
      // Rcout << "        mepv -- 6.2 -- Candidate Testlet" << std::endl;
      // Create possible response patterns of all items within the testlet
      std::string candidate_item_id = as<std::string>(candidate_item.slot("testlet_id"));
      Rcpp::S4 ip, testlet_item;
      ip = as<S4>(candidate_item.slot("item_list"));// item list of the testlet as Itempool
      temp_list = ip.slot("item_list");  // item list as a list object
      int no_of_testlet_items = temp_list.size(); // number of items in testlet
      // This will hold the response categories of each item:
      Rcpp::List resp_category_list(no_of_testlet_items);
      for (int j = 0; j < no_of_testlet_items; j++) {
        testlet_item = as<S4>(temp_list[j]);
        // Return the possible response categories for a given item. For dichotomous
        // IRT models it will return c(0, 1). For polytomous IRT models it will
        // return c(0, 1, 2, ...), number of item thresholds plus one.
        resp_categories = seq(0, get_max_possible_score_item_cpp(testlet_item));
        resp_category_list[j] = resp_categories;
      }
      // Create all possible patterns of items in the testlet using expand.grid:
      patterns = expand_grid(resp_category_list);
    // } else if (candidate_item.inherits("Item")) {
    } else {
      std::string candidate_item_id = as<std::string>(candidate_item.slot("item_id"));
      // Rcout << "        mepv -- 6.3.1 -- Candidate item" << std::endl;

      // Return the possible response categories for a given item. For dichotomous
      // IRT models it will return c(0, 1). For polytomous IRT models it will
      // return c(0, 1, 2, ...), number of item thresholds plus one.
      resp_categories = seq(0, get_max_possible_score_item_cpp(candidate_item));
      // Rcout << "        mepv -- 6.3.2 -- Expanding Grid" << std::endl;
      // patterns data frame has only one column where each row represent
      // a different response category
      patterns = expand_grid(resp_categories);
    }
    no_patterns = patterns.nrow(); // alternatively resp_row.size()
    no_items = patterns.ncol(); // number of items, 1 if not testlet.
    NumericVector current_resp(no_items); // vector holding the responses
    IntegerVector temp_int_vector(no_patterns);
    // Iterate patterns and find epv (expected posterior variance)
    // Rcout << "      mepv -- 6.3.3 -- Starting epv Calc" << std::endl;
    // timer.step("Before calc. epv    "); //@@@@@@@@@@@@@@ TIMER @@@@@@@@@@@@@@@@@@
    for (int pi = 0; pi < no_patterns; pi++) { // pi: pattern index
      // Build the response vector:
      for (int j = 0; j < no_items; j++) {
        temp_int_vector = patterns[j];
        current_resp[j] = temp_int_vector[pi];
      }

      // Rcout << std::endl << "        mepv -- 6.3.3.1 -- epv Calc Iteration: "
      // << pi << "/" << no_patterns-1 << "  Current Item: "
      // << as<std::string>(candidate_item.slot("item_id")) << " - Response: "
      // << current_resp << std::endl;
      // Rcout << "        mepv -- 6.3.3.2 -- Previous epv = " << epv << std::endl;
      epv = epv + calculate_epv_cpp(var_calc_method, current_resp, previous_resp,
                                    current_ability_est, candidate_item,
                                    administered_ip, prior_mean, prior_var);
      // Rcout << "        mepv -- 6.3.3.3 -- Updated epv = " << epv << std::endl;
    }
    // timer.step("After calc. epv     "); //@@@@@@@@@@@@@@ TIMER @@@@@@@@@@@@@@@@@@
    // Rcout << "      mepv -- 6.4 -- Final Updating epv = " << epv << std::endl;

    epv_list[i] = epv;
    // // Updtate the mininum epv and the ID of the item if a smaller value found
    // if (epv < mepv) {
    //   mepv = epv;
    //   selected_item = candidate_item;
    //   // Rcout << "      mepv -- 6.5 -- Lowest epv reached!! epv = " << epv << std::endl;
    // }
  }
  // Get the sorted (from lowest to the highest) indices of epv values.
  // IntegerVector idx = seq_along(epv_list) - 1;
  // std::sort(idx.begin(), idx.end(), [&](int i, int j){return epv_list[i] < epv_list[j];});
  Rcpp::IntegerVector idx = order_increasing(epv_list);

  // Rcout << "      mepv -- 7 -- Returning selected item!! " << std::endl;

  // timer.step("Finishing mepv      ");   //@@@@@@@@@@@@@@ TIMER @@@@@@@@@@@@@@@@
  // NumericVector res(timer);
  // StringVector res_names = res.names();
  // for (int i=0; i<res.size(); i++) {
    // Rcout << res_names[i] << ": " << res[i]/1000000 << std::endl;
  // }                                     //@@@@@@@@@@@@@@ TIMER @@@@@@@@@@@@@@@@



  // // Sort epv_list and remaining_ip_list using the sort order of epv_list
  // return List::create(Named("criteria") = epv_list[idx],
  //                     Named("remaining_ip_list") = remaining_ip_list[idx]);
  return return_select_next_item_output(cd, eh, remaining_ip_list[idx], aa);
}

//#############################################################################@
//########################### select_next_item_random_cpp ######################
//#############################################################################@
Rcpp::List select_next_item_random_cpp(Rcpp::List cd, Rcpp::List est_history,
                                       Rcpp::List additional_args) {
  Rcpp::List temp_list;
  temp_list = process_testlet_cat_cpp(cd, est_history, additional_args);
  if (temp_list.size() > 0) return temp_list;

  Rcpp::List eh = clone(est_history); // this may change, hence clone
  Rcpp::List aa = clone(additional_args); // this may change, hence clone
  Rcpp::S4 remaining_ip = get_remaining_items_cpp(cd, eh, aa);
  // Get the remaining item list
  Rcpp::List remaining_item_list = remaining_ip.slot("item_list");
  // Create a shuffled integer list
  Rcpp::IntegerVector int_seq(remaining_item_list.size());
  std::iota(int_seq.begin(), int_seq.end(), 0);
  // Feed the shuffled item list to exposure control parameter and return
  // result
  return return_select_next_item_output(
    cd, eh, remaining_item_list[sample(int_seq, int_seq.size())], aa);
}


//#############################################################################@
//########################### select_next_item_infinite_ip_cpp #################
//#############################################################################@
//' When user do not provide an item pool, administer a perfect item to an
//' examinee at each step of the adaptive test.
//'
//' @noRd
//'
Rcpp::List select_next_item_infinite_ip_cpp(Rcpp::List cd,
                                            Rcpp::List est_history,
                                            Rcpp::List additional_args) {
  Rcpp::List eh = clone(est_history); // this may change, hence clone
  Rcpp::List aa = clone(additional_args); // this may change, hence clone
  int item_no = eh.size();  // The stage of the test.
  Rcpp::List est_history_last_step = eh[item_no-1];

  //Rcpp::S4 temp_s4("1PL");
  Rcpp::S4 item("1PL");
  // Get the latest ability estimate
  // Set item parameter as the current estimate, i.e. perfect item
  // is administered to examinee from an infinite item pool
  item.slot("b") = est_history_last_step["est_before"];
  item.slot("item_id") = "Item-" + std::to_string(item_no);
  //item = temp_s4;
  est_history_last_step["testlet"] = R_NilValue; // Set the selected testlet
  est_history_last_step["item"] = item;  // Set the selected item
  eh[item_no-1] = est_history_last_step; // Update est_history
  return List::create(Named("est_history") = eh,
                      Named("additional_args") = aa);
}


//#############################################################################@
//########################### select_next_item_fixed_cpp #######################
//#############################################################################@
//' Select an item from a predetermined list of items.
//'
//' !!!! FIX THIS for testlets  !!!!
//'      Currently it automatically selects the
//'      first item of the testlet with the assumption that testlet items
//'      has previously been administered.
//'
//'
//'
//'
//' @noRd
//'
Rcpp::List select_next_item_fixed_cpp(Rcpp::List cd, Rcpp::List est_history,
                                      Rcpp::List additional_args) {
  Rcpp::List temp_list;
  temp_list = process_testlet_cat_cpp(cd, est_history, additional_args);
  if (temp_list.size() > 0) return temp_list;

  Rcpp::List eh = clone(est_history); // this may change, hence clone
  Rcpp::List aa = clone(additional_args); // this may change, hence clone
  int item_no = eh.size();  // The stage of the test.
  Rcpp::List est_history_last_step = eh[item_no-1];
  Rcpp::List cd_steps = cd["step"];
  Rcpp::List cdi = cd_steps[item_no-1];
  // Create an item pool of remaining items
  S4 remaining_ip = get_remaining_items_cpp(cd, eh, aa);
  // Get next_item_par
  Rcpp::List next_item_par = cdi["next_item_par"];
  // Get the next item parameter "item_id", which is the ID of the next
  // administered item.
  std::string next_item_id = next_item_par("item_id");
  // Vector that hold the ID's of remaining items in the item pool:
  Rcpp::StringVector remaining_ip_ids;
  remaining_ip_ids = get_slot_itempool_cpp(remaining_ip, "id");
  int temp_int = remaining_ip_ids.size();

  Rcpp::List remaining_item_list = remaining_ip.slot("item_list");
  for (int i = 0; i < temp_int; i++) {
    if (next_item_id == as<std::string>(remaining_ip_ids[i])) {
      // Check whether next_item_id belongs to an item or testlet, if it is
      // testlet select the first element of the testlet. "next_item_id"
      // should appear for testlets only once because, in further iterations
      // a testlet will be caugth automatically by the code at the beginning
      // of this function and the remaining items of the testlet will be
      // administered automatically.
      Rcpp::S4 object = remaining_item_list[i];  // object can be Item or Testlet
      if (object.inherits("Testlet")) {
        est_history_last_step["testlet"] = object; // Set the selected testlet
        object = as<S4>(object.slot("item_list")); // item_list is Itempool
        temp_list = object.slot("item_list");
        // Select the first item from the testlet
        est_history_last_step["item"] = temp_list[0];
      } else {
        est_history_last_step["testlet"] = R_NilValue; // Set the selected testlet
        est_history_last_step["item"] = object;  // Set the selected item
      }
      eh[item_no-1] = est_history_last_step; // Update est_history
      return List::create(Named("est_history") = eh,
                          Named("additional_args") = aa);
    }
  }
  stop("An item with next_item_id cannot be found within the remaining items.");
}

//#############################################################################@
//########################### get_unadministered_testlet_items_cpp #############
//#############################################################################@
//' Get list of unadministered items within a testlet
//'
//' This function returns the list of unadministered items from a Testlet using
//' the estimate history of CAT so far. The names of each list element is the
//' same as the item's ids. Consequently, can be easily used to build an
//' itempool object.
//' The function assumes that the testlet items were administered consecutively.
//'
//' @return The function will return a list of remaining items in the testlet.
//'   If there is no remaining item in the testlet, the function will return
//'   an empty list.
//'
//'
//'
//' @noRd
//'
// [[Rcpp::export]]
Rcpp::List get_unadministered_testlet_items_cpp(Rcpp::S4 testlet,
                                                Rcpp::List est_history) {
  // Output will be a list of remaining items
  int item_no = est_history.size();  // The stage of the test.
  // Item pool of testlet
  Rcpp::S4 temp_s4 = as<S4>(testlet.slot("item_list")); //item pool of testlet
  // list of items in the testlet
  Rcpp::List testlet_item_list = temp_s4.slot("item_list");
  // if there are no items administered so far, return all items in testlet
  if (item_no == 1) return testlet_item_list;

  int num_of_testlet_items = testlet_item_list.size();
  Rcpp::List remaining_item_list(num_of_testlet_items);// = Rcpp::List::create();
  Rcpp::List eh_step;
  Rcpp::S4 temp_item;
  // unadministered item IDs
  Rcpp::StringVector remaining_item_ids(num_of_testlet_items);
  std::string temp_s4_id;
  std::string temp_item_id;
  // a boolean indicating whether to add the testlet item to remaining items
  // or not.
  bool add_item;
  // iterate through all testlet items. If a testlet item has not been
  // administered yet, add that to the remaining items list.
  int item_ind = 0; // this will hold the unadminstered item index
  for (int i = 0; i < num_of_testlet_items; i++) {
    add_item = true;
    temp_s4 = as<S4>(testlet_item_list[i]); // select the testlet item
    temp_s4_id = get_s4_id(temp_s4);
    // Check whether this testlet item has been administered before:
    for (int j = item_no - 2; j >= 0; j--) {
      eh_step = est_history[j];
      temp_item = as<S4>(eh_step["item"]);
      std::string temp_item_id = as<std::string>(temp_item.slot("item_id"));
      if (temp_s4_id == temp_item_id) {
        add_item = false;
        break;
      }
    }
    // TODO: Use a preallocated list instead of push_back
    if (add_item) {
      remaining_item_list(item_ind) = temp_s4;
      remaining_item_ids(item_ind) = temp_s4_id;
      // remaining_item_list.push_back(temp_s4);
      item_ind++;
    }
  }
  if (item_ind == 0) return Rcpp::List::create();
  remaining_item_list = remaining_item_list[seq(0, item_ind-1)];
  remaining_item_list.attr("names") = remaining_item_ids[seq(0, item_ind-1)];
  return remaining_item_list;
}

//#############################################################################@
//########################### select_next_testlet_item_none_cpp ################
//#############################################################################@
//' Select remaining testlet items if a testlet has been chosen.
//' 
//' @description This item selection method chooses testlet items in the 
//'   same order they appear in the testlet. 
//' 
//' @noRd
//'
//'
// [[Rcpp::export]]
Rcpp::List select_next_testlet_item_none_cpp(Rcpp::List cd,
                                             Rcpp::List est_history,
                                             Rcpp::List additional_args) {

  Rcpp::List default_output = Rcpp::List::create();
  int item_no = est_history.size();  // The stage of the test.
  Rcpp::List est_history_last_step = est_history[item_no - 1];
  // Rcout << "    select_next_testlet_item_none_cpp -- 1 -- item_no = " << item_no << " -- Testlet NULL: " <<  Rf_isNull(est_history_last_step["testlet"]) << std::endl;
  // If this is first item, no testlets has been selected yet, so no need
  // for this section. Alternatively, sometimes, item selection functions 
  // such as "select_next_item_fisher_max_info_cpp", selects a testlet but 
  // leaves testlet selection to "process_testlet_cat_cpp" function. 
  if (item_no == 1 && Rf_isNull(est_history_last_step["testlet"])) {
    // Rcout << "    select_next_testlet_item_none_cpp -- 4 -- First Step -- Returning" << std::endl;
    return default_output;
  }
  Rcpp::S4 testlet("Testlet");
  // if in this last step testlet has not been selected, it is possible that
  // this is the second (or later) item in the testlet selection process. But
  // if the second to the last is not a testlet either, then this function 
  // should return an empty list.   
  if (Rf_isNull(est_history_last_step["testlet"])) {
    // Rcout << "    select_next_testlet_item_none_cpp -- 8 -- Testlet is NULL" << std::endl;
    Rcpp::List est_history_penultimate_step = est_history[item_no-2];
    if (Rf_isNull(est_history_penultimate_step["testlet"])) {
      return default_output;
    } else {
      testlet = as<S4>(est_history_penultimate_step["testlet"]);
    }
  } else {
    testlet = as<S4>(est_history_last_step["testlet"]);
  }
  // Check whether testlet termination criteria has been met for this testlet
  if (terminate_testlet_cat_cpp(testlet, cd, est_history, additional_args)) {
    return default_output;
  }

  Rcpp::List remaining_item_list = get_unadministered_testlet_items_cpp(
    testlet, est_history);
  if (remaining_item_list.size() > 0) { // there is at least one item in
                                        // the testlet to be administered
    Rcpp::List eh = clone(est_history);
    Rcpp::List aa = clone(additional_args);
    Rcpp::List eh_last_step = clone(est_history_last_step);
    
    Rcpp::S4 temp_s4 = as<S4>(remaining_item_list[0]);
    eh_last_step["testlet"] = testlet; // Set the selected testlet
    eh_last_step["item"] = temp_s4;  // Set the selected item
    eh[item_no-1] = eh_last_step; // Update est_history
    return Rcpp::List::create(Named("est_history") = eh,
                              Named("additional_args") = aa);
  } else { // there are not items left in testlet to be administered
    return default_output;
  }
  return default_output;
}

//#############################################################################@
//########################### select_next_testlet_item_mfi_cpp #################
//#############################################################################@

//' Select remaining testlet items using Maximum Fisher Information if a
//' testlet has been chosen
//'
//' @return This function returns either an empty list which means there
//'   no items in the item pool to be selected. Or, it will return the most
//'   informative item from the testlet that has not been administered. The
//'   function will return an updated estimate history with the updated item
//'   and testlet fields.
//'
//'
//' @noRd
//'
//'
// [[Rcpp::export]]
Rcpp::List select_next_testlet_item_mfi_cpp(Rcpp::List cd,
                                            Rcpp::List est_history,
                                            Rcpp::List additional_args) {
  Rcpp::List default_output = Rcpp::List::create();
  int item_no = est_history.size();  // The stage of the test.
  
  Rcpp::List est_history_last_step = est_history[item_no - 1];
  // Get current ability estimate based on the last step. It is important
  // get the current ability estimate now because, est_history_last_step can 
  // be set to the step before in case the testlet of the last step is NULL. 
  double current_ability_est = est_history_last_step("est_before");
  // Rcout << "    select_next_testlet_item_mfi_cpp -- 1  -- item_no = " << item_no << std::endl;
  
  // If this is first item, no testlets has been selected yet, so no need
  // for this section. Alternatively, sometimes, item selection functions 
  // such as "select_next_item_fisher_max_info_cpp", selects a testlet but 
  // leaves testlet selection to "process_testlet_cat_cpp" function. 
  if (item_no == 1 && Rf_isNull(est_history_last_step["testlet"])) 
    return default_output;
  Rcpp::S4 testlet("Testlet");
  // if in this last step testlet has not been selected, it is possible that
  // this is the second (or later) item in the testlet selection process. But
  // if the second to the last is not a testlet either, then this function 
  // should return an empty list.   
  if (Rf_isNull(est_history_last_step["testlet"])) {
    //Rcout << "    select_next_testlet_item_mfi_cpp -- 4 " << std::endl;
    Rcpp::List est_history_penultimate_step = est_history[item_no-2];
    if (Rf_isNull(est_history_penultimate_step["testlet"])) {
      return default_output;
    } else {
      testlet = as<S4>(est_history_penultimate_step["testlet"]);
    }      
    //est_history_last_step = est_history[item_no-2];
  } else {
    testlet = as<S4>(est_history_last_step["testlet"]);
  }
  // Check whether testlet termination criteria has been met for this testlet
  if (terminate_testlet_cat_cpp(testlet, cd, est_history, additional_args)) {
    return default_output;
  }
  // Get the remaining items within the testlet as a list, the list is named
  // with item id's.
  Rcpp::List remaining_item_list = get_unadministered_testlet_items_cpp(
        testlet, est_history);
  int num_of_remaining_items = remaining_item_list.size();
  // info_values will hold the information values of each element (item
  // or testlet)
  Rcpp::NumericVector info_values(num_of_remaining_items);
  
  Rcpp::List eh = clone(est_history); // this may change, hence clone
  Rcpp::List aa = clone(additional_args); // this may change, hence clone
  // Get the previous (actually current) estimated ability
  Rcpp::List eh_last_step = clone(est_history_last_step);
  // Rcout << "    select_next_testlet_item_mfi_cpp -- 8  -- current_ability_est = " << current_ability_est << "  --  num_of_remaining_items = " << num_of_remaining_items << std::endl;
    
  // Create an Itempool from the remaining_item_list
  Rcpp::S4 remaining_ip("Itempool");
  remaining_ip.slot("item_list") = remaining_item_list;
  
  // Since there is one theta, the output will be a matrix with one row. So,
  // it can be converted to a NumericVector.
  info_values = info_itempool_bare_cpp(current_ability_est, remaining_ip);
  
  Rcpp::IntegerVector idx = order_decreasing(info_values);
  
  Rcpp::S4 temp_s4 = as<S4>(remaining_item_list[idx[0]]);
  eh_last_step["testlet"] = testlet; // Set the selected testlet
  eh_last_step["item"] = temp_s4;  // Set the selected item
  eh[item_no-1] = eh_last_step; // Update est_history
  return Rcpp::List::create(Named("est_history") = eh,
                            Named("additional_args") = aa);
  
  
  // Rcpp::List temp_list = est_history[item_no-2];
  // if (Rf_isNull(temp_list["testlet"]) || item_no <= 1) {
  //   return default_output;
  // }
  // Rcpp::S4 testlet("Testlet");
  // testlet = as<S4>(temp_list["testlet"]);
  // // Check whether testlet termination criteria has been met for this testlet
  // if (terminate_testlet_cat_cpp(testlet, cd, est_history, additional_args)) {
  //   return default_output;
  // }
  // 
  // // Get the remaining items within the testlet as a list, the list is named
  // // with item id's.
  // Rcpp::List remaining_item_list = get_unadministered_testlet_items_cpp(
  //       testlet, est_history);
  // int num_of_remaining_items = remaining_item_list.size();
  // // info_values will hold the information values of each element (item
  // // or testlet)
  // Rcpp::NumericVector info_values(num_of_remaining_items);
  // 
  // Rcpp::List eh = clone(est_history); // this may change, hence clone
  // Rcpp::List aa = clone(additional_args); // this may change, hence clone
  // 
  // 
  // // Get the previous (actually current) estimated ability
  // Rcpp::List est_history_last_step = eh[item_no-1];
  // double current_ability_est = est_history_last_step("est_before");
  // 
  // // Create an Itempool from the remaining_item_list
  // Rcpp::S4 remaining_ip("Itempool");
  // remaining_ip.slot("item_list") = remaining_item_list;
  // 
  // // Since there is one theta, the output will be a matrix with one row. So,
  // // it can be converted to a NumericVector.
  // info_values = info_itempool_bare_cpp(current_ability_est, remaining_ip);
  // 
  // Rcpp::IntegerVector idx = order_decreasing(info_values);
  // 
  // Rcpp::S4 temp_s4 = as<S4>(remaining_item_list[idx[0]]);
  // est_history_last_step["testlet"] = testlet; // Set the selected testlet
  // est_history_last_step["item"] = temp_s4;  // Set the selected item
  // eh[item_no-1] = est_history_last_step; // Update est_history
  // return Rcpp::List::create(Named("est_history") = eh,
  //                           Named("additional_args") = aa);
}


//#############################################################################@
//########################### terminate_testlet_cat_cpp ########################
//#############################################################################@
//' Check whether to terminate the testlet
//'
//' @description This function checks whether the termination criteria has
//' been satisfied for a given testlet. It will check the est_history for
//' whether testlet's termination met.
//'
//' @return A boolean value. If testlet termination criteria has not been met
//' and more items can be administered from this testlet, it will return
//' `false`. If termination criteria has been met and no more items can be
//' administered from the testlet, it will return `true`.
//'
//'
//' @noRd
//'
//'
// [[Rcpp::export]]
bool terminate_testlet_cat_cpp(Rcpp::S4 testlet,
                               Rcpp::List cd,
                               Rcpp::List est_history,
                               Rcpp::List additional_args) {
  Rcpp::List testlet_rules = cd["testlet_rules"];
  
  bool terminate_testlet = false;
  
  Rcpp::StringVector termination_rules(1);
  termination_rules[0] = "max_item";
  if (testlet_rules.containsElementNamed("termination_rule")) {
    termination_rules = Rcpp::as<Rcpp::StringVector>(testlet_rules["termination_rule"]);
  }
  
  // get `termination_par`, if it doesn't exist, set it to list(max_item = 999)
  Rcpp::List termination_pars = Rcpp::List::create(Named("max_item") = 999);
  if (testlet_rules.containsElementNamed("termination_par")) {
    termination_pars = Rcpp::as<Rcpp::List>(testlet_rules["termination_par"]);
  }
  
  // get `termination_rule`, if it doesn't exist, set it to "max_item"
  std::string termination_rule = "max_item";
    //testlet_rules.containsElementNamed("termination_rule") ?
    //Rcpp::as<std::string>(testlet_rules["termination_rule"]) : "max_item";
    
  // Rcpp::List remaining_item_list = get_unadministered_testlet_items_cpp(
  //       testlet, est_history);
  // int remaining_item_number = remaining_item_list.size();
  // // if no items left in the testlet, terminate testlet
  // if (remaining_item_number == 0) return true;

  std::string testlet_id = as<std::string>(testlet.slot("testlet_id"));
  Rcpp::S4 temp_s4 = as<S4>(testlet.slot("item_list")); //item pool of testlet
  // List of items in the testlet
  Rcpp::List testlet_item_list = temp_s4.slot("item_list");
  int num_of_testlet_items = testlet_item_list.size();
  
  // Calculate number of items administered so far from this testlet, if 
  // it is larger than the number of items in the testlet, terminate the 
  // testlet (i.e. return true) without checking any testlet termination 
  // criteria.
  int num_of_items_administered = 0;
  int item_no = est_history.size();  // The stage of the test.
  Rcpp::List eh_step;
  Rcpp::S4 temp_testlet;
  for (int j = item_no - 2; j >= 0; j--) {
    eh_step = est_history[j];
    if (!Rf_isNull(eh_step["testlet"])) {
      temp_testlet = as<S4>(eh_step["testlet"]);
      std::string temp_testlet_id = as<std::string>(
        temp_testlet.slot("testlet_id"));
      if (testlet_id == as<std::string>(temp_testlet.slot("testlet_id"))) {
        num_of_items_administered++;
      }
    }
  }
  // Rcout << std::endl << "terminate_testlet_cat_cpp -- item_no = "<< item_no << "  -- num_of_testlet_items = " << num_of_testlet_items <<  "  -- num_of_items_administered = " << num_of_items_administered << std::endl;  
  if (num_of_items_administered >= num_of_testlet_items) {
    return true;
  }
  
  for (int r = 0; r < termination_rules.size(); r++) {
    termination_rule = termination_rules[r];
    
    if (termination_rule == "max_item") {
      int max_item = as<int>(termination_pars["max_item"]);
      // Rcout << "  terminate_testlet_cat_cpp -- max_item = " << max_item << std::endl;       
      if (num_of_items_administered >= max_item) {
        // Rcout << "  terminate_testlet_cat_cpp -- Terminate due to matching max_item" << std::endl; 
        return true;
      }
      // return num_of_items_administered >= max_item ||
      //   num_of_items_administered >= num_of_testlet_items;
      
      // if it is the first administered item, there is no previous SE to check,
      // so do not check min_se. At least one item from the testlet should be
      // selected for min_se criteria to be checked. This rule does not trumps
      // overall min_se termination rule. 
    } else if (termination_rule == "min_se" && item_no > 1 && num_of_items_administered > 0) {
      // it is assumed that 'termination_par' has an item called 'min_se', 
      // this is ensured by R level function irt::create_cat_design.
      double min_se = as<double>(termination_pars["min_se"]);
      
      eh_step = est_history[item_no-2];
      double current_se = as<double>(eh_step["se_after"]);
      // Rcout << "  terminate_testlet_cat_cpp -- min_se = " << min_se << "  --  current_se = " << current_se << std::endl;       
      if (current_se <= min_se) {
        // Rcout << "  terminate_testlet_cat_cpp -- Terminate due to matching min_se" << std::endl;   
        return true;
      }
    }
  }
  // by default return true, i.e. terminate the testlet.
  return terminate_testlet;
}


//#############################################################################@
//########################### process_testlet_cat_cpp ##########################
//#############################################################################@
//' Process testlet items in CAT step
//'
//' This function runs before some of the item selection functions to process
//' testlets. Usually if the first item from a testlet is administered, the
//' next item that will be administered will be from the same testlet if
//' there are available items in the testlet.
//'
//' @return The function can return an updated estimate_history element which
//' can be returned as is or it can return an empty list which means the item
//' function can find a new item/testlet.
//'
//' @noRd
//'
//'
// [[Rcpp::export]]
Rcpp::List process_testlet_cat_cpp(Rcpp::List cd, Rcpp::List est_history,
                                   Rcpp::List additional_args) {
  Rcpp::List testlet_rules = cd["testlet_rules"];

  std::string next_testlet_item_rule =
    testlet_rules.containsElementNamed("next_item_rule") ?
    Rcpp::as<std::string>(testlet_rules["next_item_rule"]) : "none";
  // Rcout << "    process_testlet_cat_cpp -- 2  -- " << next_testlet_item_rule << std::endl;
  if  (next_testlet_item_rule == "none") {
    return select_next_testlet_item_none_cpp(cd, est_history, additional_args);
  } else if (next_testlet_item_rule == "mfi") {
    return select_next_testlet_item_mfi_cpp(cd, est_history, additional_args);
  }
  return Rcpp::List::create();
}


//#############################################################################@
//########################### apply_exposure_control_cpp #######################
//#############################################################################@
// This function takes the "remaining_ip_list" which contains unadministered
// items in the item pool sorted from the best (for example if selection
// criteria is mfi, the first element of "remaining_ip_list" is the item or
// testlet that has the highest information) to the worst item to administer
// at this stage and returns one item that is selected.
Rcpp::List apply_exposure_control_cpp(Rcpp::List cd, Rcpp::List est_history,
                                      Rcpp::List remaining_ip_list,
                                      Rcpp::List additional_args) {
  if (remaining_ip_list.size() == 0)
    stop("There are no items to select from for the exposure control function.");
  int item_no = est_history.size();  // The stage of the test.
  // Rcout << "      apply_exposure_control_cpp 1 -- Beginning  -- item_no = "
  // << item_no << " -- remaining_ip_list size = " << remaining_ip_list.size() << std::endl;

  Rcpp::List aa = clone(additional_args);
  // cdi is the "cAT dESIGN iTEM"
  Rcpp::List cd_steps = cd["step"];
  Rcpp::List cdi = cd_steps[item_no-1];

  // Get exposure_control_rule
  std::string exposure_control_rule =
    cdi.containsElementNamed("exposure_control_rule") &&
    !Rf_isNull(cdi("exposure_control_rule")) ?
    Rcpp::as<std::string>(cdi("exposure_control_rule")) : "";
  // Get exposure_control_par
  Rcpp::List exposure_control_par = cdi.containsElementNamed("exposure_control_par") ?
    cdi["exposure_control_par"] : List();

  if (exposure_control_rule == "randomesque") {
    int randomesque_n = exposure_control_par("num_items");
    // Sample integers from 0 to (randomesque_n - 1)
    IntegerVector sample_n = sample(randomesque_n, 1) - 1;
    // Return the item
    return List::create(Named("additional_args") = aa,
                        Named("item") = as<S4>(remaining_ip_list[sample_n[0]]));
  } else if (exposure_control_rule == "sympson-hetter") {
    // Check whether "additional_args" has a list called "set_aside_item_list".
    // This list will contain items or testlets that has not been administered
    // during the test but set aside and cannot be administered in this
    // particular administration of the CAT test. If "set_aside_item_list"
    // is not in "additional_args" then create it. Else, extract it from
    // "additional_args".

    // Rcout << "      apply_exposure_control_cpp 2.2 -- sympson-hetter --  " << std::endl;

    Rcpp::List set_aside_item_list;
    if (!aa.containsElementNamed("set_aside_item_list")) {
      set_aside_item_list = List::create();
    } else
      set_aside_item_list = aa["set_aside_item_list"];

    int num_of_remaining_items = remaining_ip_list.size();
    S4 item; // This is either an "item" or "Testlet" object.
    List temp_list;
    double K; // Sympson-Hetter's K value.
    double u; // randomly generated number between 0 and 1.
    // Iterate through each item (from best to worst), get item's
    // "sympson_hetter_k" number, generate a random number between 0 and 1.
    // If this generated number is less than or equal to "sympson_hetter_k"
    // return that item for administration, otherwise, set aside that item
    // (by adding it to "set_aside_item_list") and check the second best
    // item for administration.
    // TODO: what happens if no items can be selected for administration?
    for (int i=0; i < num_of_remaining_items; i++) {
      item = as<S4>(remaining_ip_list[i]);

      // Rcout << "        apply_exposure_control_cpp 2.2.1 -- sympson-hetter --  item ID: "
      // << as<std::string>(item.slot("item_id")) << " --  misc slot exists? "
      // <<  item.hasSlot("misc") << std::endl;

      // Extract the item's sympson_hetter_k
      temp_list = item.slot("misc");
      // Rcout << "        apply_exposure_control_cpp 2.2.2 -- misc slot " <<
      // "exists --  misc has sympson_hetter_k? "
      // << temp_list.containsElementNamed("sympson_hetter_k") << std::endl;
      K = as<double>(temp_list["sympson_hetter_k"]);

      u = as<double>(runif(1, 0, 1));
      // Rcout << "        apply_exposure_control_cpp 2.2.1 -- sympson-hetter -- Step " << i+1 << "/"
      // << num_of_remaining_items << " -- K = " << K  << " -- u = " << u << std::endl;

      if (u > K) { // do not administer item and set that item aside
        // Add item to set_aside_item_list
        set_aside_item_list.push_back(item);


        // If none of the items are selected by the exposure control algorighm,
        // the CAT should stop.
        if (i == num_of_remaining_items - 1) {
          stop("Exposure control function cannot find an appropriate items "
                 "in the item pool to administer.");
        }
        continue;
      } else break;
    }
    // Rcout << "    apply_exposure_control_cpp  -- sympson-hetter -- "
    // << "set_aside_item_list size = " << set_aside_item_list.size()
    // << " --  selected item ID: " << as<std::string>(item.slot("item_id")) << std::endl;

    aa["set_aside_item_list"] = set_aside_item_list;
    return List::create(Named("additional_args") = aa,
                        Named("item") = item);
  }
  // By default return the first item in the remaining_ip_list
  return List::create(Named("additional_args") = aa,
                      Named("item") = as<S4>(remaining_ip_list[0]));
}


//########################### return_select_next_item_output ###################
// If the element is item, return item and testlet as NULL. If it is a
// testlet, then return the first element of the testlet. It is
// assumed that if an item from a testlet already administered
// previously, then it is administered even before this function
// reaches this point. So, if a testlet is selected by
// select_next_item_mepv_cpp, it means that no item from this testlet has
// been administered yet.
Rcpp::List return_select_next_item_output(Rcpp::List cd, Rcpp::List est_history,
                                          Rcpp::List remaining_ip_list,
                                          Rcpp::List additional_args) {
  Rcpp::List eh = clone(est_history);
  Rcpp::List aa = clone(additional_args);
  // Apply exposure control parameters
  Rcpp::List ec_output = apply_exposure_control_cpp(cd, eh, remaining_ip_list,
                                                    aa);
  Rcpp::S4 element = as<S4>(ec_output["item"]);
  int item_no = eh.size();  // The stage of the test.
  Rcpp::List est_history_last_step = eh[item_no-1];
  if (element.inherits("Testlet")) {
    // Set the testlet of the last step of estimate history. 
    est_history_last_step["testlet"] = element; // Set the selected testlet
    eh[item_no-1] = est_history_last_step; // Update est_history
    // Select the best item from the testlet based on 
    Rcpp::List temp_list = process_testlet_cat_cpp(cd, eh, aa);
    if (temp_list.size() > 0) {
      return temp_list;
    } else {
      stop("Error in Testlet selection. Cannot select a testlet.");
    }
    // Rcpp::List temp_list = element.slot("item_list");
    // est_history_last_step["testlet"] = element; // Set the selected testlet
    // // set the selected testlet item as the first item of the testlet
    // est_history_last_step["item"] = as<S4>(temp_list[0]);  // Set the selected item
  // } else if (element.inherits("Item")) {
  } else {
    est_history_last_step["testlet"] = R_NilValue; // Set the selected testlet
    est_history_last_step["item"] = element;  // Set the selected item
  }
  eh[item_no-1] = est_history_last_step; // Update est_history
  return List::create(Named("est_history") = eh,
                      Named("additional_args") = ec_output["additional_args"]);
}

//#############################################################################@
//########################### select_next_item_cpp #############################
//#############################################################################@
//' This function selects an item given an cat design (cd) and estimate
//' history (est_history). It returns a named list with "item" and it's
//' "testlet".
//'
//' @description
//' Assumptions of this function:
//' * This function assumes that the first item is administered and
//'   item number (item_no) is larger than 1.
//' * The est_history already have an current estimate, i.e. a valid
//'   'est' field in the latest element of est_history.
//' *
//'  /// Select Next Item Function Rules /////
//' The rule for next item selection functions such as 
//'     "select_next_item_fisher_max_info_cpp":
//'   * Any prescreening or constraints should be applied within the function.
//'   * Each function will need to get following three arguments:
//'     - 'cd': CAT Design object
//'     - 'est_history': Estimate History so far
//'     - 'additional_args': Additional arguments
//'   * Each function will return a list with the following named elements:
//'     - 'est_history': Estimate history where the 'item' and 'testlet'
//'          elements of the last step is updated with the selected item's ID's
//'     - 'additional_args': Additional arguments that will be passed to the
//'          next step.
//'   * If exposure control can be applied to item selection algorighm, then,
//'     it should be applied within the item selection sub-function. Exposure
//'     control is not applied item selection within testlets. I
//'     within testlet selection function, exposure control is not applied
//'     to item selection.
//'   * Currently, for testlets, CAT next item selection functions such as
//'     `select_next_item_mepv_cpp`, `select_next_item_random_cpp`,
//'     `select_next_item_fisher_max_info_cpp` first selects a Testlet and 
//'     run "process_testlet_cat_cpp" which selects a testlet item based on 
//'     "testlet_rules". 
//'     After this, each item from the testlet is selected using
//'     `process_testlet_cat_cpp` fuction. This function will be run at the
//'     beginning of each item selection method (except infinite one) and
//'     selects the testlet item separately based on testlet item selection
//'     rules. If testlet termination criteria is satisfied (checked in
//'     `process_testlet_cat_cpp` fuction), the item selection function will
//'     select the next item/testlet using `next_item_selecton` rules.
//'
//'
//'
//' @noRd
//'
//'
//'
//'
// [[Rcpp::export]]
Rcpp::List select_next_item_cpp(Rcpp::List cd, Rcpp::List est_history,
                                Rcpp::List additional_args) {
  // Infinite Item Pool
  if (!cd.containsElementNamed("ip") || Rf_isNull(cd["ip"])) {
    return select_next_item_infinite_ip_cpp(cd, est_history, additional_args);
  }

  // cdi is the "cAT dESIGN iTEM"
  Rcpp::List cd_steps = cd["step"];
  Rcpp::List cdi = cd_steps[est_history.size()-1]; // get last CatDesign step
  
  // get the next item selection method:
  std::string next_item_rule = cdi["next_item_rule"];
  
  if (next_item_rule == "mfi") {
    // Rcout << "select_next_item_cpp -- MFI item selection -- 4" << std::endl;
    return select_next_item_fisher_max_info_cpp(cd, est_history, additional_args);
  } else if (next_item_rule == "mepv") {
    return  select_next_item_mepv_cpp(cd, est_history, additional_args);
  } else if (next_item_rule == "random") {
    return select_next_item_random_cpp(cd, est_history, additional_args);
  } else if (next_item_rule == "fixed") {
    return select_next_item_fixed_cpp(cd, est_history, additional_args);
  } else
    stop("This method has not been implemented yet.");

}


