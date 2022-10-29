#include <Rcpp.h>
#include "itempool_class_methods.h"
#include "response_set_methods.h"
#include "prob.h"
#include "resp_loglik.h"
using namespace Rcpp;


//##############################################################################
//##############################################################################
//########################### irt4PMInfo #######################################
//##############################################################################
//##############################################################################

//##############################################################################
//########################### info_4pm_bare_cpp ################################
//##############################################################################

// [[Rcpp::export]]
double info_4pm_bare_cpp(double theta, Rcpp::S4 item)
{
  // This function calculates the expected information of a single item
  // for one theta.
  std::string model = as<std::string>(item.attr("class"));
  double a = 1, b = as<double>(item.slot("b")), c = 0, d = 1, D = 1;
  if (model != "Rasch") {
    D = as<double>(item.slot("D"));
    if ((model == "2PL") || (model == "3PL") || (model == "4PL")) {
      a = as<double>(item.slot("a"));
      if ((model == "3PL") || (model == "4PL")) {
        c = as<double>(item.slot("c"));
        if (model == "4PL")
          d = as<double>(item.slot("d"));
      }
    }
  }
  return ((D * a)*(D * a) * (d - c)*(d - c)) /
       ((c + d * exp(D * a * (theta - b))) *
       (1 - c + (1-d) * exp(D*a * (theta - b))) *
       (1 + exp(-D * a * (theta - b))) * (1 + exp(-D * a * (theta - b))));
  // return (pow(D * a, 2) * pow(d - c, 2)) /
  //      ((c + d * exp(D * a * (theta - b))) *
  //      (1 - c + (1-d) * exp(D*a * (theta - b))) *
  //      pow(1 + exp(-D * a * (theta - b)), 2));
}


//##############################################################################
//########################### info_grm_bare_cpp ################################
//##############################################################################

// [[Rcpp::export]]
double info_grm_bare_cpp(double theta, Rcpp::S4 item)
{
  // This function calculates the expected information for one item and one
  // theta for Graded Response Model.
  // Calculations use formula on Baker and Kim (2004) p. 226, Eq. 8.23.
  // Item difficulty
  Rcpp::NumericVector b = as<Rcpp::NumericVector>(item.slot("b"));
  // Item discrimination
  double a = as<double>(item.slot("a"));
  double D = as<double>(item.slot("D"));
  // Set the  number of choices
  int no_choices = b.size() + 1;
  double prob_cdf1, prob_cdf2;
  double info = 0;
  prob_cdf1 = 1;
  for(int i = 0; i < no_choices - 1; i++)
  {
    prob_cdf2 = 1 / (1 + exp(-D * a * (theta - b[i])));
    info = info + D*D * a*a * (prob_cdf1 *
      (1-prob_cdf1) - prob_cdf2 * (1-prob_cdf2)) *
      (prob_cdf1 * (1-prob_cdf1) - prob_cdf2 * (1-prob_cdf2)) /
      (prob_cdf1-prob_cdf2);
    // info = info + pow(D, 2) * pow(a, 2) * pow(prob_cdf1 * (1-prob_cdf1) -
    //   prob_cdf2 * (1-prob_cdf2), 2) / (prob_cdf1-prob_cdf2);
    // Rprintf("%d: %f\n", i, info);
    prob_cdf1 = prob_cdf2;
  }
  info = info + D*D * a*a * (prob_cdf1 * (1-prob_cdf1) - 0 * (1-0)) *
    (prob_cdf1 * (1-prob_cdf1) - 0 * (1-0)) / (prob_cdf1-0);
  // info = info + D*D * a*a * pow(prob_cdf1 * (1-prob_cdf1) - 0
  //                                             * (1-0), 2) / (prob_cdf1-0);
  return info;
}

//##############################################################################
//########################### info_gpcm_bare_cpp ###############################
//##############################################################################

// [[Rcpp::export]]
double info_gpcm_bare_cpp(double theta, Rcpp::S4 item)
{
  // This function calculates the expected information for one item and one
  // theta for Generalized Partial Credit Model.
  // Calculations use formula Donoghue (1994), p.299, Eq.4.
  std::string model = as<std::string>(item.attr("class"));

  // Item discrimination, if PCM, set them to 1, else if GPCM get them
  double a = 1;
  double D = 1;
  // Item difficulty
  Rcpp::NumericVector b;

  if (model == "GPCM2") {
    b = as<double>(item.slot("b")) - as<Rcpp::NumericVector>(item.slot("d"));
  } else {
    b = as<Rcpp::NumericVector>(item.slot("b"));
  }

  unsigned int no_choices = b.size() + 1; // Number of categories

  if (model == "GPCM" || model == "GPCM2") {
    a = as<double>(item.slot("a"));
    D = as<double>(item.slot("D"));
  }

  Rcpp::NumericVector P = prob_gpcm_bare_cpp(theta, item, 0);
  double lambda1 = 0;
  double lambda2 = 0;
  for(unsigned int i = 0; i < no_choices; i++)
  {
    lambda1 = lambda1 + i * i * P[i];
    lambda2 = lambda2 + i * P[i];
  }
  return D * D * a * a * (lambda1 - lambda2 * lambda2);
}


//##############################################################################
//########################### info_item_bare_cpp ###############################
//##############################################################################

// [[Rcpp::export]]
double info_item_bare_cpp(double theta, Rcpp::S4 item, bool observed,
                          double resp)
{
  // This function calculates the information of a single item for single theta.
  if (Rcpp::NumericVector::is_na(resp)) return NA_REAL;
  std::string model = as<std::string>(item.attr("class"));
  if (model == "GRM") {
    // TODO: for "GRM" both observed and expected information is the same.
    if (observed) {
      return info_grm_bare_cpp(theta, item);
    } else
      return info_grm_bare_cpp(theta, item);
  } else if (model == "GPCM" || model == "PCM" || model == "GPCM2") {
    if (observed) {
      return -1 * resp_loglik_bare_item_cpp(resp, theta, item, 2);
      //return -1 * resp_loglik_sd_gpcm_bare_cpp(resp, theta, item);
    } else
      return info_gpcm_bare_cpp(theta, item);
  } else {
    // This is default option, i.e. 1PM-4PM, if there are other models
    // add them using 'else if' above.
    if (observed) {
      return -1 * resp_loglik_bare_item_cpp(resp, theta, item, 2);
      // return -1 * resp_loglik_sd_bare_cpp(resp, theta, item);
    } else
      return info_4pm_bare_cpp(theta, item);
  }
  return 0;
}


//##############################################################################
//########################### info_testlet_bare_cpp ############################
//##############################################################################
//' This function calculates the information of a single testlet for single
//' theta. It returns the total information of all items in the testlet. 
//'  
//' @param theta A numeric value at which the information will be calculated. 
//' @param testlet A Testlet object.  
//' @param observed Boolean. If "TRUE", observed information will be calculated  
//' @param resp If 'resp' is not NULL, then it will remove the items' with NA 
//'   from the information calculation.
//'  
//' @return A numeric value for the information of the testlet at the "theta"
//'   value.
//' 
//' @noRd
// [[Rcpp::export]]
double info_testlet_bare_cpp(
  double theta, Rcpp::S4 testlet, bool observed,
  Rcpp::Nullable<Rcpp::NumericVector> resp = R_NilValue) {
  // This function calculates the information of a single testlet for single
  // theta. It returns the total information of all items in the testlet.
  Rcpp::List item_list = as<List>(testlet.slot("item_list"));
  int num_of_items = item_list.size();
  Rcpp::S4 item;
  double output = 0;
  bool return_na = !Rf_isNull(resp);
  // resp is also used to calculate whether to include an item in information
  // calculation. It is not just for 'observed' information.
  if (return_na) {
    // NumericVector resp_i = as<NumericVector>(resp);
    if (as<Rcpp::NumericVector>(resp).size() != num_of_items)
      
      stop("Inadmissible 'resp' value. The length of the " 
           "'resp' and number of items in the testlet should be the same.");
  }
  
  for (int i = 0; i < num_of_items; i++) {
    item = as<Rcpp::S4>(item_list(i));
    if (Rf_isNull(resp)  || !Rcpp::NumericVector::is_na(as<Rcpp::NumericVector>(resp)[i])) {
      output = output + info_item_bare_cpp(theta, item, false, 0);
      return_na = false;
    }
  }
  if (return_na) return NA_REAL;
  return output;
}


//##############################################################################
//########################### info_item_cpp ####################################
//##############################################################################

// [[Rcpp::export]]
Rcpp::NumericVector info_item_cpp(
    Rcpp::NumericVector theta, Rcpp::S4 item, bool observed,
    Rcpp::Nullable<Rcpp::NumericVector> resp = R_NilValue)
{
  // This function calculates the information of a single item for multiple
  // thetas.
  int num_of_theta = theta.size();
  Rcpp::NumericVector output(num_of_theta);
  // Define a new resp_ variable to deal with nullable nature of resp.
  // Rcpp::NumericVector resp_(num_of_theta);
  // Make sure the size of resp is equal to the size of theta.
  if (observed && resp.isNotNull()) {
    // If not null cast resp to the underlying type
    // See: https://stackoverflow.com/a/43391979/2275286
    Rcpp::NumericVector resp_(resp.get());
    if (num_of_theta != resp_.size()) {
      throw std::invalid_argument("The size of the 'resp' vector should be "
                                    "equal to the size of theta.");
    }
    for(int i = 0; i < num_of_theta; i++) {
      output[i] = info_item_bare_cpp(theta[i], item, true, resp_[i]);
    }
    return output;
  }

  for(int i = 0; i < num_of_theta; i++) {
    output[i] = info_item_bare_cpp(theta[i], item, false, 0);
  }
  return output;
}


//##############################################################################
//########################### info_itempool_bare_cpp ###########################
//##############################################################################
//' This function calculates the information of multiple items for a single
//' theta. It returns the information value of each item as a vector.
//' 
//' @noRd

// [[Rcpp::export]]
Rcpp::NumericVector info_itempool_bare_cpp(
    double theta, Rcpp::S4 ip, bool observed = false,
    Rcpp::Nullable<Rcpp::NumericVector> resp = R_NilValue)
{
  // This function calculates the information of multiple items for a single
  // thetas.
  Rcpp::List item_list = as<List>(ip.slot("item_list"));
  Rcpp::IntegerVector ip_size = get_itempool_size(ip);
  int num_of_items = ip_size["elements"];
  unsigned int testlet_size;
  Rcpp::S4 item;
  NumericVector output(num_of_items);
  // an index that tracts the column number to read for the resp vector

  if (Rf_isNull(resp)) {
    for (int i = 0; i < num_of_items; i++) {
      item = as<Rcpp::S4>(item_list(i));
      if (item.inherits("Testlet")) {
        output[i] = info_testlet_bare_cpp(theta, item, false, R_NilValue);
        // if (item.inherits("Item")) {
      } else  {
        output[i] = info_item_bare_cpp(theta, item, false, 0);
      }
    }
  } else {  // Use resp to calculate info, if resp is NA return NA otherwise
            // calculate info
    int resp_index = 0;
    for (int i = 0; i < num_of_items; i++) {
      item = as<Rcpp::S4>(item_list(i));

      if (item.inherits("Testlet")) {
        testlet_size = as<List>(as<S4>(
          item.slot("item_list")).slot("item_list")).size();
        NumericVector resp_ = Rcpp::as<NumericVector>(resp)[Rcpp::Range(
          resp_index, resp_index + testlet_size - 1)];
        output[i] = info_testlet_bare_cpp(theta, item, false, resp_);
        resp_index += testlet_size;
      } else { // if (item.inherits("Item")) {
        output[i] = info_item_bare_cpp(theta, item, false,
                                       as<NumericVector>(resp)[resp_index]);
        resp_index += 1;
      }
    }
  }
  return output;
}


//##############################################################################
//########################### info_itempool_bare_tif_cpp #######################
//##############################################################################
//' This function returns the total information of an itempool for a single
//' theta.
//' @noRd

// [[Rcpp::export]]
double info_itempool_bare_tif_cpp(
    double theta, Rcpp::S4 ip, bool observed = false,
    Rcpp::Nullable<Rcpp::NumericVector> resp = R_NilValue)
{
  Rcpp::NumericVector item_info = info_itempool_bare_cpp(theta, ip, observed,
                                                         resp);
  int num_of_items = item_info.length();
  double output = 0;
  for (int i = 0; i < num_of_items; i++) {
    if (!R_IsNA(item_info[i]))
      output = output + item_info[i];
  }
  return output;
}


//##############################################################################
//########################### info_itempool_cpp ################################
//##############################################################################
//' This function calculates the information of multiple items for multiple
//' thetas.
//' @noRd

// [[Rcpp::export]]
Rcpp::NumericMatrix info_itempool_cpp(
  Rcpp::NumericVector theta, Rcpp::S4 ip, bool observed = false,
  Rcpp::Nullable<Rcpp::NumericMatrix> resp = R_NilValue)
{
  int num_of_cols = as<List>(ip.slot("item_list")).size();
  int num_of_theta = theta.size();
  Rcpp::NumericMatrix output(num_of_theta, num_of_cols);
  if (Rf_isNull(resp)) {
    for(int i = 0; i < num_of_theta; i++) {
		output(i, Rcpp::_) = info_itempool_bare_cpp(theta[i], ip);
	}
  } else {
    Rcpp::NumericMatrix resp_ = Rcpp::as<NumericMatrix>(resp);
    Rcpp::NumericVector resp_row;
    for(int i = 0; i < num_of_theta; i++) {
      resp_row = resp_(i, Rcpp::_);
	  output(i, Rcpp::_) = info_itempool_bare_cpp(theta[i], ip, observed, 
	                                              resp_row);
    }
  }
  // // Consider Testlets etc.
  // // Set the column names to the item ids
  colnames(output) = get_ids_itempool_cpp(ip);

  return output;
}


//##############################################################################
//########################### info_itempool_tif_cpp ############################
//##############################################################################
//' This function calculates the total test information of multiple items for 
//' multiple thetas.
//' @noRd

// [[Rcpp::export]]
Rcpp::NumericVector info_itempool_tif_cpp(
  Rcpp::NumericVector theta, Rcpp::S4 ip, bool observed = false,
  Rcpp::Nullable<Rcpp::NumericMatrix> resp = R_NilValue)
{
  int num_of_theta = theta.size();
  Rcpp::NumericVector output(num_of_theta);
  if (Rf_isNull(resp)) {
    for(int i = 0; i < num_of_theta; i++) {
	  output[i] = info_itempool_bare_tif_cpp(theta[i], ip);			
	}
  } else {
    Rcpp::NumericMatrix resp_ = Rcpp::as<NumericMatrix>(resp);
    Rcpp::NumericVector resp_row;
    for(int i = 0; i < num_of_theta; i++) {
      resp_row = resp_(i, Rcpp::_);
	  output[i] = info_itempool_bare_tif_cpp(theta[i], ip, observed, resp_row);			
    }
  }
  return output;
}


//##############################################################################
//########################### info_response_cpp ################################
//##############################################################################
//' This function is only used when resp is not NULL and resp is a Response
//' object.
//' @noRd

// [[Rcpp::export]]
Rcpp::NumericVector info_response_cpp(
  double theta,
  Rcpp::S4 ip,
  Rcpp::S4 resp, 
  bool observed = false)
{
  if (!resp.inherits("Response"))
    stop("Invalid 'resp'. 'resp' should be a 'Response' object.");
  Rcpp::List ip_list = flatten_itempool_cpp(ip);	
  Rcpp::NumericVector scores = as<Rcpp::NumericVector>(resp.slot("score"));
  Rcpp::StringVector item_ids = as<Rcpp::StringVector>(resp.slot("item_id"));
  Rcpp::StringVector ip_item_ids = ip_list.names();
  int num_of_items_ip = ip_list.size();
  int num_of_items = scores.size();
  Rcpp::NumericVector output(num_of_items_ip, NA_REAL);
  output.attr("names") = ip_item_ids;
  //Rcpp::NumericVector output_tif(1);


  Rcpp::S4 item; // This will be item
  std::string item_id;
  for (int i = 0; i < num_of_items; i++) {
    item_id = item_ids[i];
    item = as<Rcpp::S4>(ip_list[item_id]);
    output[item_id] = info_item_bare_cpp(theta, item, observed, scores[i]);
    //output_tif[0] = output_tif[0] + output[item_id];
  }
  return output;
}


//##############################################################################
//########################### info_response_tif_cpp ############################
//##############################################################################
//' This function is only used when resp is not NULL and resp is a Response
//' object to calculate Total test information.
//' @noRd

// [[Rcpp::export]]
double info_response_tif_cpp(
  double theta,
  Rcpp::S4 ip, 
  Rcpp::S4 resp, 
  bool observed = false)
{
  if (!resp.inherits("Response"))
    stop("Invalid 'resp'. 'resp' should be a 'Response' object.");
  
  Rcpp::NumericVector item_info = info_response_cpp(theta, ip, resp, observed);
  int num_of_items = item_info.length();
  double output = 0;
  for (int i = 0; i < num_of_items; i++) {
	if (!R_IsNA(item_info[i])) output = output + item_info[i];
  }
  return output;
}


//##############################################################################
//########################### info_response_set_cpp ############################
//##############################################################################

// [[Rcpp::export]]
Rcpp::NumericMatrix info_response_set_cpp(
    Rcpp::NumericVector theta,
    Rcpp::S4 ip,
    Rcpp::S4 resp_set,
    bool tif = false,
    bool observed = false)
{
  // Make sure resp_set and ip are valid and compatible
  check_validity_response_set_cpp(resp_set, ip);
  Rcpp::List ip_list = flatten_itempool_cpp(ip);
  int num_of_items = 1;
  if (!tif) num_of_items = ip_list.size();
  // Rcpp::StringVector ip_item_ids(num_of_items);
  // for (int i = 0; i < num_of_items; i++) {
  //   ip_item_ids[i] = as<Rcpp::S4>(ip_list[i])
  // }

  Rcpp::List resp_list = as<Rcpp::List>(resp_set.slot("response_list"));
  int num_of_resp = resp_list.size();

  if (theta.size() != num_of_resp)
    stop("Incompatible 'theta' and 'resp_set'. Their length should be equal.");

  Rcpp::NumericMatrix output(num_of_resp, num_of_items);
  Rcpp::S4 temp_resp;
  for (int i = 0; i < num_of_resp; i++) {
    temp_resp = as<Rcpp::S4>(resp_list[i]);
	if (tif) {
	  output(i, 0) = info_response_tif_cpp(theta[i], ip, temp_resp, observed);	
	} else {
      output(i, Rcpp::_) = info_response_cpp(theta[i], ip, temp_resp, observed);
	}
  }
  if (tif) {
    colnames(output) = Rcpp::StringVector::create("TIF");
  } else {
    colnames(output) = as<Rcpp::StringVector>(ip_list.names());
  }

  rownames(output) = get_examinee_id_response_set_cpp(resp_set);
  return output;

}
