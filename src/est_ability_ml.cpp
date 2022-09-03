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
//########################### est_ability_4pm_nr ###############################
//#############################################################################@
//#############################################################################@

//#############################################################################@
//########################### est_ability_4pm_nr_itempool_single_iv_cpp ########
//#############################################################################@
double est_ability_4pm_nr_itempool_single_iv_cpp(
  Rcpp::NumericVector resp,
  Rcpp::S4 ip,
  Rcpp::NumericVector theta_range = Rcpp::NumericVector::create(-5, 5),
  double initial_est = 0,
  double criterion = 0.001) {

  double est = initial_est;
  double difference = criterion + 1;
  double firstDerivative, secondDerivative, adjustment, newEst;
  double minEst = theta_range[0];
  double maxEst = theta_range[1];

  if ((initial_est <= minEst) || (initial_est >= maxEst)) {
    est = 0;
  }
  while (difference > criterion) {
    firstDerivative = resp_loglik_bare_itempool_cpp(resp, est, ip, 1);
    secondDerivative = resp_loglik_bare_itempool_cpp(resp, est, ip, 2);

    // fabs = std::abs() function that calculates the absolute value.
    // -fabs(secondDerivative) is an adjustment to make algorithm to
    // the move correct direction
    adjustment = firstDerivative / (-fabs(secondDerivative));
    // Limit the move size by .5.
    if (fabs(adjustment) < .5) {
      newEst = est - adjustment;
    } else {
      newEst = est - 0.5 * ((adjustment > 0) - (adjustment < 0));
    }
    difference = fabs(newEst - est);
    // If estimate is out of bounds it will bound it.
    if ((newEst <= minEst) || (newEst >= maxEst)) {
      if ((est <= minEst) || (est >= maxEst))
      {
        if (est <= minEst) {
          return(minEst);
        } else {
          return(maxEst);
        }
      }
    }
    est = newEst;
  }
  return est;
}

//#############################################################################@
//########################### est_ability_4pm_nr_itempool_cpp ##################
//#############################################################################@
// [[Rcpp::export]]
double est_ability_4pm_nr_itempool_cpp(
  Rcpp::NumericVector resp,
  Rcpp::S4 ip,
  Rcpp::NumericVector theta_range = Rcpp::NumericVector::create(-5, 5),
  double criterion = 0.001,
  Rcpp::Nullable<Rcpp::NumericVector> initial_estimates = R_NilValue) {

  // Rcout << "est_ability_ml_nr - Stage 1" << std::endl;
  Rcpp::NumericVector init_est(3); // vector holding initial values
  double est; // Final estimate value that will be returned
  // Vector that holds estimates obtained using different initial values.
  bool all_equal = true;

  // Set the initial values if a NULL value provided
  if (initial_estimates.isNotNull()) {
    init_est = as<Rcpp::NumericVector>(initial_estimates);
  } else {
    init_est[0] = theta_range[0] + 2 * criterion;
    init_est[1] = 0;
    init_est[2] = theta_range[1] - 2 * criterion;
    // init_est = {theta_range[0] + 2 * criterion, 0,
    //             theta_range[1] - 2 * criterion};
  }
  int n_init_values = init_est.size();
  // Vector that holds estimates obtained using different initial values.
  Rcpp::NumericVector estimates(n_init_values);

  // Rcout << "est_ability_ml_nr - Stage 2" << std::endl;

  // Make sure that init_est has at least two values:
  if (init_est.size() < 2)
    stop("Please proivde at least two different initial values.");
  // Find ML ability estimates based on different initial values.

  for(int i = 0; i < n_init_values; i++) {
    estimates[i] = est_ability_4pm_nr_itempool_single_iv_cpp(
      resp, ip, theta_range, init_est[i], criterion);
    if (i > 0 && (std::fabs(estimates[i] - estimates[i-1]) >
                    n_init_values*criterion))
      all_equal = false;
    // Rcout << i << " - " << estimates[i] << std::endl;
  }
  est = estimates[0];
  // Rcout << "est_ability_ml_nr - Stage 5 - " << all_equal << " ; est = " <<
  // est << std::endl;

  // Check whether all elements of the vector are equal, if not return the
  // estimate value with the highest response log-likelihood
  if (!all_equal) {
    //Rcout << "est_ability_ml_nr - Stage 5.1 - Difference found!" << std::endl;

    // Calculate response log likelihood value at the estimate that was
    // derived from first initial estimate.
    double resp_ll = 0;
    double resp_ll_max = resp_loglik_bare_itempool_cpp(resp, estimates[0], ip,
                                                       0);

    for(int i = 0; i < n_init_values; i++) {
      // Rcout << "est_ability_ml_nr - Stage 5.2" << std::endl;
      resp_ll = resp_loglik_bare_itempool_cpp(resp, estimates[i], ip, 0);
      if (resp_ll > resp_ll_max) {
        est = estimates[i];
        resp_ll_max = resp_ll;
      }
    }
  }
  // Rcout << "est_ability_ml_nr - Stage 7 - " << est << std::endl;
  return(est);
}

//#############################################################################@
//########################### est_ability_4pm_nr_iv_response_cpp ###############
//#############################################################################@
//' This function estimates ability using Newton-Raphson method for a Response
//' object.
//' @noRd
//'
double est_ability_4pm_nr_iv_response_cpp(
    Rcpp::S4 resp,
    Rcpp::S4 ip,
    Rcpp::NumericVector theta_range = NumericVector::create(-5, 5),
    double initial_est = 0,
    double criterion = 0.001) {
  double est = initial_est;
  double difference = criterion + 1;
  double firstDerivative, secondDerivative, adjustment, newEst;
  double minEst = theta_range[0];
  double maxEst = theta_range[1];

  // Rcout << "  est_ability_ml_nr_iv - Stage 1" << std::endl;

  if ((initial_est <= minEst) || (initial_est >= maxEst)) {
    est = 0;
  }
  while (difference > criterion) {

    // Rcout << "  est_ability_ml_nr_iv - Stage 1.1" << std::endl;

    firstDerivative = resp_loglik_response_cpp(est, resp, ip, 1);
    secondDerivative = resp_loglik_response_cpp(est, resp, ip, 2);

    // fabs = std::abs() function that calculates the absolute value.
    // -fabs(secondDerivative) is an adjustment to make algorithm to
    // the move correct direction
    adjustment = firstDerivative / (-fabs(secondDerivative));
    // Limit the move size by .5.
    if (fabs(adjustment) < .5) {
      newEst = est - adjustment;
    } else {
      newEst = est - 0.5 * ((adjustment > 0) - (adjustment < 0));
    }
    difference = fabs(newEst - est);

    // Rcout << "  est_ability_ml_nr_iv - Stage 1.2 - FD = " <<
    //   firstDerivative << " ; SD = " << secondDerivative <<
    //     " ; Adj = " << adjustment << " ; est = " << est << " ; newEst = "
    //     << newEst << " ; diff = " << difference << std::endl;

    // If estimate is out of bounds it will bound it.
    if ((newEst <= minEst) || (newEst >= maxEst)) {

      // Rcout << "  est_ability_ml_nr_iv - Stage 1.2 - "<< difference <<
      //   std::endl;

      if ((est <= minEst) || (est >= maxEst))
      {

        // Rcout << "  est_ability_ml_nr_iv - Stage 1.3" << std::endl;

        if (est <= minEst) {
          return(minEst);
        } else {
          return(maxEst);
        }
      }
    }
    est = newEst;
  }
  return est;
}


//#############################################################################@
//########################### est_ability_4pm_nr_response_cpp ##################
//#############################################################################@
// [[Rcpp::export]]
double est_ability_4pm_nr_response_cpp(
    Rcpp::S4 resp,
    Rcpp::S4 ip,
    Rcpp::NumericVector theta_range = NumericVector::create(-5, 5),
    double criterion = 0.001,
    Rcpp::Nullable<Rcpp::NumericVector> initial_estimates = R_NilValue) {
  // Rcout << "est_ability_ml_nr - Stage 1" << std::endl;
  Rcpp::NumericVector init_est(3); // vector holding initial values
  double est; // Final estimate value that will be returned
  // Vector that holds estimates obtained using different initial values.
  bool all_equal = true;

  // resp_loglik_response_cpp functions require 'Response' objects to have
  // valid item_id slot.
  if (resp.slot("item_id") == R_NilValue)
    throw Rcpp::exception("Invalid 'resp'. 'resp' should have valid "
                            "'item_id' slot.", false);

  // Set the initial values if a NULL value provided
  if (initial_estimates.isNotNull()) {
    init_est = as<Rcpp::NumericVector>(initial_estimates);
  } else {
    init_est[0] = theta_range[0] + 2 * criterion;
    init_est[1] = 0;
    init_est[2] = theta_range[1] - 2 * criterion;
    // init_est = {theta_range[0] + 2 * criterion, 0,
    //             theta_range[1] - 2 * criterion};
  }
  int n_init_values = init_est.size();
  // Vector that holds estimates obtained using different initial values.
  Rcpp::NumericVector estimates(n_init_values);

  // Rcout << "est_ability_ml_nr - Stage 2" << std::endl;

  // Make sure that init_est has at least two values:
  if (init_est.size() < 2)
    stop("Please proivde at least two different initial values.");
  // Find ML ability estimates based on different initial values.

  // Rcpp::List ip_list = flatten_itempool_cpp(ip);
  for(int i = 0; i < n_init_values; i++) {
    estimates[i] = est_ability_4pm_nr_iv_response_cpp(
      resp, ip, theta_range, init_est[i], criterion);
    if (i > 0 && (std::fabs(estimates[i] - estimates[i-1]) >
                    n_init_values*criterion))
      all_equal = false;
    // Rcout << i << " - " << estimates[i] << std::endl;
  }
  est = estimates[0];
  // Rcout << "est_ability_ml_nr - Stage 5 - " << all_equal << " ; est = " <<
  // est << std::endl;

  // Check whether all elements of the vector are equal, if not return the
  // estimate value with the highest response log-likelihood
  if (!all_equal) {
    //Rcout << "est_ability_ml_nr - Stage 5.1 - Difference found!" << std::endl;

    // Calculate response log likelihood value at the estimate that was
    // derived from first initial estimate.
    double resp_ll = 0;
    double resp_ll_max = resp_loglik_response_cpp(estimates[0], resp, ip, 0);

    for(int i = 0; i < n_init_values; i++) {
      // Rcout << "est_ability_ml_nr - Stage 5.2" << std::endl;
      resp_ll = resp_loglik_response_cpp(estimates[i], resp, ip, 0);
      if (resp_ll > resp_ll_max) {
        est = estimates[i];
        resp_ll_max = resp_ll;
      }
    }
  }
  // Rcout << "est_ability_ml_nr - Stage 7 - " << est << std::endl;
  return(est);
}





// //##########################################################################@
// //########################### est_ability_4pm_nr_itempool_cpp ###############
// //##########################################################################@
// // [[Rcpp::export]]
// double est_ability_4pm_nr_itempool_cpp(NumericMatrix resp, Rcpp::S4 ip,
//                                         double initialEst, double criterion,
//                                         double minEst, double maxEst)
// {
//   // This function is replacement of estNR function. Does exactly the same.
//   // Estimates ability using Newton-Raphson for just one theta.
//   double est = initialEst;
//   double difference = criterion + 1;
//   NumericVector tempVector(1);
//   NumericVector tempEst(1);
//   double firstDerivative, secondDerivative, adjustment, newEst;
//
//   if ((initialEst <= minEst) || (initialEst >= maxEst)) {
//     est = 0;
//   }
//   while (difference > criterion)
//   {
//     tempEst[0] = est;
//     tempVector = resp_loglik_fd_itempool_cpp(resp, tempEst, ip);
//     firstDerivative = tempVector[0];
//
//     tempEst[0] = est;
//     tempVector = resp_loglik_sd_itempool_cpp(resp, tempEst, ip);
//     secondDerivative = tempVector[0];
//
//     // -fabs(secondDerivative) is an adjustment to make algorithm to
//     // the move correct direction
//     adjustment = firstDerivative / (-fabs(secondDerivative));
//     // Limit the move size by .5.
//     if (fabs(adjustment) < .5) {
//       newEst = est - adjustment;
//     } else {
//       newEst = est - 0.5 * ((adjustment > 0) - (adjustment < 0));
//     }
//     difference = fabs(newEst - est);
//     // If estimate is out of bounds it will bound it.
//     if ((newEst <= minEst) || (newEst >= maxEst)) {
//       if ((est <= minEst) || (est >= maxEst))
//       {
//         if (est <= minEst) {
//           return(minEst);
//         } else {
//           return(maxEst);
//         }
//       }
//     }
//     est = newEst;
//   }
//   return est;
// }


// //##########################################################################@
// //########################### est_ability_4pm_nr_matrix_cpp #################
// //##########################################################################@
// // [[Rcpp::export]]
// double est_ability_4pm_nr_matrix_cpp(
//     NumericMatrix resp, NumericMatrix ip,
//     double initialEst, double criterion, double minEst,
//     double maxEst)
// {
//   // This function is replacement of estNR function. Does exactly the same.
//   // Estimates ability using Newton-Raphson for just one theta.
//   double est = initialEst;
//   double difference = criterion + 1;
//   NumericVector tempVector(1);
//   NumericVector tempEst(1);
//   double firstDerivative, secondDerivative, adjustment, newEst;
//
//   if ((initialEst <= minEst) || (initialEst >= maxEst)) {
//     est = 0;
//   }
//   while (difference > criterion)
//   {
//     tempEst[0] = est;
//     tempVector = resp_loglik_fd_4pm_matrix_cpp(resp, tempEst, ip);
//     firstDerivative = tempVector[0];
//
//     tempEst[0] = est;
//     tempVector = resp_loglik_sd_4pm_matrix_cpp(resp, tempEst, ip);
//     secondDerivative = tempVector[0];
//
//     // -fabs(secondDerivative) is an adjustment to make algorithm to
//     // the move correct direction
//     adjustment = firstDerivative / (-fabs(secondDerivative));
//     // Limit the move size by .5.
//     if (fabs(adjustment) < .5) {
//       newEst = est - adjustment;
//     } else {
//       newEst = est - 0.5 * ((adjustment > 0) - (adjustment < 0));
//     }
//     difference = fabs(newEst - est);
//     // If estimate is out of bounds it will bound it.
//     if ((newEst <= minEst) || (newEst >= maxEst)) {
//       if ((est <= minEst) || (est >= maxEst))
//       {
//         if (est <= minEst) {
//           return(minEst);
//         } else {
//           return(maxEst);
//         }
//       }
//     }
//     est = newEst;
//   }
//   return est;
// }



//#############################################################################@
//#############################################################################@
//########################### est_ability_optim ################################
//#############################################################################@
//#############################################################################@

//#############################################################################@
//########################### est_ability_optim_response_cpp ###################
//#############################################################################@
//' @param resp A Response object.
//' @param ip_list A list of Item/Testlet objects.
//' @noRd

// [[Rcpp::export]]
double est_ability_optim_response_cpp(
    Rcpp::S4 resp,
    Rcpp::List ip_list,
    Rcpp::NumericVector theta_range = Rcpp::NumericVector::create(-5, 5),
    double tol = 0.0000001) {

  // Extract R's optim function
  Rcpp::Environment stats("package:stats");
  Rcpp::Function optim = stats["optim"];

  // Call the optim function from R in C++
  Rcpp::List opt_results = optim(
    Rcpp::_["par"]    = Rcpp::NumericVector::create(0),
    // Make sure this function is not exported!
    Rcpp::_["fn"]     = Rcpp::InternalFunction(&resp_loglik_response_cpp),
    Rcpp::_["method"] = "Brent",
    Rcpp::_["lower"] = Rcpp::NumericVector::create(-5),
    Rcpp::_["upper"] = Rcpp::NumericVector::create(5),
    // Pass in the other parameters as everything
    // is scoped environmentally
    Rcpp::_["resp"] = resp,
    Rcpp::_["ip_list"] = ip_list,
    Rcpp::_["derivative"] = 0
    );

  return Rcpp::as<double>(opt_results[0]);
}


