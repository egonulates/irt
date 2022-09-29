#include <Rcpp.h>
#include "misc.h"
#include "itempool_class_methods.h"
#include "prob.h"
using namespace Rcpp;



//#############################################################################@
//########################### find_icc_intersect_cpp ###########################
//#############################################################################@
//' @title Find the intersection of ICCs
//'
//' @description This function finds the points on theta scale where two item
//'   item characteristic curves intersects between two theta points (theta
//'   range). If they overlap or do not intersect, the function will return an
//'   empty vector, \code{integer(0)}.
//'   Only available for 'Rasch', '1PL', '2PL', '3PL' or '4PL' models.
//'
//' @noRd
//'

// [[Rcpp::export]]
Rcpp::NumericVector find_icc_intersect_cpp(
    Rcpp::S4 item_1,
    Rcpp::S4 item_2,
    Rcpp::NumericVector theta_range = Rcpp::NumericVector::create(-5, 5)) {
  std::string model_1 = as<std::string>(item_1.attr("class"));
  std::string model_2 = as<std::string>(item_2.attr("class"));

  if (!check_item_model(item_1, true, true) ||
      !check_item_model(item_2, true, true)) {
  // if (!(model_1 == "Rasch" || model_1 == "1PL"  || model_1 == "2PL" ||
  //   model_1 == "3PL" || model_1 == "4PL") ||
  //   !(model_2 == "Rasch" || model_2 == "1PL"  || model_2 == "2PL" ||
  //   model_2 == "3PL" || model_2 == "4PL")) {
    stop("Invalid model. Both of the items should be either 'Rasch', '1PL', '2PL', '3PL' or '4PL'");
  }

  // below is basically: 'theta <- seq(from = -5, to = 5, length.out = 20)"
  int num_of_bins = 41;
  double increment = (theta_range[1] - theta_range[0]) / (num_of_bins - 1);
  // if (increment > 0.25) increment = 0.25;
  Rcpp::NumericVector theta(num_of_bins, theta_range[0]);
  for (int i = 1; i < num_of_bins; i++) {
    theta[i] = theta[i-1] + increment;
  }
  Rcpp::NumericVector p1 = prob_4pm_item_cpp(theta, item_1, 0);
  Rcpp::NumericVector p2 = prob_4pm_item_cpp(theta, item_2, 0);

  // Find the number of intersections
  int num_of_intersections= 0; // the number of intersections betwen two ICCs
  unsigned int num_of_theta = theta.size();
  Rcpp::NumericVector interval_low = {-999, -999};
  Rcpp::NumericVector interval_high = {-999, -999};

  // -1: (p1-p2 < 0) ; 0: (p1 == p2) ; 1: (p1-p2 > 0)
  int previous_sign, current_sign;
  if (p1[0] - p2[0] > 0) {
    previous_sign = 1;
  } else if (p1[0] == p2[0]) {
    previous_sign = 0;
  } else previous_sign = -1;

  for (unsigned int i = 1; i < num_of_theta; i++) {
    // Rcout << "i = " << i << " -  previous_sign = " << previous_sign <<
    //   " - theta = " << theta[i-1] << "  - p1 = " << p1[i] << "  - p2 = " <<
    //     p2[i] << std::endl;
    if (p1[i] - p2[i] > 0) {
      current_sign = 1;
    } else if (p1[i] == p2[i]) {
      current_sign = 0;
    } else current_sign = -1;
    if (current_sign == previous_sign) {
      continue;
    } else {
      interval_low[num_of_intersections] = theta[i-1];
      interval_high[num_of_intersections] = theta[i];
      num_of_intersections += 1;
      // Rcout << "num_of_intersections " << num_of_intersections << " - " <<
      //   theta[i] << theta[i-1] << std::endl;
    }
    previous_sign = current_sign;
  }

  double tol = 0.0001;
  double diff;
  Rcpp::NumericVector result(num_of_intersections);
  double t0, t1, f, df;
  for (int i = 0; i < num_of_intersections; i++) {
    diff = 999;
    t1 = (interval_low[i] + interval_high[i]) / 2; // starting theta
    while (diff > tol) {
      t0 = t1;
      f = prob_4pm_bare_cpp(t0, item_1, 0) - prob_4pm_bare_cpp(t0, item_2, 0);
      df = prob_4pm_bare_cpp(t0, item_1, 1) - prob_4pm_bare_cpp(t0, item_2, 1);
      t1 = t0 - (f/df);
      diff = std::fabs(t0 - t1);
    }
    result[i] = t1;
  }
  return result;
}


//#############################################################################@
//########################### area_between_icc_exact_cpp #######################
//#############################################################################@
//' @title Find the exact area between two ICCs
//'
//' @description This function uses Raju (1988) formulas and finds the exact
//'   area between two ICCs
//'
//' @noRd
//'

// [[Rcpp::export]]
double area_between_icc_exact_cpp(
    Rcpp::S4 item_1,
    Rcpp::S4 item_2,
    bool signed_area = true) {

  std::string model_1 = as<std::string>(item_1.attr("class"));
  std::string model_2 = as<std::string>(item_2.attr("class"));
  if (!(model_1 == "Rasch" || model_1 == "1PL"  || model_1 == "2PL" ||
    model_1 == "3PL") ||
    !(model_2 == "Rasch" || model_2 == "1PL"  || model_2 == "2PL" ||
    model_2 == "3PL")) {
    stop("Invalid model. Both of the items should be either 'Rasch', '1PL', '2PL', '3PL'");
  }
  double result = -1;

  // // Rasch and 1PL Models
  // if ((model_1 == "Rasch" || model_1 == "1PL") &&
  //     (model_2 == "Rasch" || model_2 == "1PL")) {
  //   double b1 =  as<double>(item_1.slot("b"));
  //   double b2 =  as<double>(item_2.slot("b"));
  //   result = b2 - b1;
  //   if (signed_area) {
  //     return result;
  //   } else return std::fabs(result);
  // }

  double a1 = 1, a2 = 1, D1 = 1, D2 = 1, c1 = 0, c2 = 0;
  double b1 =  as<double>(item_1.slot("b"));
  double b2 =  as<double>(item_2.slot("b"));
  if (model_1 != "Rasch") {
    D1 = as<double>(item_1.slot("D"));
    if ((model_1 == "2PL") || (model_1 == "3PL")) {
      a1 = as<double>(item_1.slot("a"));
      if (model_1 == "3PL") c1 = as<double>(item_1.slot("c"));
    }
  }
  if (model_2 != "Rasch") {
    D2 = as<double>(item_2.slot("D"));
    if ((model_2 == "2PL") || (model_2 == "3PL")) {
      a2 = as<double>(item_2.slot("a"));
      if (model_2 == "3PL") c2 = as<double>(item_2.slot("c"));
    }
  }
  if (D1 != D2) {
    stop("Scaling constants of the items should be equal to each other.");
  }

  if (a1 == a2 && c1 == c2) {
    result = (1 - c1) * (b2 - b1);
    if (signed_area) {
      return result;
    } else return std::fabs(result);
  }

  if (c1 == c2) {
    if (signed_area) {
      return (1 - c1) * (b2 - b1);
    } else {
      return (1 - c1) * std::fabs( ((2 * (a2 - a1)) / (D1 * a1 * a2)) *
              log(1 + exp((D1 * a1 * a2 * (b2 - b1)) / (a2 - a1))) - (b2 - b1));
    }
  } else {
    return std::numeric_limits<double>::infinity();
  }

  return result;
}




//#############################################################################@
//########################### area_under_icc_closed_icc ########################
//#############################################################################@

//' @title Find the area under an ICC enclosed by boundaries
//'
//' @description This method implements Kim and Cohen (1991). Only works for
//'   Rasch, 1PL, 2PL and 3PL models.
//' @noRd

double area_under_icc_closed_icc(double a, double b, double c, double d,
                                 double D, double theta_low, double theta_high) {
  return (c * (theta_high - theta_low) + (d - c) * (1 / (D * a)) *
    log((1 + exp(D * a * (theta_high - b))) /
        (1 + exp(D * a * (theta_low - b)))));
}



//#############################################################################@
//########################### area_between_icc_closed_cpp ######################
//#############################################################################@
//' @title Find the area between two ICC within a closed interval
//'
//' @description This method implements Kim and Cohen (1991).
//'   Only available for 'Rasch', '1PL', '2PL', '3PL' or '4PL' models.
//' @noRd

// [[Rcpp::export]]
double area_between_icc_closed_cpp(
    Rcpp::S4 item_1,
    Rcpp::S4 item_2,
    bool signed_area = true,
    Rcpp::NumericVector theta_range = Rcpp::NumericVector::create(-5, 5)) {
  std::string model_1 = as<std::string>(item_1.attr("class"));
  std::string model_2 = as<std::string>(item_2.attr("class"));
  if (!check_item_model(item_1, true, true) || !check_item_model(item_2, true, true)) {
    stop("Invalid model. Both of the items should be either 'Rasch', '1PL', '2PL', '3PL' or '4PL'");
  }
  // if (!(model_1 == "Rasch" || model_1 == "1PL"  || model_1 == "2PL" ||
  //   model_1 == "3PL" || model_1 == "4PL") ||
  //   !(model_2 == "Rasch" || model_2 == "1PL"  || model_2 == "2PL" ||
  //   model_2 == "3PL" || model_2 == "4PL")) {
  //   stop("Invalid model. Both of the items should be either 'Rasch', '1PL', '2PL', '3PL' or '4PL'");
  // }

  double a1 = 1, a2 = 1, D1 = 1, D2 = 1, c1 = 0, c2 = 0, d1 = 1, d2 = 1;
  double b1 = as<double>(item_1.slot("b"));
  double b2 = as<double>(item_2.slot("b"));
  if (model_1 != "Rasch") {
    D1 = as<double>(item_1.slot("D"));
    if ((model_1 == "2PL") || (model_1 == "3PL") || model_1 == "4PL") {
      a1 = as<double>(item_1.slot("a"));
      if (model_1 == "3PL" || model_1 == "4PL") {
        c1 = as<double>(item_1.slot("c"));
        if (model_1 == "4PL") d1 = as<double>(item_1.slot("d"));
      }
    }
  }

  if (model_2 != "Rasch") {
    D2 = as<double>(item_2.slot("D"));
    if ((model_2 == "2PL") || (model_2 == "3PL") || model_2 == "4PL") {
      a2 = as<double>(item_2.slot("a"));
      if (model_2 == "3PL" || model_2 == "4PL") {
        c2 = as<double>(item_2.slot("c"));
        if (model_2 == "4PL") d2 = as<double>(item_2.slot("d"));
      }
    }
  }

  Rcpp::NumericVector icc_intersections = find_icc_intersect_cpp(
    item_1, item_2,theta_range);
  double result = std::numeric_limits<double>::infinity();
  double area1, area2;
  if (signed_area) {
    area1 = area_under_icc_closed_icc(a1, b1, c1, d1, D1, theta_range[0],
                                      theta_range[1]);
    area2 = area_under_icc_closed_icc(a2, b2, c2, d2, D2, theta_range[0],
                                      theta_range[1]);
    return (area1 - area2);
  } else { // one or more instersections and unsigned area
    double temp_theta, p1, p2;
    icc_intersections.push_front(theta_range[0]);
    icc_intersections.push_back(theta_range[1]);
    result = 0;
    for (int i = 0; i < (icc_intersections.size() - 1); i++) {
      temp_theta = (icc_intersections[i+1] + icc_intersections[i]) / 2;
      p1 = prob_4pm_bare_cpp(temp_theta, item_1, 0);
      p2 = prob_4pm_bare_cpp(temp_theta, item_2, 0);
      area1 = area_under_icc_closed_icc(a1, b1, c1, d1, D1,
                                        icc_intersections[i],
                                        icc_intersections[i+1]);
      area2 = area_under_icc_closed_icc(a2, b2, c2, d2, D2,
                                        icc_intersections[i],
                                        icc_intersections[i+1]);
      // Rcout << i << ": theta1: " << icc_intersections[i] << " - theta2: " <<
      //   icc_intersections[i+1] << " -  temp_theta = " << temp_theta <<
      //     " - Area1 = " << area1  << " - Area2 = " << area2 <<
      //     " - p1 = " << p1  << " - p2 = " << p2 << std::endl;
      if (p1 >= p2) {
        result += (area1 - area2);
      } else {
        result += (area2 - area1);
      }
    }
  }
  return result;
}




