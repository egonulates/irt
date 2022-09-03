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
//########################### est_ability_owen #################################
//#############################################################################@
//#############################################################################@

//#############################################################################@
//########################### est_ability_owen_item_cpp ########################
//#############################################################################@
// [[Rcpp::export]]
Rcpp::List est_ability_owen_item_cpp(Rcpp::S4 item, int resp,
                                     double m0, double v0) {

  // Item difficulty
  double b = as<double>(item.slot("b"));
  // Item discrimination
  double D = item.hasSlot("D") ? as<double>(item.slot("D")) : 1;
  double a = item.hasSlot("a") ? as<double>(item.slot("a")) : 1;
  a = a/D;
  double c = item.hasSlot("c") ? as<double>(item.slot("c")) : 0;
  double d = item.hasSlot("d") ? as<double>(item.slot("d")) : 1;
  // DD is the D in original owen's article. Here I rename it in case of
  // a confusion with scaling parameter D, 1.7.
  double DD = (b - m0) / sqrt((1/(a*a)) + v0);
  double dnormDD, pnormDD;
  NumericVector temp_nv;
  NumericVector DD_vector = NumericVector::create(DD);
  temp_nv = pnorm(DD_vector);
  pnormDD = temp_nv[0];
  temp_nv = dnorm(DD_vector);
  dnormDD = temp_nv[0];
  DD_vector[0] = -DD;
  temp_nv = pnorm(DD_vector);
  double pnormDD_minus = temp_nv[0];
  double A = c + (d - c) * pnormDD_minus;
  double m1 = m0 - v0 * (1/sqrt((1/(a*a)) + v0)) * (dnormDD / pnormDD) * (
    1 - resp / A);
  // double m1 = m0 - v0 * pow(pow(a, -2) + v0, -.5) * (dnormDD / pnormDD) * (
  //   1 - resp / A);
  double v1 = v0 - v0*v0 * (1/((1/(a*a)) + v0)) * (
    dnormDD / pnormDD) * (1 - resp / A) * ((dnormDD / pnormDD) * (
        1 - resp / A) + DD);
  // double v1 = v0 - pow(v0, 2) * pow(pow(a, -2) + v0, -1) * (
  //   dnormDD / pnormDD) * (1 - resp / A) * ((dnormDD / pnormDD) * (
  //       1 - resp / A) + DD);
  List output;
  output["m1"] = m1;
  output["v1"] = v1;
  return output;
}


//#############################################################################@
//########################### est_ability_owen_cpp #############################
//#############################################################################@
// [[Rcpp::export]]
Rcpp::List est_ability_owen_cpp(Rcpp::S4 ip, Rcpp::NumericVector resp,
                                double m0, double v0) {
  // Rcout << "Stage (EstAbilityOwen) 1" << std::endl;
  List output;
  // if (Rf_inherits(ip, "Item")) {
  if (Rf_inherits(ip, "Rasch") || Rf_inherits(ip, "1PL") ||
      Rf_inherits(ip, "2PL") || Rf_inherits(ip, "3PL") ||
      Rf_inherits(ip, "4PL")) {
    resp = resp[0];
    output = est_ability_owen_item_cpp(ip, as<int>(resp), m0, v0);
  } else if (Rf_inherits(ip, "Itempool")) {
    int no_items = resp.size();
        
    // Rcpp::List item_list = ip.slot("item_list");
    Rcpp::List item_list = flatten_itempool_cpp(ip);	
    output = List::create(Named("m1") = m0, Named("v1") = v0);
    for (int i = 0; i < no_items; i++) {
      if (!NumericVector::is_na(resp[i]))
        output = est_ability_owen_item_cpp(as<Rcpp::S4>(item_list(i)), resp[i],
                                           output["m1"], output["v1"]);
    }    
  } else
    stop("This function can only process an 'Item' and 'Itempool' objects.");
  // The final output is given as the square root of the posterior variance v1.
  return List::create(Named("est") = output["m1"],
                      Named("se") = sqrt(as<double>(output["v1"])));
}


