#include <Rcpp.h>
using namespace Rcpp;



//#############################################################################@
//########################### integrate ########################################
//#############################################################################@
// x: A list of values for numerical integration
// fx: A list of values corresponding to f(x) values.
// [[Rcpp::export]]
double integrate(Rcpp::NumericVector x, Rcpp::NumericVector fx)
{
  // Here it is assumed that x and fx has the same length.
  int n = x.size()-1;
  double area = 0;
  for (int i = 0; i < n; i++) {
    // area = area + base *  height
    area = area + (x[i + 1] - x[i]) * ((fx[i] + fx[i+1])/2);
  }
  return area;
}



//#############################################################################@
//########################### check_item_model #################################
//#############################################################################@
//' @title This function checks whether an Item objects model is dichotomous or
//' polytomous; or unidimensional/multidimensional.
//'
//' @description
//' This function effectively divides models into four categories:
//' model is_dichotomous is_unidimensional                                 Call
//'   2PL           true              true   check_item_model(item, true, true)
//'  M2PL           true             false  check_item_model(item, true, false)
//'  GPCM          false              true  check_item_model(item, false, true)
//'
//' @noRd
//'

// [[Rcpp::export]]
bool check_item_model(Rcpp::S4 item,
                      bool is_dichotomous = true,
                      bool is_unidimensional = true) {
  std::string model = as<std::string>(item.attr("class"));

  bool result = true;

  if (is_dichotomous) {
    result = result && (model == "Rasch" || model == "1PL"  || model == "2PL" ||
      model == "3PL" || model == "4PL" || model == "M2PL" || model == "M3PL");
  } else {
    result = result && (model == "GRM" || model == "GPCM" || model == "GPCM2" ||
      model == "PCM");
  }

  if (is_unidimensional) {
    result = result && (model == "Rasch" || model == "1PL"  || model == "2PL" ||
      model == "3PL" || model == "4PL" || model == "GRM" || model == "GPCM" ||
      model == "GPCM2" || model == "PCM");
  } else {
    result = result && (model == "M2PL" || model == "M3PL");
  }
  return result;
}
