#include <Rcpp.h>
using namespace Rcpp;


//#############################################################################@
//########################### gauss_hermite ####################################
//#############################################################################@
// This function finds the nodes and weights of Gauss-Hermite quadrature for
// a given number of points 'n'.


extern "C" {
    void gausq2_(int *n, double *a, double *b, double *z, int *ierr);
}

// [[Rcpp::export]]
Rcpp::List gauss_hermite(int n) {
  double lnmuzero = std::log(M_PI/2);
	Rcpp::NumericVector a (n);
	Rcpp::NumericVector b (n);
	for (int i = 1; i < n; ++i) {
	  b[i-1] = std::sqrt((double)i/2);
	  // Rcout << i-1 << "-"<< i << "-" << "-" << b[i-1] << std::endl;
	}
	int ierr = 0;
	Rcpp::NumericVector z(n);
	z[0] = 1;
	gausq2_(&n, a.begin(), b.begin(), z.begin(), &ierr);
	Rcpp::List output;
	output["nodes"] = a;
  output["weights"] = Rcpp::exp(lnmuzero + 2 * Rcpp::log(Rcpp::abs(z))) * 1.1283791670955125585606993;
  return output;
}
