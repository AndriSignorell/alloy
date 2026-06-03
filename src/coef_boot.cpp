
// ============================================================
// coef_boot.cpp
//
// Bootstrap confidence intervals for OLS regression coefficients.
// Uses the generic parallel bootstrap framework.
//
// R interface:
//   coef_boot_cpp(X, y, B = 2000, alpha = 0.05, seed = -1)
//
// Returns a matrix with one row per coefficient and
// columns est / lci / uci.
//
// ============================================================

#include "boot_framework.h"


// ============================================================
// statistic: OLS coefficients
// ============================================================

struct CoefFn {

  arma::vec compute(const arma::mat& X, const arma::vec& y) const {
    return arma::solve(X, y);
  }
};


// [[Rcpp::export]]
NumericMatrix coef_boot_cpp(NumericMatrix X,
                            NumericVector y,
                            int    R     = 2000,
                            double alpha = 0.05,
                            int    seed  = -1,
                            String method = "perc") {

  return run_boot_matrix(X, y, R, alpha, seed, CoefFn(), method);
}

