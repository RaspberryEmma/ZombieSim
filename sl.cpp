// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace arma;
using namespace Rcpp;


// [[Rcpp::export(name = "synthetic_likelihood")]]
arma::vec sl(vec& y, mat& X) {

  vec something(10, fill::zeros);

  return something;
}


