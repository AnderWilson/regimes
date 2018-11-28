#include <RcppArmadillo.h>
using namespace Rcpp;

// This functions generates a random inverse gaussian rv

// [[Rcpp::depends(RcppArmadillo)]]
double rinvgauss(double mu, double lambda) {

  double v = rnorm(1)(0);
  double y = v*v;
  double x = mu + (mu*mu*y)/(2*lambda) - mu/(2*lambda)*sqrt(4*mu*lambda*y + mu*mu*y*y);
  double z = runif(1)(0);
  double a = 0;

  if(z < mu/(mu+y)){
    a = x;
  }else{
    a = mu*mu/x;
  }

  return a;
}
