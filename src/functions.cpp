// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]

// Gradient Descent
// [[Rcpp::export(".gd")]]
Rcpp::List gd(arma::mat x,
              arma::vec y,
              arma::vec b,
              arma::vec stepsize,
              int maxit,
              float tolerance){

  arma::mat A(maxit, x.n_cols, arma::fill::zeros);
  int i = 0;
  while(i < maxit){

    if(abs(b).max() > 1e300)
      Rcpp::stop("Beta too large: decrease the step size!");

    arma::mat gradient = 2 * x.t() * (x * b - y);
    arma:: vec bp = b;
    b = bp - gradient * stepsize;

    if (abs(b - bp).max() < tolerance){
      Rcpp::warning("Tolerance reached\n");
      break;
    }
    A.insert_rows(i, b.t());
    i++;
  }

  return Rcpp::List::create(Rcpp::Named("b") = b,
                            Rcpp::Named("A") = A,
                            Rcpp::Named("i") = i);
}


// Steepest Descent
// [[Rcpp::export(".sd")]]
Rcpp::List sd(arma::mat x,
              arma::vec y,
              arma::vec b,
              int maxit,
              float tolerance){

  arma::mat A(maxit, x.n_cols, arma::fill::zeros);
  int i = 0;
  while(i < maxit){

    if(abs(b).max() > 1e300)
      Rcpp::stop("Beta too large: decrease the step size!");

    arma::mat gradient = 2 * x.t() * (x * b - y);
    arma::mat hessian = 4 * x.t() * x;
    arma::mat stepsize = sum(pow(gradient,2))/(gradient.t() * hessian * gradient);
    arma::vec bp = b;
    b = bp - gradient * stepsize;

    if (abs(b - bp).max() < tolerance){
      Rcpp::warning("Tolerance reached\n");
      break;
    }
    A.insert_rows(i, b.t());
    i++;
  }

  return Rcpp::List::create(Rcpp::Named("b") = b,
                            Rcpp::Named("A") = A,
                            Rcpp::Named("i") = i);

}
