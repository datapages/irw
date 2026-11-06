#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector f_outer(NumericVector x) {
  // We'll use the size of the first and second vectors for our for loops
    int n = x.size();
  
  // Initialize a result variable
  double marker = 0.0;
  NumericVector result ((n*(n-1))/2);
  // And use loops instead of outer
  for ( int i = 0; i < (n-1); ++i ) {
    for ( int j = (i+1); j < n; ++j ) {
      result[marker] = x[i]-x[j];
      marker += 1.0;
    }
  }
  // Then return the result
  return result;
}

// [[Rcpp::export]]
double relationalityC(NumericVector Xi, NumericVector Xj) {
  
  double k = Xi.size();
  
  NumericVector times_val = Xi * Xj;
  NumericVector lambda (k, 0);
  
  for ( int i = 0; i < (k); ++i ) {
    if (times_val[i] < 0) lambda[i] = -1;
    else if (times_val[i] >= 0) lambda[i] = 1;
  }
  
  NumericVector dist = 1-abs(abs(Xi) - abs(Xj));
  
  dist = lambda * dist;

  double relationalities = sum(dist);
  
  relationalities = (relationalities)/k;
  
  return relationalities;

}