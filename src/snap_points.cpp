
#include <Rcpp.h>
using namespace Rcpp;

//' @title Snap points to the closest points from another set
//' @description Snap a set of points to the closest points available in another set
//' of points. The result is a data.frame.
//' @param data A set of points to be snapped (a matrix). The result will have  
//' the same number of rows of this argument. Each row will return the respective
//' snapped point.
//' @param ref A set of reference points (another matrix). The result will be 
//' a subset of this parameter.
//' @export
// [[Rcpp::export]]
Rcpp::DataFrame cpp_snap_points(Rcpp::NumericMatrix& data, Rcpp::NumericMatrix& ref){
  Rcpp::NumericVector result_x;
  Rcpp::NumericVector result_y;
  
  const int nrow = data.nrow();
  const int ref_nrow = ref.nrow();
  
  for(int i = 0; i < nrow; i++){
    double min_dist = 1e+100;
    double min_pos = -1;
    
    const double x = data[i];
    const double y = data[i + nrow];
    
    for(int ref_i = 0; ref_i < ref_nrow; ref_i++){
      const double ref_x = ref[ref_i];
      const double ref_y = ref[ref_i + ref_nrow ];
      
      const double dist = sqrt(pow(x - ref_x, 2) + pow(y - ref_y, 2));
      
      if(dist < min_dist){
        min_dist = dist;
        min_pos = ref_i;
      }
    }
    
    result_x.push_back(ref[min_pos]);
    result_y.push_back(ref[min_pos + ref_nrow]);
  }
  
  Rcpp::DataFrame result = 
    Rcpp::DataFrame::create(Rcpp::Named("x") = result_x,
                            Rcpp::Named("y") = result_y);
  
  return result;
}
