
#include <Rcpp.h>

using namespace Rcpp;

double distanceHaversine(double latf, double lonf, double latt, double lont,
                         double tolerance);
double toRadians(double deg);

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
Rcpp::DataFrame cpp_snap_points(Rcpp::NumericMatrix& data, Rcpp::NumericMatrix& ref, int spatial_resolution){
  Rcpp::NumericVector result_x;
  Rcpp::NumericVector result_y;
  Rcpp::NumericVector result_pos;
  
  const int nrow = data.nrow();
  const int ref_nrow = ref.nrow();
  int ref_i = -1;
  int total_found = 0;

  for(int i = 0; i < nrow; i++){
    const double x = data[i];
    const double y = data[i + nrow];
    double dist;

    do {
      ref_i++;
      const double ref_x = ref[ref_i];
      const double ref_y = ref[ref_i + ref_nrow];

      dist = distanceHaversine(toRadians(y), toRadians(x), toRadians(ref_y), toRadians(ref_x), 1);
    } while (dist > spatial_resolution && ref_i < ref_nrow);

    if(ref_i < ref_nrow){
      result_x.push_back(ref[ref_i]);
      result_y.push_back(ref[ref_i + ref_nrow]);
      result_pos.push_back(ref_i + 1);
      total_found++;
    }
  }

  if(total_found < nrow){
    return cpp_snap_points(data, ref, spatial_resolution * 2);
  }
  
  Rcpp::DataFrame result = 
    Rcpp::DataFrame::create(Rcpp::Named("x") = result_x,
                            Rcpp::Named("y") = result_y,
                            Rcpp::Named("pos") = result_pos);
  
  return result;
}
