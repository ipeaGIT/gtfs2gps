
#include <Rcpp.h>
#include <iostream>

using namespace std;
using namespace Rcpp;

double distanceHaversine(double latf, double lonf, double latt, double lont,
                         double tolerance);
double toRadians(double deg);

Rcpp::NumericVector cpp_snap_points_level(Rcpp::NumericMatrix& data, Rcpp::NumericMatrix& ref, int spatial_resolution){
  Rcpp::NumericVector result_pos;
  
  const int nrow = data.nrow();
  const int ref_nrow = ref.nrow();
  
  int ref_i = -1;
  int i = 0;
  bool found;
  double dist;
  double dist_next;

  while(i < nrow && ref_i + 1 < ref_nrow) {
    const double x = data[i];
    const double y = data[i + nrow];

    found = false;
    
    do {
      ref_i++;
      const double ref_x = ref[ref_i];
      const double ref_y = ref[ref_i + ref_nrow];
      
      dist = distanceHaversine(toRadians(y), toRadians(x), toRadians(ref_y), toRadians(ref_x), 1);

      const double ref_x_next = ref[ref_i + 1];
      const double ref_y_next = ref[ref_i + 1 + ref_nrow];

      dist_next = distanceHaversine(toRadians(ref_y_next), toRadians(ref_x_next), toRadians(ref_y), toRadians(ref_x), 1);

      if(dist < spatial_resolution && dist < dist_next){
        result_pos.push_back(ref_i + 1);
        found = true;
      }
    } while (ref_i + 1 < ref_nrow && !found);

    i++;
  }

  if(result_pos.length() < nrow){
      return Rcpp::NumericVector();
  }
  return result_pos;
}

//' @title Snap points to the closest points from another set
//' @description Snap a set of points to the closest points available in another set
//' of points.
//' @param data A set of points to be snapped (a matrix). The result will have  
//' the same number of rows of this argument. Each row will return the respective
//' snapped point.
//' @param ref A set of reference points (another matrix). The result will be 
//' a subset of this parameter.
//' @param spatial_resolution The spatial resolution of data, which means that from each
//' point of data it is possible to reach at least one point within data with distance 
//' equals or less than spatial_resolution.
//' @return A data.frame with the snapped points.
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector cpp_snap_points(Rcpp::NumericMatrix& data, Rcpp::NumericMatrix& ref, int spatial_resolution){
  return cpp_snap_points_level(data, ref, spatial_resolution);
}
