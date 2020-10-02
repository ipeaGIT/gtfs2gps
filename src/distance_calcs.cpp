
#include <Rcpp.h>
using namespace Rcpp;

double inverseHaversine(double d){
  return 2 * atan2(sqrt(d), sqrt(1 - d)) * 6378137.0;
}

double distanceHaversine(double latf, double lonf, double latt, double lont,
                         double tolerance){
  double d;
  double dlat = latt - latf;
  double dlon =  lont - lonf;

  d = (sin(dlat * 0.5) * sin(dlat * 0.5)) + (cos(latf) * cos(latt)) * (sin(dlon * 0.5) * sin(dlon * 0.5));
  if(d > 1 && d <= tolerance){
    d = 1;
  }
  return inverseHaversine(d);
}

double toRadians(double deg){
  return deg * 0.01745329251;  // PI / 180;
}

// [[Rcpp::export]]
Rcpp::NumericVector rcpp_distance_haversine(Rcpp::NumericVector latFrom, Rcpp::NumericVector lonFrom, 
                        Rcpp::NumericVector latTo, Rcpp::NumericVector lonTo,
                        double tolerance) {

  int n = latFrom.size();
  NumericVector distance(n);

  double latf;
  double latt;
  double lonf;
  double lont;
  double dist = 0;
  
  distance[0] = 0;

  for(int i = 1; i < n; i++){

    latf = toRadians(latFrom[i - 1]);
    lonf = toRadians(lonFrom[i - 1]);
    latt = toRadians(latTo[i - 1]);
    lont = toRadians(lonTo[i - 1]);
    dist = distanceHaversine(latf, lonf, latt, lont, tolerance);

    distance[i] = dist;
  }
  return distance;
}
