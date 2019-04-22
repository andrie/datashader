#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix discretize_cpp(DataFrame data, NumericVector dims,
                             NumericVector x_range, NumericVector y_range) {
  // Construct empty output matrix
  int rows = dims[0];
  int cols = dims[1];
  NumericMatrix z(rows, cols);
  std::fill(z.begin(), z.end(), 0);

  // Now loop over x and populate z

  NumericVector x = data[0];
  NumericVector y = data[1];

  unsigned long int xsize = data.nrow();
  for (unsigned long int i = 0; i < xsize; i++){
    int xx = round(((x[i] - x_range[0]) / (x_range[1] - x_range[0])) * (rows - 1));
    int yy = round(((y[i] - y_range[0]) / (y_range[1] - y_range[0])) * (cols - 1));
    // Rprintf("xx: %2i, yy: %2i\n", xx, yy);
    z(xx, yy) += 1;
  }
  return z;
}
