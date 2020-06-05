#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// Fast version of empty in C++
// [[Rcpp::export]]
LogicalMatrix emptyRcpp(arma::cube a) {
  
  // Extract the different dimensions.
  
  unsigned int xdim = a.n_rows;
  unsigned int ydim = a.n_cols;
  unsigned int ddim = a.n_slices;
  
  // Initialise an empty matrix to hold test results.
  
  LogicalMatrix result(xdim, ydim);
  
  // Begin pixel selection.
  // For each column in the array:
  
  for (unsigned int x = 0; x < xdim; x++) { // array index starts at 0 in C++
    
    // Work through the row indices:
    
    for (unsigned int y = 0; y < ydim; y++) { // array index starts at 0 in C++
      
      // Initialise a vector for the values from each slice
      
      NumericVector pixel(ddim); 
      
      // For each slice take the value and fill in the pixel vector
      
      for (unsigned int d = 0; d < ddim; d++) { // array index starts at 0 in C++
        
        pixel(d) = a(x,y,d);
        
      }
      
      // Now we have the values at a pixel, does it only contain NAs?
      // Answer in the result matrix
      
      result(x,y) = is_true(all(is_na(pixel)));
      
    }
  }
  
  return result;
  
} 






// [[Rcpp::export]]
LogicalMatrix emptyRcpp2(arma::cube a) {
  
  // Extract the different dimensions.
  
  unsigned int xdim = a.n_rows;
  unsigned int ydim = a.n_cols;
  unsigned int ddim = a.n_slices;
  
  // Initialise an empty matrix to hold test results.
  
  LogicalMatrix result(xdim, ydim);
  
  // Begin pixel selection.
  // For each column in the array:
  
  for (unsigned int x = 0; x < xdim; x++) { // array index starts at 0 in C++
    
    // Work through the row indices:
        for (unsigned int y = 0; y < ydim; y++) { // array index starts at 0 in C++
      
      // Initialise a vector for the values from each slice
      // Now we have the values at a pixel, does it only contain NAs?
      // Answer in the result matrix
      
  //  arma::subview_cube<double> pixel = a.tube(x,y);

    NumericVector pixel = a.elem(x,y, 0, 1, 1, ddim);
    
    result(x,y) = is_true(all(is_na(pixel)));
      
    }
  }
  
  return result;
  
} 
  