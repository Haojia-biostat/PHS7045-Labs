Lab 05 - Rcpp
================
Haojia Li
2/9/23

# Learning goals

- Use the different data types in Rcpp.
- Learn some fundamentals about C++ optimization.
- Practice your GitHub skills.

# Lab description

For this lab, we will create a function for propensity score matching.
The goal is simple: write out a C++ function with Rcpp and measure how
faster it is compared to the following R implementation:

``` r
ps_matchR <- function(x) {
  
  match_expected <- as.matrix(dist(x))
  diag(match_expected) <- .Machine$integer.max
  indices <- apply(match_expected, 1, which.min)
  
  list(
    match_id = as.integer(unname(indices)),
    match_x  = x[indices]
  )
  
}
```

## Question 1: Create a simple function

Use the following pseudo-code template to get started:

``` rcpp
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List ps_match1(const NumericVector & x) {
  
  int n = x.size();
  
  IntegerVector indices(n);
  NumericVector values(n);
  
  for (int i = 0; i < n; ++i) {
    
    int best_n = 0;
    double best_dist = std::numeric_limits< double >::max();
    
    for (int j = 0; j < n; ++j) {
      
      if(i == j)
        continue;
      
      double tmp_dist = abs(x[i] - x[j]);
      if(tmp_dist < best_dist){
        
        best_dist = tmp_dist;
        best_n    = j;
        
      }
      
    }
    
    indices[i] = best_n;
    values[i] = x[best_n];
    
  }
  
  return List::create(
    _["match_id"] = indices,
    _["match_n"] = values
  );
  
}
```

``` r
set.seed(7045)
x <- runif(10)
x
```

     [1] 0.88230841 0.06227915 0.80597008 0.01570025 0.61318581 0.31418546
     [7] 0.70914982 0.45784591 0.05574193 0.78320645

``` r
ps_matchR(x)
```

    $match_id
     [1]  3  9 10  9  7  8 10  6  2  3

    $match_x
     [1] 0.80597008 0.05574193 0.78320645 0.05574193 0.70914982 0.45784591
     [7] 0.78320645 0.31418546 0.06227915 0.80597008

``` r
ps_match1(x)
```

    $match_id
     [1] 2 8 9 8 6 7 9 5 1 2

    $match_n
     [1] 0.80597008 0.05574193 0.78320645 0.05574193 0.70914982 0.45784591
     [7] 0.78320645 0.31418546 0.06227915 0.80597008

In C++ the indices range from 0 to (n - 1), whereas in R is from 1 to n.

## Question 2: Things can be done faster

In the previous question, we have a double loop running twice over the
full set of observations. We need you to write the C++ so that the
computational complexity goes below `n^2`. (hint: Distance is symmetric)

``` rcpp
```
