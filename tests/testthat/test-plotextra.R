library(simrel)
library(testthat)

context("Testing Plot Extra Functions.")

set.seed(123)
sobj <- multisimrel(
  n      = 100, 
  p      = 15, 
  q      = c(5, 4, 3), 
  m      = 5, 
  relpos = list(c(1,  2), c(3, 4, 6), c(5, 7)), 
  gamma  = 0.6, 
  R2     = c(0.8, 0.7, 0.8), 
   eta   = 0, 
  ntest  = NULL, 
  muX    = NULL, 
  muY    = NULL, 
  ypos   = list(c(1),  c(3, 4), c(2, 5))
)

test_that(
  "Tidyed Beta Coefficients from simrel.", {
    expect_dim(tidy_beta(sobj), c(75, 3))
    expect_equal(tidy_beta(sobj)[['BetaCoef']][1], 0.364895)
    expect_equal(unique(tidy_beta(sobj)[['Predictor']]), 1:15)
    expect_equal(unique(tidy_beta(sobj)[['Response']]), 1:5)
  }
)

test_that(
  "Test Population Covariance of the simulated data.", {
    expect_dim(cov_xy(sobj), c(15, 5))
    expect_equal(cov_xy(sobj)[1, 1], 0.3202315, tol = 1e-5)
    expect_equal(cov_xy(sobj)[5, 5], 0)
  }
)

test_that(
  "Test Sample Covariance of the simulated data.", {
    expect_dim(cov_xy(sobj, FALSE), c(15, 5))
    expect_equal(cov_xy(sobj, FALSE)[1, 1], 0.2915116, tol = 1e-5)
    expect_equal(cov_xy(sobj, FALSE)[5, 5], -0.005538044, tol = 1e-5)
  }
)