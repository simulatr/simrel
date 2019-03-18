library(simrel)
library(testthat)

context("Testing Plot Extra Functions for bisimrel.")

set.seed(2019)
sobj <- bisimrel(
    n = 100,
    p = 15,
    q = c(5, 6, 2),
    rho = c(0.8, 0.4),
    relpos = list(c(1, 2, 3), c(2, 3, 4, 5)),
    gamma = 0.7,
    R2 = c(0.8, 0.7),
    ntest = 50
)
cov_xy = simrel:::cov_xy(sobj)
cov_zy = simrel:::cov_zy(sobj)

test_that("Tidyed Beta Coefficients from simrel.", {
    expect_equal(nrow(tidy_beta(sobj)), 15 * 2)
    expect_equal(ncol(tidy_beta(sobj)), 3)
    expect_equal(tidy_beta(sobj)[['BetaCoef']][1], -0.0497643, tolorance = 1e-5)
    expect_equal(unique(tidy_beta(sobj)[['Predictor']]), 1:15)
    expect_equal(unique(tidy_beta(sobj)[['Response']]), 1:2)
  }
)

test_that("Test Population Covariance of the simulated data.", {
    expect_equal(nrow(cov_xy(sobj)), 15)
    expect_equal(ncol(cov_xy(sobj)), 2)
    expect_equal(cov_xy(sobj)[1, 1], -0.0497643, tol = 1e-5)
    expect_equal(cov_xy(sobj)[6, 2], 0)
  }
)

test_that("Test Sample Covariance of the simulated data.", {
    expect_equal(nrow(cov_xy(sobj, FALSE)), 15)
    expect_equal(ncol(cov_xy(sobj, FALSE)), 2)
    expect_equal(cov_xy(sobj, FALSE)[1, 1], -0.037495170, tol = 1e-5)
    expect_equal(cov_xy(sobj, FALSE)[6, 2], -0.0040653878, tol = 1e-5)
  }
)

test_that("Test tidy lambda.", {
    expect_equal(tidy_lambda(sobj)[["Predictor"]], seq.int(sobj$p))
    expect_equal(tidy_lambda(sobj)[["lambda"]][2], exp(-sobj$gamma))
    expect_true(all(tidy_lambda(sobj)[["lambda"]] > 0))
  }
)

test_that("Test tidy sigma.", {
    expect_equal(tidy_sigma(cov_zy)[["Covariance"]][1],  0.2959773, tol = 1e-5)
    expect_equal(tidy_sigma(cov_xy)[["Covariance"]][1], -0.0497643, tol = 1e-5)
  }
)

test_that("Test Covariance Matrices", {
    expect_equal(sum(abs(simrel:::cov_zy(sobj)) > 0), length(unlist(sobj$relpos)))
    expect_equal(nrow(simrel:::cov_zy(sobj)), sobj$p)
    expect_equal(ncol(simrel:::cov_zy(sobj)), 2)
    expect_equal(nrow(simrel:::cov_xy(sobj)), sobj$p)
    expect_equal(ncol(simrel:::cov_xy(sobj)), 2)
  }
)

test_that("Absolute Covariances.", {
    expect_true(all(abs_sigma(tidy_sigma(cov_xy))[["Covariance"]] >= 0))
    expect_true(all(abs_sigma(tidy_sigma(cov_zy))[["Covariance"]] >= 0))
  }
)
