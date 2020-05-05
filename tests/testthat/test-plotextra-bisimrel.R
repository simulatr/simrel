library(simrel)
library(testthat)

context("Testing Plot Extra Functions for Bivariate Simulation.")

set.seed(2020)
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
cov_xy = cov_xy(sobj)
cov_xy_sample = cov_xy(sobj, use_population=FALSE)
cov_zy = cov_zy(sobj)
cov_zy_sample = cov_zy(sobj, use_population=FALSE)

test_that("Tidyed Beta Coefficients from simrel.", {
    expect_equal(nrow(tidy_beta(sobj)), 15 * 2)
    expect_equal(ncol(tidy_beta(sobj)), 3)
    expect_equal(unique(tidy_beta(sobj)[['Predictor']]), 1:15)
    expect_equal(unique(tidy_beta(sobj)[['Response']]), 1:2)
    testthat::skip_on_cran()
    expect_equal(tidy_beta(sobj)[['BetaCoef']][1], -0.024117509, tolerance = 1e-5)
  }
)

test_that("Test Population Covariance of the simulated data.", {
    expect_equal(nrow(cov_xy(sobj)), 15)
    expect_equal(ncol(cov_xy(sobj)), 2)
    testthat::skip_on_cran()
    expect_equal(cov_xy(sobj)[6, 2], 0.00104368143, tolerance = 1e-5)
    expect_equal(cov_xy(sobj)[1, 1], -0.024117509, tolerance =1e-5)
  }
)

test_that("Test Sample Covariance of the simulated data.", {
    expect_equal(nrow(cov_xy(sobj, FALSE)), 15)
    expect_equal(ncol(cov_xy(sobj, FALSE)), 2)
    testthat::skip_on_cran()
    expect_equal(cov_xy(sobj, FALSE)[1, 1], -0.01307352, tolerance =1e-5)
    expect_equal(cov_xy(sobj, FALSE)[6, 2], 0.01540996, tolerance =1e-5)
  }
)

test_that("Test tidy lambda population.", {
    expect_equal(tidy_lambda(sobj)[["Predictor"]], seq.int(sobj$p))
    expect_equal(tidy_lambda(sobj)[["lambda"]][2], exp(-sobj$gamma))
    expect_true(all(tidy_lambda(sobj)[["lambda"]] > 0))
  }
)

test_that("Test tidy lambda sample.", {
    expect_equal(tidy_lambda(sobj, use_population = FALSE)[["Predictor"]], seq.int(sobj$p))
    expect_true(all(tidy_lambda(sobj, use_population = FALSE)[["lambda"]] > 0))
    testthat::skip_on_cran()
    expect_equal(tidy_lambda(sobj, use_population = FALSE)[["lambda"]][2], 0.4571567, tolerance =1e-5)
})

test_that("Test tidy sigma.", {
    testthat::skip_on_cran()
    expect_equal(tidy_sigma(cov_zy)[["Covariance"]][1], 0.21579509, tolerance =1e-5)
    expect_equal(tidy_sigma(cov_xy)[["Covariance"]][1], -0.02411751, tolerance =1e-5)
  }
)

test_that("Test Covariance Matrices", {
    expect_equal(sum(abs(cov_zy(sobj)) > 0), length(unlist(sobj$relpos)))
    expect_equal(nrow(cov_zy(sobj)), sobj$p)
    expect_equal(ncol(cov_zy(sobj)), 2)
    expect_equal(nrow(cov_xy(sobj)), sobj$p)
    expect_equal(ncol(cov_xy(sobj)), 2)
  }
)

test_that("Test Sample Covariance Matrices.", {
    expect_equal(nrow(cov_zy(sobj, use_population = FALSE)), sobj$p)
    expect_equal(ncol(cov_zy(sobj, use_population = FALSE)), 2)
    expect_equal(nrow(cov_xy(sobj, use_population = FALSE)), sobj$p)
    expect_equal(ncol(cov_xy(sobj, use_population = FALSE)), 2)
    testthat::skip_on_cran()
    expect_equal(cov_zy(sobj, use_population = FALSE)[1], 0.1844192, tolerance =1e-5)
    expect_equal(cov_xy(sobj, use_population = FALSE)[1], -0.01307352, tolerance =1e-5)
})

test_that("Function cov_zw stops for bivariate simrel.", {
    expect_error(cov_zw(sobj), "Use cov_zy function instead")
})

test_that("Absolute Covariances.", {
    expect_true(all(abs_sigma(tidy_sigma(cov_xy))[["Covariance"]] >= 0))
    expect_true(all(abs_sigma(tidy_sigma(cov_zy))[["Covariance"]] >= 0))
  }
)
