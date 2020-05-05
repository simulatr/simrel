library(simrel)
library(testthat)

context("Testing Plot Extra Functions for Univariate Simulation.")

set.seed(2020)
sobj    <- unisimrel(
    n = 100,
    p = 15,
    q = 10,
    relpos = 1:5,
    gamma = 0.7,
    R2 = 0.8,
    ntest = 50
)
cov_xy = cov_xy(sobj)
cov_xy_sample = cov_xy(sobj, use_population=FALSE)
cov_zy = cov_zy(sobj)
cov_zy_sample = cov_zy(sobj, use_population=FALSE)

test_that("Tidyed Beta Coefficients from simrel.", {
    expect_equal(nrow(tidy_beta(sobj)), 15 * 1)
    expect_equal(ncol(tidy_beta(sobj)), 3)
    expect_equal(unique(tidy_beta(sobj)[['Predictor']]), 1:15)
    expect_equal(unique(tidy_beta(sobj)[['Response']]), 1)
    testthat::skip_on_cran()
    expect_equal(tidy_beta(sobj)[['BetaCoef']][2], 1.21859679, tolerance = 1e-7)
})

test_that("Test Population Covariance of the simulated data.", {
    expect_equal(nrow(cov_xy(sobj)), 15)
    expect_equal(ncol(cov_xy(sobj)), 1)
    testthat::skip_on_cran()
    expect_equal(cov_xy(sobj)[1, 1], -0.001798418, tolerance = 1e-5)
    expect_equal(cov_xy(sobj)[6, 1], -0.096248054, tolerance = 1e-5)
})

test_that("Test Sample Covariance of the simulated data.", {
    expect_equal(nrow(cov_xy(sobj, FALSE)), 15)
    expect_equal(ncol(cov_xy(sobj, FALSE)), 1)
    testthat::skip_on_cran()
    expect_equal(cov_xy(sobj, FALSE)[1, 1], -0.1737318, tolerance = 1e-5)
    expect_equal(cov_xy(sobj, FALSE)[6, 1], -0.01231804, tolerance = 1e-5)
})

test_that("Test tidy lambda population.", {
    expect_equal(tidy_lambda(sobj)[["Predictor"]], seq.int(sobj$p))
    expect_equal(tidy_lambda(sobj)[["lambda"]][2], exp(-sobj$gamma))
    expect_true(all(tidy_lambda(sobj)[["lambda"]] > 0))
})

test_that("Test tidy lambda sample.", {
    expect_equal(tidy_lambda(sobj, use_population = FALSE)[["Predictor"]], seq.int(sobj$p))
    expect_true(all(tidy_lambda(sobj, use_population = FALSE)[["lambda"]] > 0))
    testthat::skip_on_cran()
    expect_equal(tidy_lambda(sobj, use_population = FALSE)[["lambda"]][2], 0.5713497, tolerance = 1e-5)
})

test_that("Test tidy sigma.", {
    testthat::skip_on_cran()
    expect_equal(tidy_sigma(cov_zy)[["Covariance"]][1],  0.3529785, tolerance = 1e-5)
    expect_equal(tidy_sigma(cov_xy)[["Covariance"]][1], -0.001798418, tolerance = 1e-5)
})

test_that("Test Covariance Matrices.", {
    expect_equal(sum(abs(cov_zy(sobj)) > 0), length(unlist(sobj$relpos)))
    expect_equal(nrow(cov_zy(sobj)), sobj$p)
    expect_equal(ncol(cov_zy(sobj)), 1)
    expect_equal(nrow(cov_xy(sobj)), sobj$p)
    expect_equal(ncol(cov_xy(sobj)), 1)
})

test_that("Test Sample Covariance Matrices.", {
    expect_equal(nrow(cov_zy(sobj, use_population = FALSE)), sobj$p)
    expect_equal(ncol(cov_zy(sobj, use_population = FALSE)), 1)
    expect_equal(nrow(cov_xy(sobj, use_population = FALSE)), sobj$p)
    expect_equal(ncol(cov_xy(sobj, use_population = FALSE)), 1)
    testthat::skip_on_cran()
    expect_equal(cov_zy(sobj, use_population = FALSE)[1], -0.150382402, tolerance = 1e-6)
    expect_equal(cov_xy(sobj, use_population = FALSE)[1], -0.1737318, tolerance = 1e-5)
})

test_that("Function cov_zw stops for univariate simrel.", {
    expect_error(cov_zw(sobj), "Use cov_zy function instead")
})

test_that("Absolute Covariances.", {
    expect_true(all(abs_sigma(tidy_sigma(cov_xy))[["Covariance"]] >= 0))
    expect_true(all(abs_sigma(tidy_sigma(cov_zy))[["Covariance"]] >= 0))
})
