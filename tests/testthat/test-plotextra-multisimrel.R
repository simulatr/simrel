suppressPackageStartupMessages(library(simrel))
suppressPackageStartupMessages(library(testthat))

context("Testing Plot Extra Functions for Multivariate Simulation.")

set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion")
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
cov_xy = simrel:::cov_xy(sobj)
cov_xy_sample = simrel:::cov_xy(sobj, use_population=FALSE)
cov_zy = simrel:::cov_zy(sobj)
cov_zy_sample = simrel:::cov_zy(sobj, use_population=FALSE)
cov_zy = simrel:::cov_zy(sobj)
cov_zy_sample = simrel:::cov_zy(sobj, use_population=FALSE)

test_that("Tidied Beta Coefficients from simrel.", {
    expect_equal(nrow(tidy_beta(sobj)), 75)
    expect_equal(ncol(tidy_beta(sobj)), 3)
    expect_equal(tidy_beta(sobj)[['BetaCoef']][1], 0.364895)
    expect_equal(unique(tidy_beta(sobj)[['Predictor']]), 1:15)
    expect_equal(unique(tidy_beta(sobj)[['Response']]), 1:5)
})

test_that("Test Population Covariance of the simulated data.", {
    expect_equal(nrow(cov_xy(sobj)), 15)
    expect_equal(ncol(cov_xy(sobj)), 5)
    expect_equal(cov_xy(sobj)[1, 1], 0.3202315, tol = 1e-5)
    expect_equal(cov_xy(sobj)[5, 5], 0)
})

test_that("Test Sample Covariance of the simulated data.", {
    expect_equal(nrow(cov_xy(sobj, FALSE)), 15)
    expect_equal(ncol(cov_xy(sobj, FALSE)), 5)
    expect_equal(cov_xy(sobj, FALSE)[1, 1], 0.2915116, tol = 1e-5)
    expect_equal(cov_xy(sobj, FALSE)[5, 5], -0.005538044, tol = 1e-5)
})

test_that("Test tidy lambda population.", {
    expect_equal(tidy_lambda(sobj)[["Predictor"]], seq.int(sobj$p))
    expect_equal(tidy_lambda(sobj)[["lambda"]][2], exp(-sobj$gamma))
    expect_true(all(tidy_lambda(sobj)[["lambda"]] > 0))
})

test_that("Test tidy lambda sample.", {
    expect_equal(tidy_lambda(sobj, use_population = FALSE)[["Predictor"]], seq.int(sobj$p))
    expect_true(all(tidy_lambda(sobj, use_population = FALSE)[["lambda"]] > 0))
    expect_equal(tidy_lambda(sobj, use_population = FALSE)[["lambda"]][2], 0.4879944, tol = 1e-5)
})

test_that("Test tidy sigma.", {
    expect_equal(tidy_sigma(cov_zy)[["Covariance"]][1],  -0.867989, tol = 1e-5)
    expect_equal(tidy_sigma(cov_xy)[["Covariance"]][1], 0.3202315, tol = 1e-5)
})

test_that("Test Covariance Matrices", {
    expect_equal(sum(abs(simrel:::cov_zw(sobj)) > 0), length(unlist(sobj$relpos)))
    expect_equal(nrow(simrel:::cov_zw(sobj)), sobj$p)
    expect_equal(ncol(simrel:::cov_zw(sobj)), sobj$m)
    expect_equal(nrow(simrel:::cov_zy(sobj)), sobj$p)
    expect_equal(ncol(simrel:::cov_zy(sobj)), sobj$m)
    expect_equal(nrow(simrel:::cov_xy(sobj)), sobj$p)
    expect_equal(ncol(simrel:::cov_xy(sobj)), sobj$m)
})

test_that("Test Sample Covariance Matrices.", {
    expect_equal(nrow(simrel:::cov_zy(sobj, use_population = FALSE)), sobj$p)
    expect_equal(ncol(simrel:::cov_zy(sobj, use_population = FALSE)), sobj$m)
    expect_equal(simrel:::cov_zy(sobj, use_population = FALSE)[1], 0.8727467, tol = 1e-5)
    expect_equal(nrow(simrel:::cov_xy(sobj, use_population = FALSE)), sobj$p)
    expect_equal(ncol(simrel:::cov_xy(sobj, use_population = FALSE)), sobj$m)
    expect_equal(simrel:::cov_xy(sobj, use_population = FALSE)[1], 0.2915116, tol = 1e-5)
})

test_that("Absolute Covariances.", {
    expect_true(all(abs_sigma(tidy_sigma(cov_xy))[["Covariance"]] >= 0))
    expect_true(all(abs_sigma(tidy_sigma(cov_zy))[["Covariance"]] >= 0))
})
