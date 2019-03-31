suppressPackageStartupMessages(library(simrel))
suppressPackageStartupMessages(library(testthat))

context("Testing Plot Extra Functions for Univariate Simulation.")

set.seed(2019)
sobj    <- unisimrel(
    n = 100,
    p = 15,
    q = 10,
    relpos = 1:5,
    gamma = 0.7,
    R2 = 0.8,
    ntest = 50
)
cov_xy = simrel:::cov_xy(sobj)
cov_xy_sample = simrel:::cov_xy(sobj, use_population=FALSE)
cov_zy = simrel:::cov_zy(sobj)
cov_zy_sample = simrel:::cov_zy(sobj, use_population=FALSE)

test_that("Tidyed Beta Coefficients from simrel.", {
    expect_equal(nrow(tidy_beta(sobj)), 15 * 1)
    expect_equal(ncol(tidy_beta(sobj)), 3)
    expect_equal(tidy_beta(sobj)[['BetaCoef']][1], 0.37312493, tolorance = 1e-5)
    expect_equal(unique(tidy_beta(sobj)[['Predictor']]), 1:15)
    expect_equal(unique(tidy_beta(sobj)[['Response']]), 1)
})

test_that("Test Population Covariance of the simulated data.", {
    expect_equal(nrow(cov_xy(sobj)), 15)
    expect_equal(ncol(cov_xy(sobj)), 1)
    expect_equal(cov_xy(sobj)[1, 1], -0.1058502332, tol = 1e-5)
    expect_equal(cov_xy(sobj)[6, 1], -0.1197740858)
})

test_that("Test Sample Covariance of the simulated data.", {
    expect_equal(nrow(cov_xy(sobj, FALSE)), 15)
    expect_equal(ncol(cov_xy(sobj, FALSE)), 1)
    expect_equal(cov_xy(sobj, FALSE)[1, 1], 0.185069740, tol = 1e-5)
    expect_equal(cov_xy(sobj, FALSE)[6, 1], 0.055623607, tol = 1e-5)
})

test_that("Test tidy lambda population.", {
    expect_equal(tidy_lambda(sobj)[["Predictor"]], seq.int(sobj$p))
    expect_equal(tidy_lambda(sobj)[["lambda"]][2], exp(-sobj$gamma))
    expect_true(all(tidy_lambda(sobj)[["lambda"]] > 0))
})

test_that("Test tidy lambda sample.", {
    expect_equal(tidy_lambda(sobj, use_population = FALSE)[["Predictor"]], seq.int(sobj$p))
    expect_true(all(tidy_lambda(sobj, use_population = FALSE)[["lambda"]] > 0))
    expect_equal(tidy_lambda(sobj, use_population = FALSE)[["lambda"]][2], 0.4084102, tol = 1e-5)
})

test_that("Test tidy sigma.", {
    expect_equal(tidy_sigma(cov_zy)[["Covariance"]][1],  0.14773852, tol = 1e-5)
    expect_equal(tidy_sigma(cov_xy)[["Covariance"]][1], -0.1058502332, tol = 1e-5)
})

test_that("Test Covariance Matrices.", {
    expect_equal(sum(abs(simrel:::cov_zy(sobj)) > 0), length(unlist(sobj$relpos)))
    expect_equal(nrow(simrel:::cov_zy(sobj)), sobj$p)
    expect_equal(ncol(simrel:::cov_zy(sobj)), 1)
    expect_equal(nrow(simrel:::cov_xy(sobj)), sobj$p)
    expect_equal(ncol(simrel:::cov_xy(sobj)), 1)
})

test_that("Test Sample Covariance Matrices.", {
    expect_equal(nrow(simrel:::cov_zy(sobj, use_population = FALSE)), sobj$p)
    expect_equal(ncol(simrel:::cov_zy(sobj, use_population = FALSE)), 1)
    expect_equal(simrel:::cov_zy(sobj, use_population = FALSE)[1], 0.3638807789, tol = 1e-8)
    expect_equal(nrow(simrel:::cov_xy(sobj, use_population = FALSE)), sobj$p)
    expect_equal(ncol(simrel:::cov_xy(sobj, use_population = FALSE)), 1)
    expect_equal(simrel:::cov_xy(sobj, use_population = FALSE)[1], 0.1850697, tol = 1e-5)
})

test_that("Function cov_zw stops for univariate simrel.", {
    expect_error(cov_zw(sobj), "Use cov_zy function instead")
})

test_that("Absolute Covariances.", {
    expect_true(all(abs_sigma(tidy_sigma(cov_xy))[["Covariance"]] >= 0))
    expect_true(all(abs_sigma(tidy_sigma(cov_zy))[["Covariance"]] >= 0))
})
