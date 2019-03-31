suppressPackageStartupMessages(library(simrel))
suppressPackageStartupMessages(library(testthat))

context("Testing Multivariate Simulation.")

set.seed(2019)
sobj <- multisimrel(
  n = 100,
  p = 15,
  q = c(5, 4, 3),
  m = 5,
  relpos = list(c(1, 2), c(3, 4, 6), c(5, 7)),
  ypos = list(c(1), c(3, 4), c(2, 5)),
  gamma = 0.6,
  R2 = c(0.8, 0.7, 0.8),
  eta = 0,
  ntest = 50
)
relpos <- unname(sobj$relpos)
relpred <- unname(sobj$relpred)
ny <- sobj$m
cov_xy <- sobj$Sigma[seq_along(sobj$relpos), -c(1:ny)]
cov_zw <- sobj$SigmaWZ[seq_along(sobj$relpos), -c(1:ny)]


testthat::test_that(
  "Testing of names.", {
    expect_s3_class(sobj, "simrel")
    expect_true(sobj$type == "multivariate")
    expect_subset(names(sobj), c("beta", "beta0", "X", "Y", "Sigma", "type"))
  }
)

testthat::test_that(
  "Test Dimensions.", {
    expect_equal(sobj$p + ncol(sobj$Y), ncol(sobj$Sigma))
    expect_equal(sobj$p + ncol(sobj$Y), nrow(sobj$Sigma))
    expect_equal(nrow(sobj$beta), sobj$p)
    expect_equal(ncol(sobj$beta), ncol(sobj$Y))
    expect_equal(ncol(sobj$X), ncol(sobj$testX))
    expect_equal(ncol(sobj$Y), ncol(sobj$testY))
    expect_equal(nrow(sobj$X), sobj$n)
    expect_equal(nrow(sobj$testX), sobj$call$ntest)
  }
)

testthat::test_that(
  "Testing of position indices.", {
    expect_subset(unlist(relpred), unlist(relpos))
    expect_equal(unique(diag(sobj$RsqY + sobj$minerror)), 1)
    expect_equal(length(unlist(relpred[-length(relpred)])), sum(sobj$q))
    expect_equal(
      apply(cov_xy, 1, function(x) sum(x != 0)),
      sapply(relpred, length)[seq_along(sobj$q)]
    )
    expect_equal(
      apply(cov_zw, 1, function(x) sum(x != 0)),
      sapply(relpos, length)[seq_along(sobj$q)]
    )
  }
)

testthat::test_that(
  "Testing different values.", {
    expect_equal(sobj$lambda[1], 1)
    expect_equal(sobj$lambda[2], exp(-sobj$gamma))
    expect_equal(sobj$minerror[1, 1], 0.2)
    expect_equal(sobj$minerror[2, 2], 0.65)
    expect_equal(sobj$minerror[5, 2], 0.35)
    expect_equal(sobj$beta[1,1], 0.689421991)
  }
)
