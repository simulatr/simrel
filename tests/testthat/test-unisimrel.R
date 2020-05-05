library(simrel)
library(testthat)

context("Testing Univariate Simulation.")

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
relpos  <- unname(sobj$relpos)
relpred <- unname(sobj$relpred)
sobj2   <- expression({
    set.seed(2019, kind = "Mersenne-Twister", normal.kind = "Inversion")
    unisimrel(n = 500, sim = sobj)
})

testthat::test_that("Expect Warning when using old simrel", {
    expect_warning({sobj2 <- eval(sobj2)},
                   regexp = "All parameters are collected from the supplied")
    expect_equal(sobj2$n, 500)
    expect_equal(sobj2$relpos, sobj$relpos)
    expect_equal(sobj2$Sigma, sobj$Sigma)
    expect_equal(sobj2$relpred, sobj$relpred)
  }
)

testthat::test_that("Testing of names.", {
    expect_s3_class(sobj, "simrel")
    expect_true(sobj$type == "univariate")
    expect_subset(names(sobj), c("beta", "beta0", "X", "Y", "Sigma", "type"))
  }
)

testthat::test_that("Test Dimensions.", {
    expect_equal(sobj$p + ncol(sobj$Y), ncol(sobj$Sigma))
    expect_equal(sobj$p + ncol(sobj$Y), nrow(sobj$Sigma))
    expect_equal(nrow(sobj$beta), sobj$p)
    expect_equal(ncol(sobj$beta), ncol(sobj$Y))
    expect_equal(ncol(sobj$beta), 1)
    expect_equal(ncol(sobj$Y), 1)
  }
)

testthat::test_that("Testing of position indices.", {
    expect_subset(unlist(relpred), unlist(relpos))
    expect_equal(unique(diag(sobj$R2 + sobj$minerror)), 1)
    expect_equal(which(sobj$Sigma[1, -1] != 0), sobj$relpos)
    expect_equal(which(sobj$Sigma[-1, 1] != 0), sobj$relpos)
    expect_equal(sobj$q, length(sobj$relpred))
    expect_equal(which(sobj$Sigma[1, -1] != 0), sobj$relpos)
  }
)

testthat::test_that("Testing different values.", {
    expect_equal(sobj$lambda[1], 1)
    expect_equal(sobj$lambda[2], exp(-sobj$gamma))
    testthat::skip_on_cran()
    expect_equal(sobj$minerror[1, 1], 0.2)
    expect_equal(sobj$beta[1,1], -1.70665523)
  }
)
