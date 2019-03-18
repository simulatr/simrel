suppressPackageStartupMessages(library(simrel))
suppressPackageStartupMessages(library(testthat))

context("Testing Bivariate Simulation.")

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
relpos <- unname(sobj$relpos)
relpred <- unname(sobj$relpred)


testthat::test_that(
              "Testing of names.", {
                  expect_s3_class(sobj, "simrel")
                  expect_true(sobj$type == "bivariate")
                  expect_subset(names(sobj), c("beta", "beta0", "X", "Y", "Sigma", "type"))
              }
          )
