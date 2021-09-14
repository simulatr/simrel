library(simrel)
library(testthat)

context("Testing Bivariate Simulation.")

set.seed(2020)
sobj <- bisimrel(
    n      = 100,
    p      = 15,
    q      = c(5, 6, 2),
    rho    = c(0.8, 0.4),
    relpos = list(c(1, 2, 3), c(2, 3, 4, 5)),
    gamma  = 0.7,
    R2     = c(0.8, 0.7),
    ntest  = 50
)
relpos <- unname(sobj$relpos)
relpred <- unname(sobj$relpred)

testthat::test_that("Testing of names.", {
    expect_s3_class(sobj, "simrel")
    expect_true(sobj$type == "bivariate")
    expect_subset(names(sobj), c("beta", "beta0", "X", "Y", "Sigma", "type"))
})

testthat::test_that("Test Dimensions.", {
    expect_equal(sobj$p + ncol(sobj$Y), ncol(sobj$Sigma))
    expect_equal(sobj$p + ncol(sobj$Y), nrow(sobj$Sigma))
    expect_equal(nrow(sobj$beta), sobj$p)
    expect_equal(ncol(sobj$beta), ncol(sobj$Y))
    expect_equal(ncol(sobj$X), ncol(sobj$testX))
    expect_equal(ncol(sobj$Y), ncol(sobj$testY))
    expect_equal(nrow(sobj$X), sobj$n)
    expect_equal(nrow(sobj$testX), sobj$call$ntest)
    expect_equal(ncol(sobj$Y), 2)
    expect_equal(ncol(sobj$beta), 2)
})

testthat::test_that("Testing of position indices.", {
    expect_subset(unlist(relpred), unlist(relpos))
    expect_equal(unique(diag(sobj$R2 + sobj$minerror)), 1)
    expect_equal(sum(sobj$q[1:2]) - sobj$q[3], length(unlist(relpred)))
    expect_equal(
        apply(sobj$Sigma[1:2, -c(1:2)], 1, function(x) sum(x != 0)),
        sapply(relpos, length)
    )
    expect_equal(
        apply(sobj$Sigma[1:2, -c(1:2)], 1, function(x) which(x != 0)),
        sobj$relpos
    )
    expect_equal(
        apply(sobj$Sigma[-c(1:2), 1:2], 2, function(x) which(x != 0)),
        sobj$relpos
    )
})

testthat::test_that("Testing different values.", {
    testthat::skip_on_cran()
    expect_equal(sobj$lambda[1], 1)
    expect_equal(sobj$lambda[2], exp(-sobj$gamma))
    expect_equal(sobj$minerror[1, 1], 0.2)
    expect_equal(sobj$minerror[1, 2], 0.09797959)
    expect_equal(sobj$beta[1,1], -0.024117509, tolerance = 1e-5)
})

context("Testing Bivariate Simulation with every components common.")

set.seed(2020)
sobj_expr <- expression({
    bisimrel(
        n      = 100,
        p      = 15,
        q      = c(5, 5, 5),
        rho    = c(0.8, 0.4),
        relpos = list(c(1, 2, 3), c(1, 2, 3)),
        gamma  = 0.7,
        R2     = c(0.8, 0.7),
        ntest  = 50
    )
})
sobj2 <- suppressWarnings(eval(sobj_expr))

testthat::test_that("Testing different values.", {
    testthat::skip_on_cran()
    expect_equal(sobj2$lambda[1], 1)
    expect_equal(sobj2$lambda[2], exp(-sobj2$gamma))
    expect_equal(sobj2$minerror[1, 1], 0.2)
    expect_equal(sobj2$minerror[1, 2], 0.09797959)
    expect_equal(sobj2$beta[1,1], 0.5965721, tolerance = 1e-7)
    expect_equal(sum(simrel:::cov_zy(sobj2)[,1] != 0),
                 sum(simrel:::cov_zy(sobj2)[,2] != 0))
})
