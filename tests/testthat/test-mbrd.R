suppressPackageStartupMessages(library(simrel))
suppressPackageStartupMessages(library(testthat))

context("Test MBR design creating function")

design1 <- mbrd(l2levels = c(2,2), fraction = 0)
design2 <- mbrd(l2levels = c(4, 2, 2), fraction = 3)

testthat::test_that(
  "Test mbrd function.", {
    expect_named(design1, c("BitDesign", "Design"))
    expect_error(mbrd(l2levels = c(2, 2), fraction = 3),
                 "runs are not covered by FrF2.")
    expect_equal(nrow(design1$BitDesign), 16)
    expect_equal(ncol(design1$BitDesign), 4)
    expect_equal(nrow(design2$BitDesign), 32)
    expect_equal(ncol(design2$BitDesign), 8)
    expect_equal(nrow(design1$BitDesign), nrow(design1$Design))
    expect_equal(nrow(design2$BitDesign), nrow(design2$Design))
    expect_equal(ncol(design1$BitDesign), 2^ncol(design1$Design))
    expect_equal(ncol(design2$BitDesign), 2^ncol(design2$Design))
  }
)

sim_list1 <- list(
  p = c(50, 100, 500, 1000),
  gamma = seq(0.2, 1.1, length.out = 4),
  m = c(4, 8, 4, 8)
)
sim_list2 <- list(
  p = c(50, 100)
)
sim_list3 <- list(
  p = c(50, 100),
  gamma = seq(0.2, 1.1, length.out = 3)
)

testthat::test_that(
  "Test function creating MBR simulation design.", {
    expect_true(all(sapply(sim_list1, length) == length(sim_list1[[1]])))
    expect_equal(ncol(mbrdsim(sim_list1, 2)[["BitDesign"]]), 6)
    expect_equal(nrow(mbrdsim(sim_list1, 2)[["BitDesign"]]), 16)
    expect_equal(ncol(mbrdsim(sim_list1, 2)[["Design"]]), 3)
    expect_equal(nrow(mbrdsim(sim_list1, 2)[["Design"]]), 16)
    expect_error(mbrdsim(sim_list2, 1), "runs are not covered by FrF2")
    expect_error(mbrdsim(sim_list2), "argument \"fraction\" is missing")
    expect_error(mbrdsim(sim_list3, 1), "nruns must be a power of 2.")
  }
)

