suppressPackageStartupMessages(library(simrel))
suppressPackageStartupMessages(library(testthat))

context("Testing Custom Tests.")

testthat::test_that(
  "Test expect_subset", {
    expect_subset(names(mtcars), c("mpg", "hp"))
  }
)