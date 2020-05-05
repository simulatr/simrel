library(simrel)
library(testthat)

context("Testing Custom Tests.")

testthat::test_that(
  "Test expect_subset", {
    expect_success(expect_subset(names(mtcars), c("mpg", "hp")))
    expect_error(expect_subset(list(1, 2, 3), list(1, 2, 3, 4)),
                 class = "expectation_failure")
    expect_success(expect_subset(1:5, 2:4))
    expect_success(expect_subset(LETTERS, toupper(letters)))
  }
)