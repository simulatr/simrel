library(simrel)
library(testthat)

context("Test parameter-parser and design-creator functions")

opts <- list(
  n      = rep(100, 2),
  p      = c(20, 40),
  q      = c("5, 5, 4", "10, 5, 5"),
  m      = c(5, 5),
  relpos = c("1; 2, 4; 3", "1, 2; 3, 4; 5"),
  gamma  = c(0.2, 0.4),
  R2     = c("0.8, 0.9, 0.7", "0.6, 0.8, 0.7"),
  ypos   = c("1, 4; 2, 5; 3", "1; 2, 4; 3, 5"),
  ntest  = rep(1000, 2)
)
dgn <- tibble::tibble(
  n = c(100, 100),
  p = c(20, 40),
  q = list(Design1 = c(5,  5, 4), Design2 = c(10, 5, 5)),
  m = c(5, 5),
  relpos = list(
    Design1 = list(1, c(2, 4), 3),
    Design2 = list(c(1, 2), c(3, 4), 5)),
  gamma = c(0.2,  0.4),
  R2 = list(Design1 = c(0.8, 0.9, 0.7),
            Design2 = c(0.6,  0.8, 0.7)),
  ypos = list(Design1 = list(c(1, 4), c(2, 5), 3),
              Design2 = list(1, c(2, 4), c(3, 5))),
  ntest = c(1000, 1000)
)

testthat::test_that(
  "Test if parameters are parsed correctly or not.", {
    expect_equal(parse_parm("1, 2, 3, 4"), c(1, 2, 3, 4))
    expect_equal(parse_parm("1, 2; 3, 4"), list(1:2, 3:4))
    expect_equal(parse_parm("1, 2, ;"), list(1:2))
    expect_equal(parse_parm("1;2"), list(1, 2))
  }
)

testthat::test_that(
  "Prepare Design", {
    expect_identical(prepare_design(opts), dgn)
    expect_identical(
      prepare_design(list(relpos = "1, 2, 3; 4, 5, 6")),
      tibble::tibble(relpos = list(Design1 = list(c(1, 2, 3), c(4, 5, 6))))
    )
    expect_error(
      prepare_design(list("1, 2, 3")),
      "Use .name_repair to specify repair."
    )
  }
)
