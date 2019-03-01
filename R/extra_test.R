#' Extra Test Functions
#' @importFrom testthat expect quasi_label
#' @importFrom rlang enquo
#' @export
expect_dim <- function(object, n) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object))
  
  # 2. Call expect()
  act$n <- dim(act$val)
  expect(
    all(act$n == n),
    sprintf("%s has dim %i, not dim %i.", act$lab, act$n, n)
  )
  
  # 3. Invisibly return the value
  invisible(act$val)
}