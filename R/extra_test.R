#' @title Extra test functions
#' @importFrom rlang enquo
#' @importFrom testthat expect quasi_label
#' @param object object to test
#' @param expected Expected value
#' @param expected.label Equivalent of `label` for shortcut form.
#' @param label object label. When `NULL`, computed from deparsed object.
#' @param info extra information to be included in the message (useful when writing tests in loops).
#' @examples
#' expect_subset(c(1, 2, 3, 4, 5), c(2, 4, 5))
#' @export

expect_subset <- function(object, expected, info = NULL, label = NULL, expected.label = NULL) {
  act <- quasi_label(enquo(object), label)
  exp <- quasi_label(enquo(expected), expected.label)
  comp <- all(exp$val %in% act$val)
  expect(comp, sprintf("%s does not contain %s.", act$lab, exp$lab), info = info)
  invisible(act$val)
}