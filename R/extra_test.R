expect_subset <- function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) {
  act <- quasi_label(enquo(object), label)
  exp <- quasi_label(enquo(expected), expected.label)
  comp <- all(exp$val %in% act$val)
  expect(comp, sprintf("%s does not contain %s.", act$lab, exp$lab), info = info)
  invisible(act$val)
}