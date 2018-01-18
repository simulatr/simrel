#' Helper Functions
#' @title Some helper function for simulation
#' @name parse_parm
#' @description These function helps to parse a character string into a list object and also creates parameters for performing multiple simulations
#' @aliases parse_parm
#' @aliases prepare_design
#' @importFrom purrr transpose
#'
#'
#' @param character_string A character string for parameter where the items in a list is separated by semicolon. For example: 1, 2; 3, 4
#' @param in_list TRUE if the result need to wrap in a list, default is FALSE
#' @return A list or a vector
#' @examples
#' parse_parm("1, 2; 3, 4")
#' parse_parm("1, 2")
#' @rdname parse_parm
#' @export
parse_parm <- function(character_string, in_list = FALSE){
  x = unlist(strsplit(character_string, ";"))
  x = gsub("[[:space:]]", "", x)
  y = lapply(x, function(y) unlist(strsplit(y, ",")))
  ret = lapply(y, as.numeric)
  if (!grepl(";", character_string)) ret <- ret[[1]]
  if (in_list) ret <- list(ret)
  return(ret)
}

#' @param option_list A list of options that is to be parsed
#' @param tabular logical if output is needed in tabular form or list format
#' @return A list of parsed parameters for simulatr
#' @examples
#' opts <- list(
#'   n = rep(100, 2),
#'   p = c(20, 40),
#'   q = c("5, 5, 4",
#'         "10, 5, 5"),
#'   m = c(5, 5),
#'   relpos = c("1; 2, 4; 3",
#'              "1, 2; 3, 4; 5"),
#'   gamma = c(0.2, 0.4),
#'   R2 = c("0.8, 0.9, 0.7",
#'          "0.6, 0.8, 0.7"),
#'   ypos = c("1, 4; 2, 5; 3",
#'            "1; 2, 4; 3, 5"),
#'   ntest = rep(1000, 2)
#' )
#' design <- prepare_design(opts)
#' @rdname parse_parm
#' @export
prepare_design <- function(option_list, tabular = T){
  n_design <- max(sapply(option_list, length))
  ret <- lapply(option_list, function(x){
    lapply(x, function(y){
      if (is.character(y)) y <- parse_parm(y)
      return(y)
    })
  })
  ret <- transpose(ret)
  names(ret) <- paste0("Design", 1:n_design)
  return(ret)
}
