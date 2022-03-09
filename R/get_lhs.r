#' Get LHS of a formula
#'
#' @param formula formula
#' @export

get_lhs <- function(formula){
  ss <- deparse(formula[2])
  substr(ss, 1, nchar(ss) - 2)
}
