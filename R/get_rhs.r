#' Get RHS of a formula
#'
#' @param formula formula
#' @export

get_rhs <- function(formula){
  paste(attr(stats::terms(formula), "term.labels"), collapse = " + ")
}
