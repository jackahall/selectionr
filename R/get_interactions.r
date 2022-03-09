#' Get interactions from Variables
#'
#' @param rhs variables
#' @param max_ints max interaction level
#' @export

get_interactions <- function(rhs, max_ints = 2){
  rhs <- vars_as_str(rhs)
  fun1 <- stats::formula(paste("~", rhs))
  fun2 <- stats::formula(paste("~ (", rhs, ") ^ 2"))
  single_terms <- attr(stats::terms(fun1), "term.labels")
  all_terms <- attr(stats::terms(fun2), "term.labels")
  all_terms <- all_terms[!all_terms%in%single_terms]

  all_terms[stringr::str_count(all_terms, ":") < max_ints]
}
