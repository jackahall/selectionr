#' Formula Variables to Vector
#'
#' @param vars_str variables
#' @export

vars_as_vect <- function(vars_str){
  if(length(vars_str) == 1){
    attr(stats::terms(stats::formula(paste("~", vars_str))), "term.labels")
  } else {
    vars_str
  }
}
