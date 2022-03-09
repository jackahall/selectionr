#' Formula Variables to String
#'
#' @param vars_vect variables
#' @export

vars_as_str <- function(vars_vect){
  if(length(vars_vect) == 1){
    vars_vect
  } else {
    paste(vars_vect, collapse = " + ")
  }
}
