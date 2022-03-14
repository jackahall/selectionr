#' Wrapper for Select Function
#'
#' @param formula The formula for the starting point model
#' @param data The dataset to use
#' @param test A vector of test variables
#' @param method Method to use
#' @param family Not used
#' @param sort Sort output by this variable
#' @param rev Reverse output
#' @param max_ints Maximum level of interactions
#' @param complete_cases Only include complete cases of data
#' @param main_fx Require all main effects in order to consider interactions. Default TRUE
#' @param ... ...
#'
#' @return An fs object
#' @export

forward_select <- function(formula, data, test, method = NULL, family = NULL,
                           sort = NULL, rev = FALSE, max_ints = 2,
                           complete_cases = NULL, main_fx = TRUE, ...){
  if(method == "lm"){
    forward_select.lm(formula, data, test,
                      sort, rev, max_ints,
                      complete_cases, main_fx, ...)
  } else if(method == "glm"){
    forward_select.glm(formula, data, test, family,
                       sort, rev, max_ints,
                       complete_cases, main_fx, ...)
  } else if (method == "coxph"){
    forward_select.coxph(formula, data, test,
                         sort, rev, max_ints,
                         complete_cases, main_fx, ...)
  } else {
    print("Error: invalid 'method'. Please try with 'lm', 'glm' or 'coxph'.")
  }
}
