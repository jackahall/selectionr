#' S3 method for print for fs class
#'
#' @param x x
#' @param digits digits
#' @param ... ...
#' @export

print.fs <- function(x, digits = 3, ...){
  x <- list("Information Critera and Model Selection Parameters" = x$ic %>%
              mutate_if(is.numeric,
                        round,
                        digits = digits))
  NextMethod(right = FALSE, ...)
}
