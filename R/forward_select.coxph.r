#' Forward Selection for Cox-PH
#'
#' @param formula The formula for the starting point model
#' @param data The dataset to use
#' @param test A vector of test variables
#' @param sort Sort output by this variable
#' @param rev Reverse output
#' @param max_ints Maximum level of interactions
#' @param complete_cases Only include complete cases of data
#' @param main_fx Require all main effects in order to consider interactions. Default TRUE
#' @param ... ...
#'
#' @return An fs object
#' @import lmtest survival dplyr gtools stringr
#' @export

forward_select.coxph <- function(formula, data, test,
                                 sort = NULL, rev = FALSE, max_ints = 2,
                                 complete_cases = NULL, main_fx = TRUE, ...){

  ### Sorting variables

  rev_sorts <- c("n", "concordance", "lrt_chisq")
  full_set_sorts <- c("lrt_df", "lrt_chisq", "lrt_p")

  ### Complete cases based on control variable and sort variable

  if(is.null(complete.cases)){
    if(!is.null(sort)){
      if(sort %in% full_set_sorts){
        data <- data[stats::complete.cases(data), ]
      }
    }
  } else if(complete.cases){
    data <- data[stats::complete.cases(data), ]
  }

  ### model0

  model0 <- survival::coxph(formula(formula), data = data, ...)

  vars_test <- vars_as_vect(test)
  model0_vars <- get_rhs(model0)

  if(main_fx){
    model0_ints <- get_interactions(model0_vars, max_ints)
  } else {
    model0_ints <- get_interactions(c(model0_vars, test[!test %in% model0_vars]), max_ints)
  }

  vars_test <- vars_test[!vars_test %in% vars_as_vect(model0_vars)]
  vars_test <- c("1", vars_test, model0_ints)

  lhs <- get_lhs(formula)

  label <- vector()
  n <- vector()
  aic <- vector()
  bic <- vector()
  conc <- vector()
  models <- list()

  lrt_m0 <- list()

  lrt_df_m0 <- vector()
  lrt_chisq_m0 <- vector()
  lrt_p_m0 <- vector()
  lrt_ps_m0 <- vector()

  for(i in 1:length(vars_test)){
    if(vars_test[i] == "1"){
      label[i] <- sub("     ", "", Reduce(paste, deparse(formula(model0))))
      new.model <- model0
      models[[i]] <- new.model
    } else {
      label[i] <- paste(lhs, "~", vars_as_str(model0_vars), "+", vars_test[i])
      new.model <- survival::coxph(stats::as.formula(label[i]), data)
      models[[i]] <- new.model
    }

    n[i] <- new.model$n
    aic[i] <- stats::AIC(new.model)
    bic[i] <- stats::BIC(new.model)
    conc[i] <- survival::concordance(new.model)$concordance

    if(model0$n == new.model$n){
      new.lrt <- lmtest::lrtest(model0, new.model)

      lrt_m0[[i]] <- new.lrt
      lrt_df_m0[i] <- new.lrt[2, 3]
      lrt_chisq_m0[i] <- new.lrt[2, 4]
      lrt_p_m0[i] <- new.lrt[2, 5]
      lrt_ps_m0[i] <- gtools::stars.pval(new.lrt[2, 5])
    } else {
      lrt_df_m0[i] <- NA
      lrt_chisq_m0[i] <- NA
      lrt_p_m0[i] <- NA
      lrt_ps_m0[i] <- NA
    }


  }

  names(models) <- label

  ic <- data.frame(list("Model" = label,
                        "n" = n,
                        "aic" = aic,
                        "bic" = bic,
                        "concordance" = conc,
                        "lrt_df" = lrt_df_m0,
                        "lrt_chisq" = lrt_chisq_m0,
                        "lrt_p" = lrt_p_m0,
                        "sig" = lrt_ps_m0))

  if(!is.null(sort)){
    if(sort %in% rev_sorts){
      rev <- TRUE
    }

    ic <- ic[order(ic[, sort]),]
  }

  if(rev){
    ic <- ic[nrow(ic):1, ]
  }

  rtn <- list("ic" = ic,
              "test_models" = models,
              "model" = models[[1]],
              "lrt" = lrt_m0)

  attr(rtn, "class") <- c("fs")

  rtn
}
