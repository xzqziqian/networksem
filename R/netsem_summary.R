#' Summarize output from networksem
#' @param object a networksem output
#' @param ... optional arguments
#' @return a summary of the output
#' @export
summary.networksem <- function(object,...){

  res = object
  otype = "obs"
  if (class(res$estimates)=="list"){
    otype = "lsm"
  }

#   cat("The reconstructed model:\n")
#   cat(res$model)
#   cat("\n\n")

  if (otype == "obs"){
    lvsummary <- getMethod("summary",signature(object="lavaan"))
    cat("The SEM output:\n")
    print(lvsummary(res$estimates, fit = T))
  }else{
    lvsummary <- getMethod("summary",signature(object="lavaan"))

    # just fit info
    semsum <- lvsummary(res$estimates$sem.es)
    teststats <- semsum$test$standard$stat
    df <- semsum$test$standard$df
    pval <- semsum$test$standard$pval
    cat("Model Fit Information")
    cat("SEM Test statistics: ", teststats, "on", df, "df with p-value: ", pval, "\n")


    for (i in 1:length(res$estimates$lsm.es)){
      lsmsum <- summary(res$estimates$lsm.es[[i]])
      bic <- lsmsum$bic$overall
      cat("network", i, "LSM BIC: ", bic, "\n")
    }
    cat("======================================== \n")
    cat("========================================\n\n")

    # full output
    cat("The SEM output:\n")
    print(lvsummary(res$estimates$sem.es, fit = T))
    cat("The LSM output:\n")
    for (lsmout in res$estimates$lsm.es){
      print(summary(lsmout))
    }
  }
}
.S3method("summary", "networksem", "summary.networksem")


#' Calculate a mediation effect from a networksem model
#' @param res a networksem output file
#' @param predictor a character string of the predictor variable
#' @param mediator a character string of the mediator variable
#' @param outcome a character string of the outcome variable
#' @return a target path, associated estimates, and z-score
#' @export
path.networksem <- function(res, predictor, mediator, outcome){
  # only allow 1 predictor and 1 outcome
  if (length(predictor) > 1 | length(outcome) > 1){
    stop("Only 1 predictor and 1 outcome are allowed")
  }

  otype = "obs"
  if (class(res$estimates)=="list"){
    otype = "lsm"
  }

  if (otype == "obs"){
    pars <- parameterEstimates(res$estimates)
    covm <- vcov(res$estimates)
  }else{
    pars <- parameterEstimates(res$estimates$sem.es)
    covm <- vcov(res$estimates$sem.es)
  }


  effect_table <- expand.grid("predictor" = predictor,
                              "mediator" = mediator,
                              "outcome" = outcome)
  effect_table$apath = NA; effect_table$bpath = NA; effect_table$indirect = NA;
  effect_table$indirect_se = NA; effect_table$indirect_z = NA;


  for (i in 1:nrow(effect_table)){

    # estimates of a and b paths
    effect_table$apath[i] <- pars[pars$rhs == effect_table$predictor[i] & pars$lhs == effect_table$mediator[i], "est"]
    effect_table$bpath[i] <- pars[pars$rhs == effect_table$mediator[i] & pars$lhs == effect_table$outcome[i], "est"]
    effect_table$indirect[i] <- effect_table$apath[i]*effect_table$bpath[i]


    # indirect effect se usinh sobel
    headera = paste0(effect_table$mediator[i], "~", effect_table$predictor[i])
    headerb = paste0(effect_table$outcome[i], "~", effect_table$mediator[i])
    abcov = covm[c(headera, headerb), c(headera, headerb)]
    a <- effect_table$apath[i]
    b <- effect_table$bpath[i]
    ab_vector <- c(a, b)
    var_ab <- t(ab_vector) %*% abcov %*% ab_vector
    effect_table$indirect_se[i] <- sqrt(var_ab)
    effect_table$indirect_z[i] <- effect_table$indirect[i]/effect_table$indirect_se[i]

  }

  return(effect_table)
}

