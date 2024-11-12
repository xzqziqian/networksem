#' Summarize output from networksem
#' @param res a networksem output file
#' @return a summary of the networksem output
#' @export
summary.networksem <- function(res){

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

#' Summarize output from networksem
#' @param res a networksem output file
#' @return a target path
#' @export
path.networksem <- function(res, predictor, mediator, outcome){

  otype = "obs"
  if (class(res$estimates)=="list"){
    otype = "lsm"
  }

  if (otype == "obs"){
    pars <- parameterEstimates(res$estimates)
  }else{
    pars <- parameterEstimates(res$estimates$sem.es)
  }
  effect_table <- expand.grid("predictor" = predictor,
                              "mediator" = mediator,
                              "outcome" = outcome)
  effect_table$apath = NA; effect_table$bpath = NA; effect_table$indirect = NA
  effect_table$apath_se = NA; effect_table$bpath_se = NA;
  for (i in 1:nrow(effect_table)){
    effect_table$apath[i] <- pars[pars$rhs == effect_table$predictor[i] & pars$lhs == effect_table$mediator[i], "est"]
    effect_table$apath_se[i] <- pars[pars$rhs == effect_table$predictor[i] & pars$lhs == effect_table$mediator[i], "se"]
    effect_table$bpath[i] <- pars[pars$rhs == effect_table$mediator[i] & pars$lhs == effect_table$outcome[i], "est"]
    effect_table$bpath_se[i] <- pars[pars$rhs == effect_table$mediator[i] & pars$lhs == effect_table$outcome[i], "se"]
    effect_table$indirect[i] <- effect_table$apath[i]*effect_table$bpath[i]
  }

  # print(effect_table)
  return(effect_table)
}

