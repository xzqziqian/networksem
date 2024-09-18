#' Summarize output from networksem
#' @param res a networksem output file
#' @return a summary of the networksem output
#' @export
summary.networksem <- function(res){

  otype = "obs"
  if (class(res$estimates)=="list"){
    otype = "lsm"
  }



  cat("The reconstructed model:\n")
  cat(res$model)
  cat("\n\n")


  if (otype == "obs"){
    lvsummary <- getMethod("summary",signature(object="lavaan"))
    cat("The SEM output:\n")
    print(lvsummary(res$estimates, fit = T))
  }else{
    lvsummary <- getMethod("summary",signature(object="lavaan"))
    cat("The SEM output:\n")
    print(lvsummary(res$estimates$sem.es, fit = T))

    lvsummary <- getMethod("summary",signature(object="lavaan"))
    cat("The LSM output:\n")
    for (lsmout in res$estimates$lsm.es){
      print(summary(lsmout))
    }
  }
}


