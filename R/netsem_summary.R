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

  # cat ("summary from using network data in SEM:\n")

  if (otype=="obs"){
    sem.out <- lavaan::parameterestimates(res$estimates)
    cat(sem.out[1,1])
    cat("SEM estimates with network variables: \n")
    for (col in colnames(sem.out)){
      cat(col)
      cat("\t")
    }
    for (i in 1:nrow(sem.out)){
      for (j in 1:ncol(sem.out)){
        cat(sem.out[i,j])
        cat("\t")
      }
      cat(sem.out[i,1])
      cat("\n")
    }
  }else{
    sem.out <- lavaan::parameterestimates(res$estimates$sem.es)
    cat(sem.out[1,1])
    cat("SEM estimates with network variables: \n")
    for (col in colnames(sem.out)){
      cat(col)
      cat("\t")
    }
    for (i in 1:nrow(sem.out)){
      for (j in 1:ncol(sem.out)){
        cat(sem.out[i,j])
        cat("\t")
      }
      cat(sem.out[i,1])
      cat("\n")
    }




  }



}


# conv <- function(res){
#   otype = "obs"
#   if (class(res$estimates)=="list"){
#     otype = "lsm"
#   }
#
#   out = list()
#   if (otype=="obs"){
#     out[[1]] = capture.output(summary(res$estimates))
#   } else {
#     out[[1]] = (summary(res$estimates$sem.es))
#     for (i in 1:length(res$estimates$lsm.es)){
#       out[[i+1]] = capture.output(summary(res$estimates$lsm.es[[i]]))
#     }
#   }
#   return(out)
# }
