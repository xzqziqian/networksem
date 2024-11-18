#' Fit a sem model with network data using edges as variables. User-specified network statistics will be calculated and used as variables instead of the networks themselves in the SEM.
#' @param model a model specified in lavaan model syntax.
#' @param data a list containing both the non-network and network data
#' @param type "difference" for using the difference between the network statistics of the two actors as the edge covariate; "average" for using the average of the network statistics of the two actors as the edge covariate
#' @param ordered parameter same as "ordered" in the lavaan sem() function; whether to treat data as ordinal
#' @param data.rescale whether to rescale the whole dataset (with restructured network and nonnetwork data) to have mean 0 and standard deviation 1 when fitting it to SEM, default to FALSE
#' @param netstats.rescale a logical value indicating whether to rescale network statistics or variables to have mean 0 and sd 1
#' @param sampling.weights parameter same as "sampling.weights" in the lavaan sem() function; whether to apply weights to data
#' @param group parameter same as "group" in the lavaan sem() function; whether to fit a multigroup model
#' @param cluster parameter same as "cluster" in the lavaan sem() function; whether to fit a cluster model
#' @param constraints parameter same as "constraints" in the lavaan sem() function; whether to apply constraints to the model
#' @param WLS.V parameter same as "WLS.V" in the lavaan sem() function; whether to use WLS.V estimator
#' @param NACOV parameter same as "NACOV" in the lavaan sem() function; whether to use NACOV estimator
#' @param ... optional arguments for the sem() function
#' @return the updated model specification and a lavaan object which is the SEM results, and the data generated
#' @export
sem.net.edge <- function(model = NULL, data = NULL, type = "difference",
                    ordered = NULL, sampling.weights = NULL, data.rescale = FALSE,
                    group = NULL, cluster = NULL, netstats.rescale = FALSE,
                    constraints = "", WLS.V = NULL, NACOV = NULL,
                    ...){
  ## checking proper input
  if(is.null(model)){
    stop("required argument model is not specified.")
  }
  if(is.null(data)){
    stop("required argument data is not specified.")
  }


 params <- c(as.list(environment()), list(...))


  ## get the variable names in the model
  model.info <- lavParseModelString(model)
  model.var <- unique(c(model.info$lhs, model.info$rhs))

  ## non-network data variable names
  data.nonnetwork.var <- names(data$nonnetwork)

  ## network data variable names
  if (!is.null(data$network)){
    data.network.var <- names(data$network)
  }

  ## find the network variables in the model
  model.network.var <- data.network.var[data.network.var %in% model.var]

  ## create variables for network data and model
  ## add network data variables to the non-network data
  model.network.stat.var.list <- list()
  data_edge = data.frame(row_actor=rep(NA, nrow(data$nonnetwork)^2), col_actor=rep(NA, nrow(data$nonnetwork)^2))
  for (i in 1:length(model.network.var)){
    data_edge[model.network.var[i]]=NA
  }
  if (length(model.network.var)>0){
      for (i in 1:nrow(data$nonnetwork)){
        for (j in 1:nrow(data$nonnetwork)){
          data_edge[j+(i-1)*nrow(data$nonnetwork), "row_actor"]=i
          data_edge[j+(i-1)*nrow(data$nonnetwork), "col_actor"]=j
          for (netind in 1:length(model.network.var)){
            data_edge[j+(i-1)*nrow(data$nonnetwork), model.network.var[netind]]=data$network[[netind]][i,j]

          }
        }
      }
  }

  if(netstats.rescale){
    for (netind in 1:length(model.network.var)){
      data_edge[model.network.var[netind]]=scale(data_edge[model.network.var[netind]], center=TRUE, scale=TRUE)
    }
  }



  #print(model.network.stat.var.list)
  ## reconstruct the path model with the network variables
  ## replace the network variable name with the network variable stats name

  ## lavaanify the model
  model.lavaanify <- lavaanify(model)

  ## get the use specified model information
  model.user <- model.lavaanify[model.lavaanify$user==1, ]

  ## now process each part of the user specified model
  model.to.remove.index <- NULL
  model.to.add <- ""

  variables.to.change=c()
  for (i in 1:nrow(model.user)){
    ## check if the variable on the lhs is a nonnetwork variable
    if (model.user$lhs[i] %in% colnames(data$nonnetwork) && !model.user$lhs[i] %in% model.network.var){
      variables.to.change <- c(variables.to.change, model.user$lhs[i])
    }
    if (model.user$rhs[i] %in% colnames(data$nonnetwork) && !model.user$rhs[i] %in% model.network.var){
      variables.to.change <- c(variables.to.change, model.user$rhs[i])
    }
  }


  for (i in 1:length(variables.to.change)){
    data_edge[variables.to.change[i]]=NA
  }

  if (length(variables.to.change)>0){
    for (vind in 1:length(variables.to.change)){
      v_row <- rep(data$nonnetwork[variables.to.change[vind]][[1]], each=nrow(data$nonnetwork))
      v_col <- rep(data$nonnetwork[variables.to.change[vind]][[1]], nrow(data$nonnetwork))
      if (type=="difference"){
        data_edge[variables.to.change[vind]] <- v_row - v_col
      }else if (type=="average"){
        data_edge[variables.to.change[vind]] <- (v_row + v_col)/2
      }

    }
  }



  lavparams <- list()
  for (i in 1:length(params)){
    if (names(params)[i] %in% names(lavOptions())){
      lavparams[[names(params[i])]] <- params[[i]]
    }
  }

  if (data.rescale){
    for (i in 1:ncol(data_edge)){
      if (is.numeric(data_edge[,i])){
        data_edge[,i] <- scale(data_edge[,i], center = TRUE, scale = TRUE)
      }
    }
  }


  lavparams[["data"]] <- data_edge
  lavparams[["model"]] <- model
  lavparams[["ordered"]] <- ordered
  lavparams[["sampling.weights"]] <- sampling.weights
  lavparams[["group"]] <- group
  lavparams[["cluster"]] <- cluster
  lavparams[["constraints"]] <- constraints
  lavparams[["WLS.V"]] <- WLS.V
  lavparams[["NACOV"]] <- NACOV

  model.res <- do.call(what="sem", args=c(lavparams))


  obj <- list(model=model, estimates=model.res, data=data_edge)
  class(obj) <- "networksem"
  return(obj)
}
