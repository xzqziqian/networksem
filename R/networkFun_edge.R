#' Fit a sem model with network data using edges as variables. User-specified network statistics will be calculated and used as variables instead of the networks themselves in the SEM.
#' @param model a model specified in lavaan model syntax.
#' @param data a data frame containing the observed non-network nodal variables
#' @param network a named list of networks that will be used in the SEM
#' @param type "difference" for using the difference between the network statistics of the two actors as the edge covariate; "average" for using the average of the network statistics of the two actors as the edge covariate
#' @param ordered parameter same as "ordered" in the lavaan sem() function; whether to treat data as ordinal
#' @param sampling.weights parameter same as "sampling.weights" in the lavaan sem() function; whether to apply weights to data
#' @param group parameter same as "group" in the lavaan sem() function; whether to fit a multigroup model
#' @param cluster parameter same as "cluster" in the lavaan sem() function; whether to fit a cluster model
#' @param constraints parameter same as "constraints" in the lavaan sem() function; whether to apply constraints to the model
#' @param WLS.V parameter same as "WLS.V" in the lavaan sem() function; whether to use WLS.V estimator
#' @param NACOV parameter same as "NACOV" in the lavaan sem() function; whether to use NACOV estimator
#' @param ... optional arguments for the sem() function
#' @return the updated model specification and a lavaan object which is the SEM results, and the data generated
#' @export
sem.net.edge <- function(model = NULL, data = NULL, network = NULL, type = "difference",
                    ordered = NULL, sampling.weights = NULL,
                    group = NULL, cluster = NULL,
                    constraints = "", WLS.V = NULL, NACOV = NULL,
                    ...){
  ## checking proper input
  if(is.null(model)){
    stop("required argument model is not specified.")
  }
  if(is.null(data)){
    stop("required argument data is not specified.")
  }
  if(is.null(network)){
    stop("required argument network is not specified.")
  }

 params <- c(as.list(environment()), list(...))


  ## get the variable names in the model
  model.info <- lavParseModelString(model)
  model.var <- unique(c(model.info$lhs, model.info$rhs))

  ## non-network data variable names
  data.nonnetwork.var <- names(data)

  ## network data variable names
  if (!is.null(network)){
    data.network.var <- names(network)
  }

  ## find the network variables in the model
  model.network.var <- data.network.var[data.network.var %in% model.var]

  ## create variables for network data and model
  ## add network data variables to the non-network data
  model.network.stat.var.list <- list()

  data_edge = data.frame(row_actor=rep(NA, nrow(data)^2), col_actor=rep(NA, nrow(data)^2))

  for (i in 1:length(model.network.var)){
    data_edge[model.network.var[i]]=NA
  }
  if (length(model.network.var)>0){
      for (i in 1:nrow(data)){
        for (j in 1:nrow(data)){
          data_edge[j+(i-1)*nrow(data), "row_actor"]=i
          data_edge[j+(i-1)*nrow(data), "col_actor"]=j
          for (netind in 1:length(model.network.var)){
            data_edge[j+(i-1)*nrow(data),model.network.var[netind]]=network[[netind]][i,j]
          }
        }
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
    if (model.user$lhs[i] %in% colnames(data) && !model.user$lhs[i] %in% model.network.var){
      variables.to.change <- c(variables.to.change, model.user$lhs[i])
    }
    if (model.user$rhs[i] %in% colnames(data) && !model.user$rhs[i] %in% model.network.var){
      variables.to.change <- c(variables.to.change, model.user$rhs[i])
    }
  }


  for (i in 1:length(variables.to.change)){
    data_edge[variables.to.change[i]]=NA
  }

  if (length(variables.to.change)>0){
    for (vind in 1:length(variables.to.change)){
      v_row <- rep(data[variables.to.change[vind]][[1]], each=nrow(data))
      v_col <- rep(data[variables.to.change[vind]][[1]], nrow(data))
      if (type=="difference"){
        data_edge[variables.to.change[vind]] <- v_row - v_col
      }else if (type=="average"){
        data_edge[variables.to.change[vind]] <- (v_row + v_col)/2
      }

    }
  }


  # if (length(variables.to.change)>0){
  #   for (i in 1:nrow(data)){
  #     for (j in 1:nrow(data)){
  #       for (vind in 1:length(variables.to.change)){
  #         data_edge[j+(i-1)*nrow(data),variables.to.change[vind]]=data[i, variables.to.change[vind]]-data[j, variables.to.change[vind]]
  #       }
  #     }
  #   }
  # }
  #

  lavparams <- list()
  for (i in 1:length(params)){
    if (names(params)[i] %in% names(lavOptions())){
      lavparams[[names(params[i])]] <- params[[i]]
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


  list(model=model, estimates=model.res, data=data_edge)
}
