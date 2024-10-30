
#' Fit a sem model with network data using node latent positions and/or network statistics as variables. User-specified network statistics will be calculated and used as variables instead of the networks themselves in the SEM.
#' @param model a model specified in lavaan model syntax.
#' @param data a list containing both the non-network and network data
#' @param netstats.rescale a logical value indicating whether to rescale network statistics or variables to have mean 0 and sd 1
#' @param data.rescale whether to rescale the whole dataset (with restructured network and nonnetwork data) to have mean 0 and standard deviation 1 when fitting it to SEM, default to FALSE
#' @param ordered parameter same as "ordered" in the lavaan sem() function; whether to treat data as ordinal
#' @param sampling.weights parameter same as "sampling.weights" in the lavaan sem() function; whether to apply weights to data
#' @param group parameter same as "group" in the lavaan sem() function; whether to fit a multigroup model
#' @param cluster parameter same as "cluster" in the lavaan sem() function; whether to fit a cluster model
#' @param constraints parameter same as "constraints" in the lavaan sem() function; whether to apply constraints to the model
#' @param WLS.V parameter same as "WLS.V" in the lavaan sem() function; whether to use WLS.V estimator
#' @param NACOV parameter same as "NACOV" in the lavaan sem() function; whether to use NACOV estimator
#' @param latent.dim number of network latent dimensions to use
#' @param ... optional arguments for the sem() function
#' @return the updated model specification with the network statistics as variables and a lavaan object which is the SEM results
#' @export
sem.net.lsm <- function(model=NULL, data=NULL, latent.dim = 2,
                    ordered = NULL, sampling.weights = NULL, data.rescale=FALSE,
                    netstats.rescale=FALSE, group = NULL, cluster = NULL,
                    constraints = "", WLS.V = NULL, NACOV = NULL,
                    netstats.options=NULL, ...){
  ## checking proper input
  if(is.null(model)){
    stop("required argument model is not specified.")
  }
  if(is.null(data)){
    stop("required argument data is not specified.")
  }

 params <- c(as.list(environment()), list(...))


  ## get the variable names in the model
  model.info <- lavaan::lavParseModelString(model)
  model.var <- unique(c(model.info$lhs, model.info$rhs))

  ## non-network data variable names
  data.nonnetwork.var <- names(data$nonnetwork)

  ## network data variable names
  if (!is.null(data$network)){
    data.network.var <- names(data$network)
  }
  latent.network = data.network.var

  ## find the network variables in the model
  model.network.var <- data.network.var[data.network.var %in% model.var]

  ## create variables for network data and model
  ## add network data variables to the non-network data

  latent.vars <- list()
  model.lavaanify <- lavaan::lavaanify(model)

  ## get the use specified model information
  model.user <- model.lavaanify[model.lavaanify$user==1, ]


  ## estimate network latent positions
  lsm.fits <- list()
  for (i in 1:length(latent.network)){
    fit <- latentnet::ergmm(network::network(data$network[[latent.network[i]]]) ~ euclidean(d = latent.dim))
    lsm.fits[[i]] <-fit
    latent.vars[[latent.network[i]]] <- c()
    for (dimind in 1:latent.dim){
      data$nonnetwork[paste0(latent.network[i], ".Z", dimind)] <- fit$mcmc.mle$Z[,dimind]
      if (netstats.rescale){
        data$nonnetwork[paste0(latent.network[i], ".Z", dimind)] <- scale(fit$mcmc.mle$Z[,dimind], center = TRUE, scale = TRUE)
      }
      latent.vars[[latent.network[i]]] <- c(latent.vars[[latent.network[i]]], paste0(latent.network[i], ".Z", dimind))
    }
  }

  # print(lsm.fits)




  ## reconstruct the path model with the network variables
  ## replace the network variable name with the network variable stats name

  ## lavaanify the model
  model.lavaanify <- lavaan::lavaanify(model)

  ## get the use specified model information
  model.user <- model.lavaanify[model.lavaanify$user==1, ]


  ## now process each part of the user specified model
  model.to.remove.index <- NULL
  model.to.add <- ""
  for (i in 1:nrow(model.user)){
    ## check if left is network with LSM, remake
    if (model.user$lhs[i] %in% latent.network){
      model.to.remove.index <- c(model.to.remove.index, i)
      model.stat.var.to.add <- latent.vars[[model.user$lhs[i]]]
      for (j in 1:length(model.stat.var.to.add)){
        model.temp <- paste0("\n ", model.stat.var.to.add[j], model.user$op[i], model.user$rhs[i])
        model.to.add <- paste0(model.to.add, model.temp)
      }
    }
    ## check if right is network with LSM and left is other variables
    if (model.user$rhs[i] %in% latent.network){
      model.to.remove.index <- c(model.to.remove.index, i)
      model.stat.var.to.add <- latent.vars[[model.user$rhs[i]]]
      for (j in 1:length(model.stat.var.to.add)){
        model.temp <- paste0("\n ",  model.user$lhs[i], model.user$op[i],  model.stat.var.to.add[j])
        model.to.add <- paste0(model.to.add, model.temp)
      }
    }
    ## left is latent position and right is others
    # if (grepl("lp.", model.user$rhs[i], fixed = TRUE) && (!(model.user$lhs[i] %in% model.network.var))){
    #   ## if it is, record the index i and create new model items
    #   model.to.remove.index <- c(model.to.remove.index, i)
    #   model.stat.var.to.add <- latent.vars[[substring(model.user$rhs[i], 4)]]
    #   for (j in 1:length(model.stat.var.to.add)){
    #     model.temp <- paste0("\n ", model.user$lhs[i], model.user$op[i], model.stat.var.to.add[j])
    #     model.to.add <- paste0(model.to.add, model.temp)
    #   }
    # }
  }

  model.remove.network.var <- model.user[-model.to.remove.index, ]
  model.non.network.var <- ""
  for (i in 1:nrow(model.remove.network.var)){
    model.non.network.var.temp <- paste0(paste0(model.remove.network.var[i, c('lhs', 'op', 'rhs')], collapse = ' '))
    model.non.network.var <- paste0(model.non.network.var.temp, "\n", model.non.network.var)
  }

  model.full <- paste0(model.non.network.var, "\n", model.to.add)



  lavparams <- list()
  for (i in 1:length(params)){
    if (names(params)[i] %in% names(lavOptions())){
      lavparams[[names(params[i])]] <- params[[i]]
    }
  }

  if (data.rescale){
    for (i in 1:ncol(data$nonnetwork)){
      if (is.numeric(data$nonnetwork[,i])){
        data$nonnetwork[,i] <- scale(data$nonnetwork[,i], center = TRUE, scale = TRUE)
      }
    }
  }

  lavparams[["data"]] <- data$nonnetwork
  lavparams[["model"]] <- model.full
  lavparams[["ordered"]] <- ordered
  lavparams[["sampling.weights"]] <- sampling.weights
  lavparams[["group"]] <- group
  lavparams[["cluster"]] <- cluster
  lavparams[["constraints"]] <- constraints
  lavparams[["WLS.V"]] <- WLS.V
  lavparams[["NACOV"]] <- NACOV

  model.res <- do.call(what="sem", args=c(lavparams))


  obj <- list(model=model.full, estimates=list(sem.es=model.res,lsm.es=lsm.fits), data=data)
  class(obj) <- "networksem"
  return(obj)
}



