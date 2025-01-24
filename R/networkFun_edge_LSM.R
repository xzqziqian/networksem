#' Fit a Structural Equation Model (SEM) with both network and non-network data by transforming nonnetwork data into paired values corresponding to network latent distance pairs.
#' @importFrom latentnet ergmm
#' @import latentnet
#' @param model A model string specified in lavaan model syntax that includes relationships among the network and non-network variables.
#' @param data A list containing the data. The list has two named components, "network" and "nonnetwork"; "network" is a list of named adjacency matrices for the network data, and "nonnetwork" is the dataframe of non-network covariates.
#' @param type "difference" for using the difference between the network statistics of the two actors as the edge covariate; "average" for using the average of the network statistics of the two actors as the edge covariate.
#' @param latent.dim The number of network latent dimensions to use in extracting latent positions of network nodes.
#' @param netstats.rescale TRUE or FALSE, whether to rescale the network statistics to have mean 0 and standard deviation 1, default to FALSE.
#' @param data.rescale TRUE or FALSE, whether to rescale the whole dataset (with restructured network and nonnetwork data) to have mean 0 and standard deviation 1 when fitting it to SEM, default to FALSE.
#' @param ordered Parameter same as "ordered" in the lavaan sem() function; whether to treat data as ordinal.
#' @param sampling.weights Parameter same as "sampling.weights" in the lavaan sem() function; whether to apply weights to data.
#' @param group Parameter same as "group" in the lavaan sem() function; whether to fit a multigroup model.
#' @param cluster Parameter same as "cluster" in the lavaan sem() function; whether to fit a cluster model.
#' @param constraints Parameter same as "constraints" in the lavaan sem() function; whether to apply constraints to the model.
#' @param WLS.V Parameter same as "WLS.V" in the lavaan sem() function; whether to use WLS.V estimator.
#' @param NACOV Parameter same as "NACOV" in the lavaan sem() function; whether to use NACOV estimator.
#' @param ... Optional arguments for the sem() function.
#' @return A networksem object containing the updated model specification string with the reconstructed network statistics as variables, a lavaan SEM output object, and a latentnet ergm object.
#' @export
#' @examples
#' set.seed(10)
#' nsamp = 20
#' lv1 <- rnorm(nsamp)
#' net <- ifelse(matrix(rnorm(nsamp^2) , nsamp, nsamp) > 1, 1, 0)
#' lv2 <- rnorm(nsamp)
#' nonnet <- data.frame(x1 = lv1*0.5 + rnorm(nsamp),
#'                      x2 = lv1*0.8 + rnorm(nsamp),
#'                      x3 = lv2*0.5 + rnorm(nsamp),
#'                      x4 = lv2*0.8 + rnorm(nsamp))
#'
#' model <-'
#'   lv1 =~ x1 + x2
#'   lv2 =~ x3 + x4
#'   net ~ lv1
#'   lv2 ~ net
#' '
#' data = list(network = list(net = net), nonnetwork = nonnet)
#' set.seed(100)
#' res <- sem.net.edge.lsm(model = model, data = data, latent.dim = 1)
#' summary(res)
sem.net.edge.lsm <- function(model=NULL, data=NULL, type="difference",
                             latent.dim = 2, data.rescale = FALSE,
                    ordered = NULL, sampling.weights = NULL,
                    group = NULL, cluster = NULL, netstats.rescale = FALSE,
                    constraints = "", WLS.V = NULL, NACOV = NULL,
                    ...){

  requireNamespace("latentnet", quietly = TRUE)

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
  latent.network <- model.network.var


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
          data_edge[j+(i-1)*nrow(data$nonnetwork),model.network.var[netind]]=data$network[[netind]][i,j]
        }
      }
    }
  }



  latent.vars <- list()
  lsm.fits <- list()
  fit.prev <- NULL
  cov.mani <- list()
  edgeatt <- list()

  model.lavaanify <- lavaan::lavaanify(model)

  ## get the use specified model information
  model.user <- model.lavaanify[model.lavaanify$user==1, ]


  ## change nonnetwork variable to be pairwise
  variables.to.change=c()
  for (i in 1:nrow(model.user)){
    ## check if the variable on the lhs is a nonnetwork variable
    if (model.user$lhs[i] %in% colnames(data$nonnetwork) && !model.user$rhs[i] %in% colnames(data$nonnetwork)){
      variables.to.change <- c(variables.to.change, model.user$lhs[i])
    }
    if (model.user$rhs[i] %in% colnames(data$nonnetwork) && !model.user$lhs[i] %in% colnames(data$nonnetwork)){
      variables.to.change <- c(variables.to.change, model.user$rhs[i])
    }
    if (model.user$rhs[i] %in% colnames(data$nonnetwork) && model.user$lhs[i] %in% colnames(data$nonnetwork)){
      variables.to.change <- c(variables.to.change, model.user$rhs[i])
      variables.to.change <- c(variables.to.change, model.user$lhs[i])
    }
  }



  for (i in 1:length(variables.to.change)){
    data_edge[variables.to.change[i]]=NA
  }

  if (length(variables.to.change)>0){
    for (vind in 1:length(variables.to.change)){
      v_row <- rep(data$nonnetwork[variables.to.change[vind]][[1]], each = nrow(data$nonnetwork))
      v_col <- rep(data$nonnetwork[variables.to.change[vind]][[1]], nrow(data$nonnetwork))
      if (type=="difference"){
        data_edge[variables.to.change[vind]] <- v_row - v_col
      }else if (type=="average"){
        data_edge[variables.to.change[vind]] <- (v_row + v_col)/2
      }

    }
  }


  ## estimate network latent positions
  lsm.fits <- list()
  for (i in 1:length(latent.network)){
    fit <- latentnet::ergmm(network::network(data$network[[latent.network[i]]]) ~ euclidean(d = latent.dim))
    lsm.fits[[i]] <-fit
    latent.vars[[latent.network[i]]] <- c()
    for (dimind in 1:latent.dim){
      distsum <- 0
      for (dimind in 1:latent.dim){
        distsum = distsum + outer(fit$mcmc.mle$Z[,dimind], fit$mcmc.mle$Z[,dimind], "-")^2
      }
      dists <- array(t(sqrt(distsum)))

      data_edge[paste0(model.network.var[i], ".dists")] <- dists
      if (netstats.rescale){
        data_edge[paste0(model.network.var[i], ".dists")] <- scale(dists, center = TRUE, scale = TRUE)
      }
      latent.vars[[model.network.var[i]]] <- c(paste0(model.network.var[i], ".dists"))
    }
  }

  # print(lsm.fits)


  #print(model.network.stat.var.list)
  ## reconstruct the path model with the network variables
  ## replace the network variable name with the network variable stats name

  ## lavaanify the model
  model.lavaanify <- lavaan::lavaanify(model)

  ## get the use specified model information
  model.user <- model.lavaanify[model.lavaanify$user==1, ]


  ## now process each part of the user specified model
  model.to.remove.index <- NULL
  model.to.add <- ""
  model.to.remove.index <- NULL
  model.to.add <- ""
  for (i in 1:nrow(model.user)){
    ## check if left is network with LSM, remake
    if (model.user$lhs[i] %in% latent.network && !model.user$rhs[i] %in% latent.network){
      model.to.remove.index <- c(model.to.remove.index, i)
      model.stat.var.to.add <- latent.vars[[model.user$lhs[i]]]
      for (j in 1:length(model.stat.var.to.add)){
        model.temp <- paste0("\n ", model.stat.var.to.add[j], model.user$op[i], model.user$rhs[i])
        model.to.add <- paste0(model.to.add, model.temp)
      }
    }
    ## check if right is network with LSM and left is other variables
    if (model.user$rhs[i] %in% latent.network && !model.user$lhs[i] %in% latent.network){
      model.to.remove.index <- c(model.to.remove.index, i)
      model.stat.var.to.add <- latent.vars[[model.user$rhs[i]]]
      for (j in 1:length(model.stat.var.to.add)){
        model.temp <- paste0("\n ",  model.user$lhs[i], model.user$op[i],  model.stat.var.to.add[j])
        model.to.add <- paste0(model.to.add, model.temp)
      }
    }
    ## check if both lhs and rhs are network variables
    if (model.user$rhs[i] %in% model.network.var  && model.user$lhs[i] %in% model.network.var){
      ## if it is, record the index i and create new model items
      model.to.remove.index <- c(model.to.remove.index, i)
      model.stat.var.to.add.rhs <- latent.vars[[model.user$rhs[i]]]
      model.stat.var.to.add.lhs <- latent.vars[[model.user$lhs[i]]]
      for (j in 1:length(model.stat.var.to.add.rhs)){
        for (k in 1:length(model.stat.var.to.add.lhs)){
          model.temp <- paste0("\n", model.stat.var.to.add.lhs[j], model.user$op[i], model.stat.var.to.add.rhs[k])
          model.to.add <- paste0(model.to.add, model.temp)
        }
      }
    }
  }


  model.remove.network.var <- model.user[-model.to.remove.index, ]
  model.non.network.var <- ""
  if (nrow(model.remove.network.var)>0){
    for (i in 1:nrow(model.remove.network.var)){
      model.non.network.var.temp <- paste0(paste0(model.remove.network.var[i, c('lhs', 'op', 'rhs')], collapse = ' '))
      model.non.network.var <- paste0(model.non.network.var.temp, "\n", model.non.network.var)
    }
  }



  model.full <- paste0(model.non.network.var, "\n", model.to.add)





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
  lavparams[["model"]] <- model.full
  lavparams[["ordered"]] <- ordered
  lavparams[["sampling.weights"]] <- sampling.weights
  lavparams[["group"]] <- group
  lavparams[["cluster"]] <- cluster
  lavparams[["constraints"]] <- constraints
  lavparams[["WLS.V"]] <- WLS.V
  lavparams[["NACOV"]] <- NACOV

  model.res <- do.call(what="sem", args=c(lavparams))


  obj <- list(model=model.full, estimates=list(sem.es=model.res,lsm.es=lsm.fits), data = data_edge)
  class(obj) <- "networksem"
  return(obj)
}


