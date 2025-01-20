
#' Fit a Structural Equation Model (SEM) with both network and non-network data by transforming nonnetwork data into paired values corresponding to network edge values.
#' @param model A model string specified in lavaan model syntax that includes relationships among the network and non-network variables.
#' @param data A list containing the data. The list has two named components, "network" and "nonnetwork"; "network" is a list of named adjacency matrices for the network data, and "nonnetwork" is the dataframe of non-network covariates.
#' @param type Option for transforming nonnework data; "difference" for using the difference between two individuals as the edge covariate; "average" for using the average between two individuals as the edge covariate.
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
#' @return A networksem object containing the updated model specification string with the reconstructed network statistics as variables and a lavaan SEM object.
#' @export
#' @examples
#' \donttest{
#' set.seed(100)
#' nsamp = 20
#' net <- data.frame(ifelse(matrix(rnorm(nsamp^2), nsamp, nsamp) > 1, 1, 0))
#' mean(net) # density of simulated network
#' lv1 <- rnorm(nsamp)
#' lv2 <- rnorm(nsamp)
#' nonnet <- data.frame(x1 = lv1*0.5 + rnorm(nsamp),
#'                      x2 = lv1*0.8 + rnorm(nsamp),
#'                      x3 = lv2*0.5 + rnorm(nsamp),
#'                      x4 = lv2*0.8 + rnorm(nsamp))
#'
#' model <-'
#'   lv1 =~ x1 + x2
#'   lv2 =~ x3 + x4
#'   lv1 ~ net
#'   lv2 ~ lv1
#' '
#' data = list(network = list(net = net), nonnetwork = nonnet)
#' set.seed(100)
#' res <- sem.net.edge(model = model, data = data, type = 'difference')
#' summary(res)
#' }
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
