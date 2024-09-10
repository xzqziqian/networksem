


#' Compute a list of user-specified network statistics values using the "sna" package and add them to the non-network data.
#' @param data a list containing both the non-network and network data
#' @param model.network.stat.var.list a list of elements with names corresponding to the network names and values corresponding to lists of network statistics that will be calculated for the corresponding network
#' @param model.network.var.i an index indicating a specific network within all networks
#' @param stats a network statistics that can be calculated using package "sna"
#' @param statsname name of the network statistics
#' @param netstats.rescale a logical value indicating whether to rescale network statistics to have mean 0 and sd 1
#' @param netstats.options a list with names being the argument names for calculating the network statistics, and values being the argument values
#' @return a list with the first value being the list of network statistics names and the second value being the data frame with added network statistics
#' @export
sem.net.addvar.stat <- function(model.network.stat.var.list, data, model.network.var.i, stats, statsname, netstats.rescale, netstats.options=NULL){
  degree <- sna::degree
  betweenness <- sna::betweenness
  closeness <- sna::closeness
  ## create the network stats variable name
  model.network.stat.var <- paste0(model.network.var.i, ".", statsname)

  ## add network statistics to the variable list
  model.network.stat.var.list[[model.network.var.i]] <- c(model.network.stat.var.list[[model.network.var.i]], model.network.stat.var)


  ## using do.call to calculate the network statistics in variable list, add statistics to nonnetwork data
  args <- list("dat"=data$network[[model.network.var.i]])
  args <- c(args, netstats.options)
  data$nonnetwork[[model.network.stat.var]] <- do.call(what=stats, args=args)


  # scale
  if(netstats.rescale){
    data$nonnetwork[model.network.stat.var] <- scale(data$nonnetwork[model.network.stat.var])
  }

  return(list(model.network.stat.var.list, data$nonnetwork))
}


#' Compute a list of user-specified network statistics using the "influential" package and add it to the existing data.
#' @param model.network.stat.var.list a list of elements with names corresponding to the network names and values corresponding to lists of network statistics that will be calculated for the corresponding network
#' @param data a list containing both the non-network and network data
#' @param model.network.var.i an index indicating a specific network within all networks
#' @param stats a network statistics that can be calculated using package "influential"
#' @param statsname name of the network statistics
#' @param netstats.rescale a logical value indicating whether to rescale network statistics to have mean 0 and sd 1
#' @param netstats.options a list with names being the argument names for calculating the network statistics, and values being the argument values
#' @return a list with the first value being the list of network statistics names and the second value being the data frame with added network statistics
#' @export
sem.net.addvar.influential <- function(model.network.stat.var.list, data, model.network.var.i, stats, statsname, netstats.rescale, netstats.options=NULL){
  ## create the network stats variable name
  model.network.stat.var <- paste0(model.network.var.i, ".", statsname)

  ## add network statistics to the variable list
  model.network.stat.var.list[[model.network.var.i]] <- c(model.network.stat.var.list[[model.network.var.i]], model.network.stat.var)

  ## using do.call to calculate the network statistics in variable list, add statistics to nonnetwork data
  args <- list("graph"=graph_from_adjacency_matrix(data$network[[model.network.var.i]]))
  args <- c(args, netstats.options)
  data$nonnetwork[[model.network.stat.var]] <- do.call(what=stats, args=args)

  # scale
  if(netstats.rescale){
    data$nonnetwork[model.network.stat.var] <- scale(data$nonnetwork[model.network.stat.var])
  }

  return(list(model.network.stat.var.list, data$nonnetwork))
}





#' Compute user-specified network statistics for a specific network.
#' @param model.network.stat.var.list  a list of elements with names corresponding to the network names and values corresponding to lists of network statistics that will be calculated for the corresponding network
#' @param data a list containing both the non-network and network data
#' @param netstats a list of user-specified network statistics
#' @param model.network.var.i the index of a network within all networks
#' @param netstats.rescale a logical value indicating whether to rescale network statistics to have mean 0 and sd 1
#' @param netstats.options a list with element names corresponding to the network statistics and element values corresponding to another list. The list corresponding to each network statistics has element names being the argument names for calculating the network statistics, and values being the argument values
#' @return a list with the first value being the list of network statistics names and the second value being the data frame with added network statistics variables
#' @export
sem.net.addvar <- function(model.network.stat.var.list=NULL, data=NULL, netstats=NULL, model.network.var.i=NULL, netstats.rescale=TRUE, netstats.options=NULL){
  res.list<-list()
  for (stat in netstats){

    if( stat %in% c("degree", "closeness", "betweenness", "evcent", "stresscent", "infocent")){
      # sna
      res.list<-sem.net.addvar.stat(model.network.stat.var.list, data, model.network.var.i, stats=stat, statsname=stat,  netstats.rescale, netstats.options[[stat]])
    }else{
      # influential
      res.list<-sem.net.addvar.influential(model.network.stat.var.list, data, model.network.var.i, stats=stat, statsname=stat,  netstats.rescale, netstats.options[[stat]])
    }
    model.network.stat.var.list <- res.list[[1]]
    data$nonnetwork <- res.list[[2]]
  }
  return(res.list)
}


#' Fit a sem model with network data using node statistics as variables. User-specified network statistics will be calculated and used as variables instead of the networks themselves in the SEM.
#' @param model a model specified in lavaan model syntax.
#' @param data a list containing the observed non-network nodal variables and the network data
#' @param netstats a user-specified list of network statistics to be calculated and used in the SEM, e.g., c("degree", "betweenness"), available options include "degree", "betweenness", "closeness", "evcent", "stresscent", and "infocent" from the "sna" package and "ivi", "hubness.score", "spreading.score" and "clusterRank" from the "influential" package
#' @param netstats.options a user-specified named list with element names corresponding to the network statistics names and element values corresponding to other lists. The list corresponding to each network statistics name has element names being the argument names for calculating the network statistics, and values being the argument values, as used in the corresponding functions in the "sna" or "influential" packages. e.g., netstats.options=list("degree"=list("cmode"="freeman"), "closeness"=list("cmode"="undirected"), "clusterRank"=list("directed"=FALSE))
#' @param netstats.rescale a list of logical value indicating whether to rescale network statistics to have mean 0 and sd 1.
#' @param data.rescale whether to rescale the whole dataset (with restructured network and nonnetwork data) to have mean 0 and standard deviation 1 when fitting it to SEM, default to FALSE
#' @param ordered parameter same as "ordered" in the lavaan sem() function; whether to treat data as ordinal
#' @param sampling.weights parameter same as "sampling.weights" in the lavaan sem() function; whether to apply weights to data
#' @param group parameter same as "group" in the lavaan sem() function; whether to fit a multigroup model
#' @param cluster parameter same as "cluster" in the lavaan sem() function; whether to fit a cluster model
#' @param constraints parameter same as "constraints" in the lavaan sem() function; whether to apply constraints to the model
#' @param WLS.V parameter same as "WLS.V" in the lavaan sem() function; whether to use WLS.V estimator
#' @param NACOV parameter same as "NACOV" in the lavaan sem() function; whether to use NACOV estimator
#' @param ... optional arguments for the sem() function
#' @return the updated model specification with the network statistics as variables and a lavaan object which is the SEM results
#' @export
sem.net <- function(model=NULL, data=NULL, netstats=NULL,
                    ordered = NULL, sampling.weights = NULL, data.rescale = FALSE,
                    netstats.rescale = FALSE, group = NULL, cluster = NULL,
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
  if (length(model.network.var) > 0){
    if (is.null(netstats)){
      ## loop through the statistics
      for (i in 1:length(model.network.var)){
        ## call helper function, which loops over all target statistics to be used
        res.tmp <- sem.net.addvar(model.network.stat.var.list, data, c("degree"), model.network.var[i])
        model.network.stat.var.list <-res.tmp[[1]]
        data$nonnetwork <- res.tmp[[2]]
      }
    }else{
      ## loop through the variables and statistics
      for (i in 1:length(model.network.var)){
        res.tmp <- sem.net.addvar(model.network.stat.var.list, data, netstats, model.network.var[i],  netstats.rescale, netstats.options)
        model.network.stat.var.list <- res.tmp[[1]]
        data$nonnetwork <- res.tmp[[2]]
      }
    }
  }


  ## reconstruct the path model with the network variables
  ## replace the network variable name with the network variable stats name

  ## lavaanify the model
  model.lavaanify <- lavaanify(model)

  ## get the use specified model information
  model.user <- model.lavaanify[model.lavaanify$user==1, ]

  ## now process each part of the user specified model
  model.to.remove.index <- NULL # row index of the model items to remove
  model.to.add <- ""
  for (i in 1:nrow(model.user)){
    ## check if the variable on the lhs is a network variable
    if (model.user$lhs[i] %in% model.network.var && (!(model.user$rhs[i] %in% model.network.var))){
      ## if it is, record the index i and create new model items for te network
      model.to.remove.index <- c(model.to.remove.index, i)
      model.stat.var.to.add <- model.network.stat.var.list[[model.user$lhs[i]]]
      for (j in 1:length(model.stat.var.to.add)){
        model.temp <- paste0("\n", model.stat.var.to.add[j], model.user$op[i], model.user$rhs[i])
        model.to.add <- paste0(model.to.add, model.temp)
      }
    }
    ## check if the variable on the rhs is a network variable and the lhs is not
    if (model.user$rhs[i] %in% model.network.var  && (!(model.user$lhs[i] %in% model.network.var))){
      ## record the index i and create new model items
      model.to.remove.index <- c(model.to.remove.index, i)
      model.stat.var.to.add <- model.network.stat.var.list[[model.user$rhs[i]]]
      for (j in 1:length(model.stat.var.to.add)){
        model.temp <- paste0("\n", model.user$lhs[i], model.user$op[i], model.stat.var.to.add[j])
        model.to.add <- paste0(model.to.add, model.temp)
      }
    }
    ## check if both lhs and rhs are network variables
    if (model.user$rhs[i] %in% model.network.var  && model.user$lhs[i] %in% model.network.var){
      ## if it is, record the index i and create new model items
      model.to.remove.index <- c(model.to.remove.index, i)
      model.stat.var.to.add.rhs <- model.network.stat.var.list[[model.user$rhs[i]]]
      model.stat.var.to.add.lhs <- model.network.stat.var.list[[model.user$lhs[i]]]
      for (j in 1:length(model.stat.var.to.add.rhs)){
        for (k in 1:length(model.stat.var.to.add.lhs)){
          model.temp <- paste0("\n", model.stat.var.to.add.lhs[j], model.user$op[i], model.stat.var.to.add.rhs[k])
          model.to.add <- paste0(model.to.add, model.temp)
        }
      }
    }
  }

  model.remove.network.var <- model.user[-model.to.remove.index, ] # remove initial model specification
  # add altered model specification
  model.non.network.var <- ""
  for (i in 1:nrow(model.remove.network.var)){
    model.non.network.var.temp <- paste0(paste0(model.remove.network.var[i, c('lhs', 'op', 'rhs')], collapse = ' '))
    model.non.network.var <- paste0(model.non.network.var.temp, "\n", model.non.network.var)
  }

  model.full <- paste0(model.non.network.var, "\n", model.to.add)

  # if(!is.null(community)){
  #   communities_clust <- cutree(sna::equiv.clust(network)$cluster, k=community)
  #   data["communities_clust"]<-communities_clust
  # }
  #

  # if(!is.null(community) || !is.null(group)){
  #   group <- ifelse(!is.null(community), "communities_clust", group)
  # }
  #


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


  list(model=model.full, estimates=model.res, data=data)
}








