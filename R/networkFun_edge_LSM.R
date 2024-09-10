#' Fit a sem model with network data using latent distances between actors as variables
#' @param model a model specified in lavaan model syntax.
#' @param data a list containing both the non-network and network data
#' @param type "difference" for using the difference between the network statistics of the two actors as the edge covariate; "average" for using the average of the network statistics of the two actors as the edge covariate
#' @param ordered parameter same as "ordered" in the lavaan sem() function; whether to treat data as ordinal
#' @param sampling.weights parameter same as "sampling.weights" in the lavaan sem() function; whether to apply weights to data
#' @param group parameter same as "group" in the lavaan sem() function; whether to fit a multigroup model
#' @param cluster parameter same as "cluster" in the lavaan sem() function; whether to fit a cluster model
#' @param constraints parameter same as "constraints" in the lavaan sem() function; whether to apply constraints to the model
#' @param WLS.V parameter same as "WLS.V" in the lavaan sem() function; whether to use WLS.V estimator
#' @param NACOV parameter same as "NACOV" in the lavaan sem() function; whether to use NACOV estimator
#' @param latent.dim number of network latent dimensions to use
#' @param ... optional arguments for the sem() function
#' @return the updated model specification with the network statistics as variables and a lavaan object which is the SEM results, also the data generated
#' @export
sem.net.edge.lsm <- function(model=NULL, data=NULL, type="difference",
                             latent.dim = 2,
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
      v_row <- rep(data$nonnetwork[variables.to.change[vind]][[1]], each = nrow(data$nonnetwork))
      v_col <- rep(data$nonnetwork[variables.to.change[vind]][[1]], nrow(data$nonnetwork))
      if (type=="difference"){
        data_edge[variables.to.change[vind]] <- v_row - v_col
      }else if (type=="average"){
        data_edge[variables.to.change[vind]] <- (v_row + v_col)/2
      }

    }
  }

  ## variables predicting latent positions of social networks
  lat.var.pred.net <- list()
  for (i in 1:nrow(model.user)){
    if ((model.user$lhs[i] %in% model.network.var) && !(model.user$rhs[i] %in% model.network.var)){
      if (is.null(lat.var.pred.net[[model.user$lhs[i]]])){
        lat.var.pred.net[[model.user$lhs[i]]] <- c(model.user$rhs[i])
      }else{
        lat.var.pred.net[[model.user$lhs[i]]] <- c(model.user$rhs[i], lat.var.pred.net[[model.user$lhs[i]]])
      }
    }
  }


  # find cov in LSM, fit LSM with cov model
  for (i in 1:length(model.network.var)){

    if (paste0("lp.", model.network.var[i]) %in% lat.var.pred.net[[model.network.var[i]]]){
      # filter out the covariates
      lat.var.pred.net.cov <- lat.var.pred.net[[model.network.var[i]]][!grepl("lp.", lat.var.pred.net[[model.network.var[i]]], fixed = TRUE)]
      if (length(lat.var.pred.net.cov)>0){
        for (j in 1:length(lat.var.pred.net.cov)){
          for (k in 1:nrow(model.user)){
            if ((model.user$lhs[k]==lat.var.pred.net.cov[j]) && !(model.user$rhs[k] %in% model.network.var) && model.user$op[k] == "=~"){
              if (is.null(cov.mani[[model.user$lhs[k]]])){
                cov.mani[[model.user$lhs[k]]] <- c(model.user$rhs[k])
              }else{
                cov.mani[[model.user$lhs[k]]] <- c(model.user$rhs[k],  cov.mani[[model.user$lhs[k]]])
              }
            }
          }
        }

        if (length(cov.mani)>0){
          model.prev <- ""
          for (j in 1:length(cov.mani)){
            for (k in 1:length(cov.mani[[j]]))
              model.prev <- paste0(model.prev, names(cov.mani)[j], "=~", cov.mani[[j]][k], "\n")
          }

          fit.prev <- sem(data=data_edge, model=model.prev)
          score.prev <- lavPredict(fit.prev)

          for (j in 1:length(cov.mani)){
            data_edge[, names(cov.mani)[j]]<-score.prev[,j]
          }
        }

        formu <- "latentnet::ergmm(net ~ euclidean(d=latent.dim)"
        net <- network::network(data$network[[model.network.var[i]]])

        for (j in 1:length(lat.var.pred.net.cov)){
          # set.vertex.attribute(net, lat.var.pred.net.cov[[j]], data[, lat.var.pred.net.cov[[j]]])
          #as.matrix(net, attrname="age")
          val <- matrix(data_edge[,lat.var.pred.net.cov[[j]]], nrow=nrow(data$nonnetwork), byrow=T)
          set.edge.value(net, lat.var.pred.net.cov[[j]], val)
          # edgeatt[[j]] <- val

          # formu <- paste0(formu, "+ edgecov(edgeatt[[",j,"]])")
          formu <- paste0(formu, "+ edgecov(as.matrix(net, attrname='",lat.var.pred.net.cov[[j]],"'))")

        }

        formu <- paste0(formu, ")")

        formu_parsed <- parse(text=formu)
        assign("net",net,envir = globalenv())
        lsm.fit <- eval(formu_parsed)
        lsm.fits[[i]] <- lsm.fit



        # lsm.fit = ergmm(net~euclidean(d=2) + edgecov(as.matrix(net, attrname="age")) + edgecov(as.matrix(net, attrname="smoke")))

      }else{
        net <- network::network(data$network[[model.network.var[i]]])
        lsm.fit <- latentnet::ergmm(net ~ euclidean(d=latent.dim))
        lsm.fits[[i]] <- lsm.fit
      }

      distsum <- 0
      for (dimind in 1:latent.dim){
        distsum = distsum + outer(lsm.fit$mcmc.mle$Z[,dimind], lsm.fit$mcmc.mle$Z[,dimind], "-")^2
      }
      dists <- array(t(sqrt(distsum)))

      data_edge[paste0(model.network.var[i], ".dists")] <- dists
      latent.vars[[model.network.var[i]]] <- c(paste0(model.network.var[i], ".dists"))
    }
  }


  if (length(cov.mani)>0){
    data_edge <- data_edge[,!(colnames(data_edge) %in% names(cov.mani))]
  }





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
  for (i in 1:nrow(model.user)){
    ## check if already estimated
    # if (model.user$lhs[i] %in% names(cov.mani) && !(model.user$rhs[i] %in% model.network.var) && model.user$op[i] =="=~"){
    #   model.to.remove.index <- c(model.to.remove.index, i)
    # }
    ## check if left is network and right is latent position, remove this
    if (model.user$lhs[i] %in% model.network.var){
      ## if it is, record the index i and create new model items
      model.to.remove.index <- c(model.to.remove.index, i)
    }
    ## check if right is latent position and left is other variables
    if (grepl("lp.", model.user$lhs[i], fixed = TRUE) && (!(model.user$rhs[i] %in% model.network.var))){
      ## if it is, record the index i and create new model items
      model.to.remove.index <- c(model.to.remove.index, i)
      model.stat.var.to.add <- latent.vars[[substring(model.user$lhs[i], 4)]]
      for (j in 1:length(model.stat.var.to.add)){
        model.temp <- paste0("\n ", model.stat.var.to.add[j], model.user$op[i], model.user$rhs[i])
        model.to.add <- paste0(model.to.add, model.temp)
      }
    }
    ## left is latent position and right is others
    if (grepl("lp.", model.user$rhs[i], fixed = TRUE) && (!(model.user$lhs[i] %in% model.network.var))){
      ## if it is, record the index i and create new model items
      model.to.remove.index <- c(model.to.remove.index, i)
      model.stat.var.to.add <- latent.vars[[substring(model.user$rhs[i], 4)]]
      for (j in 1:length(model.stat.var.to.add)){
        model.temp <- paste0("\n ", model.user$lhs[i], model.user$op[i], model.stat.var.to.add[j])
        model.to.add <- paste0(model.to.add, model.temp)
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
  lavparams[["model"]] <- model.full
  lavparams[["ordered"]] <- ordered
  lavparams[["sampling.weights"]] <- sampling.weights
  lavparams[["group"]] <- group
  lavparams[["cluster"]] <- cluster
  lavparams[["constraints"]] <- constraints
  lavparams[["WLS.V"]] <- WLS.V
  lavparams[["NACOV"]] <- NACOV

  model.res <- do.call(what="sem", args=c(lavparams))


  list(model=model.full, estimates=list(sem.es=model.res,lsm.es=lsm.fits), data = data_edge)
}
