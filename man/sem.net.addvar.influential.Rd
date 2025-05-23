% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/networkFun.R
\name{sem.net.addvar.influential}
\alias{sem.net.addvar.influential}
\title{Compute a list of user-specified network statistics using the "influential" package and add it to the existing data.}
\usage{
sem.net.addvar.influential(
  model.network.stat.var.list,
  data,
  model.network.var.i,
  stats,
  statsname,
  netstats.rescale,
  netstats.options = NULL
)
}
\arguments{
\item{model.network.stat.var.list}{a list of elements with names corresponding to the network names and values corresponding to lists of network statistics that will be calculated for the corresponding network}

\item{data}{a list containing both the non-network and network data}

\item{model.network.var.i}{an index indicating a specific network within all networks}

\item{stats}{a network statistics that can be calculated using package "influential"}

\item{statsname}{name of the network statistics}

\item{netstats.rescale}{a logical value indicating whether to rescale network statistics to have mean 0 and sd 1}

\item{netstats.options}{a list with names being the argument names for calculating the network statistics, and values being the argument values}
}
\value{
a list with the first value being the list of network statistics names and the second value being the data frame with added network statistics
}
\description{
Compute a list of user-specified network statistics using the "influential" package and add it to the existing data.
}
