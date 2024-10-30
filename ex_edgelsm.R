library(networksem)
load("data/flomarriage.RData")

network <- list()
network$flo <- flomarriage.network
nonnetwork <- flomarriage.nonnetwork


model <- '
  flo ~  wealth
  priorates ~ flo + wealth
'

data = list(network=network, nonnetwork=nonnetwork)
set.seed(100)
res <- sem.net.edge.lsm(model=model,data=data, type = "difference", latent.dim = 2, netstats.rescale = T, data.rescale = T)
## results
summary(res)
str(res$data)

plot(res$estimates$lsm.es[[1]], pie=T)
summary(res)
