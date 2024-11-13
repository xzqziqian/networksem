


############ simulated data
set.seed(10)
nsamp = 50
lv1 <- rnorm(nsamp)
net <- ifelse(matrix(rnorm(nsamp^2) + lv1 %*% t(lv1), nsamp, nsamp) > 1, 1, 0)
lv2 <- rnorm(nsamp) + rowSums(net)
nonnet <- data.frame(x1 = lv1*0.5 + rnorm(nsamp),
                     x2 = lv1*0.8 + rnorm(nsamp),
                     x3 = lv2*0.5 + rnorm(nsamp),
                     x4 = lv2*0.8 + rnorm(nsamp))

model <-'
  lv1 =~ x1 + x2
  lv2 =~ x3 + x4
  net ~ lv1
  lv2 ~ net
'
data = list(network = list(net = net), nonnetwork = nonnet)
set.seed(100)
res <- sem.net.edge.lsm(model = model, data = data, latent.dim = 1)
summary(res)
path.networksem(res, 'lv2', c('net.dists'), 'lv1')

######################## real data example
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
path.networksem(res, "wealth","flo.dists", "priorates")

