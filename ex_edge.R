library(networksem)

############# simulated data
set.seed(100)
nsamp = 100
net <- data.frame(ifelse(matrix(rnorm(nsamp^2), nsamp, nsamp) > 1, 1, 0))
mean(net) # density of simulated network
lv1 <- rnorm(nsamp)
lv2 <- rnorm(nsamp)
nonnet <- data.frame(x1 = lv1*0.5 + rnorm(nsamp),
                     x2 = lv1*0.8 + rnorm(nsamp),
                     x3 = lv2*0.5 + rnorm(nsamp),
                     x4 = lv2*0.8 + rnorm(nsamp))

model <-'
  lv1 =~ x1 + x2
  lv2 =~ x3 + x4
  lv1 ~ net
  lv2 ~ lv1
'
data = list(network = list(net = net), nonnetwork = nonnet)
set.seed(100)
res <- sem.net.edge(model = model, data = data, type = 'difference')
summary(res)
path.networksem(res, "net", "lv1", "lv2")

############# friendship and wechat data
load("data/cf_data_book.RData")  ## load the list cf_data
write.csv(cf_data$cf_nodal_cov, "data/cf_nodal_cov.csv")
write.csv(cf_data$cf_friend_network, "data/cf_friend_network.csv")

## data - non-network variables
non_network <- as.data.frame(cf_data$cf_nodal_cov)
dim(non_network)

## network - network variables (friends network and wechat network)
## note that the names of the networks are used in model specification
network <- list()
network$friends <- cf_data$cf_friend_network
network$wechat <- cf_data$cf_wetchat_network
network$wechat[is.na(network$wechat)] <- 0

model <-'
  Happiness =~ happy1 + happy2 + happy3 + happy4
  Happiness ~ friends
'

data = list(network=network, nonnetwork=non_network)
set.seed(100)
res <- sem.net.edge(model=model,data=data, type = 'average',  nestats.rescale = F)

## results
summary(res)

################### attorney data
non_network <- read.table("../data/attorney/ELattr.dat")[,c(3,5)]
colnames(non_network) <- c('gender', 'years')
non_network$gender <- non_network$gender - 1
network <- list()
network$advice <- read.table("../data/attorney/ELadv.dat")
network$cowork <- read.table("../data/attorney/ELwork.dat")

model <-'
  advice ~ gender + years
  cowork ~ advice + gender + years
'

data = list(network=network, nonnetwork=non_network)
set.seed(100)
res <- sem.net.edge(model = model, data = data, data.rescale = F, netstats.rescale = T,
                    network = network, type = "difference", ordered = c("cowork", "advice"))
summary(res)
path.networksem(res, "gender", "advice", "cowork")

plot.res <- lavaan2ram(res$estimates, ram.out = F)
plot.res.path <- ramPathBridge(plot.res, F, F)
plot(plot.res.path, 'exedge', output.type='dot')

