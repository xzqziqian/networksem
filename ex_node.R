library(networksem)
library(DiagrammeR)
library(RAMpath)

#################  simulated data
set.seed(100)
nsamp = 100
net <- ifelse(matrix(rnorm(nsamp^2), nsamp, nsamp) > 1, 1, 0)
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
  net ~ lv2
  lv1 ~ net + lv2
'
data = list(network = list(net = net), nonnetwork = nonnet)
set.seed(100)
res <- sem.net(model = model, data = data, netstats = c('degree'))
summary(res)
path.networksem(res, "lv2", c("net.degree"), "lv1")


################# friendship and wechat data

# load data
load("data/cf_data_book.RData")  ## load the list cf_data

## data - non-network variables
non_network <- as.data.frame(cf_data$cf_nodal_cov)
dim(non_network)

## network - network variables (friends network and wechat network)
## note that the names of the networks are used in model specification
network <- list()
network$friends <- cf_data$cf_friend_network
network$wechat <- cf_data$cf_wetchat_network


model <-'
  Extroversion =~ personality1 + personality6
                + personality11 + personality16
  Conscientiousness =~ personality2 + personality7
                + personality12 + personality17
  Neuroticism  =~ personality3 + personality8
                + personality13 + personality18
  Openness =~ personality4 + personality9
                + personality14 + personality19
  Agreeableness =~ personality5 + personality10 +
                personality15 + personality20
  Happiness =~ happy1 + happy2 + happy3 + happy4
  friends ~ Extroversion + Conscientiousness + Neuroticism +
  Openness + Agreeableness
  Happiness ~ friends + wechat
'

## run sem.net
data = list(
  nonnetwork = non_network,
  network = network
)

set.seed(100)
res <- sem.net(model=model, data=data,
               netstats=c("degree", "betweenness", "closeness"),
               netstats.rescale = T,
               netstats.options=list("degree"=list("cmode"="freeman")))


## results
summary(res)


plot.res <- lavaan2ram(res$estimates, ram.out = F)
plot.res.path <- ramPathBridge(plot.res, F, F)
plot(plot.res.path, 'ex1', output.type='dot')

grViz('ex1.dot')

path.networksem(res, 'Agreeableness', c('friends.degree', 'friends.betweenness', 'friends.closeness'), 'Happiness')

## Wald statistics for two loadings
W <- coef(res$estimates)[2:3] %*% solve(vcov(res$estimates)[2:3, 2:3]) %*% coef(res$estimates)[2:3]

## p-value
1 - pnorm(W, lower.tail = F)*2



