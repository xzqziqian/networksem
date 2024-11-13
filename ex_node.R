library(networksem)


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

## the actual data used for model
str(res$data)
mean(res$data$nonnetwork$wechat.degree)
## calculate mediation effect
pars <- parameterEstimates(res$estimates)
predictor = c("Extroversion")
mediator = c("friends.degree", "friends.betweenness", "friends.closeness")
outcome = c("Happiness")
effect_table <- expand.grid("predictor" = predictor,
                            "mediator" = mediator,
                            "outcome" = outcome)
effect_table$apath = NA; effect_table$bpath = NA; effect_table$indirect = NA
effect_table$apath_se = NA; effect_table$bpath_se = NA;
for (i in 1:nrow(effect_table)){
  effect_table$apath[i] <- pars[pars$rhs == effect_table$predictor[i] & pars$lhs == effect_table$mediator[i], "est"]
  effect_table$apath_se[i] <- pars[pars$rhs == effect_table$predictor[i] & pars$lhs == effect_table$mediator[i], "se"]
  effect_table$bpath[i] <- pars[pars$rhs == effect_table$mediator[i] & pars$lhs == effect_table$outcome[i], "est"]
  effect_table$bpath_se[i] <- pars[pars$rhs == effect_table$mediator[i] & pars$lhs == effect_table$outcome[i], "se"]
  effect_table$indirect[i] <- effect_table$apath[i]*effect_table$bpath[i]
}

effect_table


# extract loadings etc

vals <- lavInspect(res$estimates, what="est")
# factor loadings
# kable(round(vals$lambda,2)[,1:6], booktab=T, format = "latex")
# error covariance
# vals$theta
# regression coefficients
# vals$beta
# observed variable covariance
# vals$psi


plot.res <- lavaan2ram(res$estimates, ram.out = F)
plot.res.path <- ramPathBridge(plot.res, F, F)
plot(plot.res.path, 'ex1', output.type='dot')

grViz('ex1.dot')

## Wald statistics for two loadings
W <- coef(res$estimates)[2:3] %*% solve(vcov(res$estimates)[2:3, 2:3]) %*% coef(res$estimates)[2:3]

## p-value
1 - pnorm(W, lower.tail = F)*2



