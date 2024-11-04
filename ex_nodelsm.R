

############ simulated data
set.seed(10)
nsamp = 50
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
res <- sem.net.lsm(model = model, data = data, latent.dim = 2)
summary(res)
path.networksem(res, 'lv2', c('net.Z1', 'net.Z2'), 'lv1')



############ friendship and wechat data

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
network$wechat[is.na(network$wechat)] <- 0





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


data = list(network=network, nonnetwork=non_network)
set.seed(100)
res <- sem.net.lsm(model=model,data=data, latent.dim = 2, data.rescale = T)


### write a new summary function
## results
summary(res)
plot(res$estimates$lsm.es[[1]])
plot(res$estimates$lsm.es[[2]])
str(res$data)

## calculate mediation effect
pars <- parameterEstimates(res$estimates$sem.es)
predictor = c("Extroversion")
mediator = c("friends.Z1", "friends.Z2")
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
