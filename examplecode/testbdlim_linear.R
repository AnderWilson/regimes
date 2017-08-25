rm(list=ls())
gc()

#load package
library(devtools)
install_github(repo="anderwilson/regimes", ref="Developmental")
library(regimes)

#simulate data
dat <- simregimes(scenario="bdlim2", seed=1234, n=1000)

# estimate model
fit <- bdlim(Y=dat$Y,X=dat$X, G=dat$G, Z=dat$Z, inter.model="all", niter=10000, nthin=50,
             basis.opts = list(df=4, type="ns"),
             seed=1234)

summary(fit)


