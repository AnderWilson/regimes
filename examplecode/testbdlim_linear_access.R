rm(list=ls())
gc()

#load package
# library(devtools)
# install_github(repo="anderwilson/regimes", ref="Developmental")
# library(regimes)

#----------------------------------------------------------------------------------------------------------
#data prep
#----------------------------------------------------------------------------------------------------------
dat <- read.csv("AnalysisData/Nitrate_PFT_for_Ander.csv")
head(dat)
dim(dat)

colnames(dat[,-grep("now",colnames(dat))])

X <- dat[,grep("now",colnames(dat))]
Y <- dat[,"fev1_adjz"]
Z <- as.matrix(dat[,c("enrollment_age","lths","misslths","ch_age_spir")])
G <- dat[,"childsex"]


# estimate model
allfits <- list()
results <- NULL
for(d in 3:7){

fit <- bdlim(Y=Y,X=X, G=G, Z=Z, inter.model="all", niter=10000, nthin=50,
             basis.opts = list(df=d, type="ns"),
             seed=1234)

allfits[[as.character(d)]] <- fit

tmp <- fit$modelfit
tmp$df <- d
results <- rbind(results,tmp)

}

results <- results[order(results[,"DIC"]),]
plot(results$DIC)

# summary(fit)

summary(allfits[["5"]])
# plot(summary(allfits[["4"]]))
plot(summary(allfits[["5"]]),grid=4)
plot(summary(allfits[["4"]]),grid=4)
plot(summary(allfits[["6"]]),grid=4)

?plot.summary.bdlim
# plot(summary(allfits[["6"]]))

