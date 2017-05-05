#-------------------------------------------------------------------------------------------------
#' Bayesian distributed lag interaction models
#'
#' This estimates the Bayesian distributed lag interaction model (BDLIM).
#'
#' @param Y Vector of outcomes.
#' @param X Matrix of exposures over an evenly spaced grid. There should be no missing exposures.
#' @param Z Design matrix of covariates. The design matrix for G will be added automatically, including an intercept.
#' @param G Vector indicating group membership. This should be a factor.
#' @param inter.model Indicator of how G enters the model. If 'bw' both the betas and weight functions will vary between groups. If 'b' then only the betas will vary between groups. If 'w' then only the wieght function will vary by group. If "n" then neither beta or the wieght function will vary by group. If a vector of any combination of the four choices is provided then those models will be run and compared. The option 'all' runs all four models and compares them.
#' @param family A description of the error distribution and link function to be used in the model. Currently, gaussian and binomial are supported.
#' @param niter Number of MCMC iterations including burnin.
#' @param nburn Number of MCMC iterations to be discarded as burnin.
#' @param nthin Number of draws taken to obtain one sample.
#' @param prior List with the entries: sigma = a numeric 2-vector with the shape and rate paramters for the pirior in the error precision (1/sigma^2); betavar = the prior variance for beta; and gamma = the prior variance for the covarites. The priors on beta and gamma are iid normal mean zero.
#' @param basis.opts List with the entries: type = the type of basis used, either 'face' (default) or "ns"; knots = the number of knots used for method face; pve = the percent of variance explained by the PCs for method face; df = the df for ns method.
#' @param seed A seed to be set before each model is run.
#' @return An object of class 'bdlim'.
#' @author Ander Wilson
#' @import stats
#' @export




bdlim <- function(Y,X,Z,G=NULL,inter.model="all",family=gaussian,niter=1000,nburn=500,nthin=1,basis.opts,prior,seed){

  # make family a function
  if(is.character(family)){
    family <- get(family, mode = "function", envir = parent.frame())
  }
  if(is.function(family)){
      family <- family()
  }


  #account for missing input in priors
  if(missing(prior)) prior <- NULL
  if(is.null(prior$sigma)) prior$sigma <- c(0.001,0.001)
  if(is.null(prior$beta)) prior$beta <- Inf
  if(is.null(prior$gamma)) prior$gamma <- Inf


  #account for missing input in basis function
  if(missing(basis.opts)) basis.opts <- NULL
  if(is.null(basis.opts$type)) basis.opts$type <- "face"
  if(toupper(basis.opts$type)=="FACE"){
    if(is.null(basis.opts$knots)) basis.opts$knots <- round(ncol(X)/3)
    if(is.null(basis.opts$pve)) basis.opts$pve <- .99
  }else if(toupper(basis.opts$type)=="NS"){
    if(is.null(basis.opts$df) & !is.null(basis.opts$knots)){
      basis.opts$df <- basis.opts$knots+2
    }else if(is.null(basis.opts$df)){ 
      basis.opts$df <- 5
    }
  }
  #make basis
  B <- bdlimbasis(as.matrix(X),knots=basis.opts$knots,df=basis.opts$df,pve=basis.opts$pve, type=basis.opts$type)

  #scaled data
  x <- X%*%B$psi
  # print(G)
  if(is.null(G)){
    z1 <- model.matrix(~Z)
    inter.model <- c("n")
  }else{
    if(!is.factor(G)) G <- as.factor(G)
    z1 <- model.matrix(~G+Z-1)
  }
  # print(head(z1))
  z1 <- z1[,qr(z1)$pivot[1:qr(z1)$rank]]
  colnames(x) <- paste0("x",1:ncol(x))

  # make y a vector
  Y <- drop(Y)
  G <- drop(G)

  if(inter.model=="all"){
    runmods <- c("bw","b","w","n")
  }else{
    runmods <- inter.model
  }

  runmods <- c("BDLIM_n","BDLIM_bw","BDLIM_b","BDLIM_w")[which( c("N","BW","B","W") %in% toupper(runmods) )]

  fit <- list()

  #fit normal linear model
  if(family$family=="gaussian"){
    if(is.null(prior$sigma)) prior$sigma <- c(1/2,0)

    for(Gmodel in runmods){
      if(!missing(seed)) set.seed(seed)
      cat(paste0("\nFitting: BDLIM-",substring(Gmodel,7,15),"\n"))
        if(Gmodel=="BDLIM_bw"){
          fit[[Gmodel]] <- bdlimlmbw(Y,x,z1,G,B,niter,nburn,nthin,prior)
          fit[[Gmodel]]$model <- "BDLIM_bw"
        }else if(Gmodel=="BDLIM_b"){
          fit[[Gmodel]] <- bdlimlmb(Y,x,z1,G,B,niter,nburn,nthin,prior)
          fit[[Gmodel]]$model <- "BDLIM_b"
        }else if(Gmodel=="BDLIM_w"){
          fit[[Gmodel]] <- bdlimlmw(Y,x,z1,G,B,niter,nburn,nthin,prior)
          fit[[Gmodel]]$model <- "BDLIM_w"
        }else if(Gmodel=="BDLIM_n"){
          fit[[Gmodel]] <- bdlimlmoverall(Y,x,z1,G,B,niter,nburn,nthin,prior)
          fit[[Gmodel]]$model <- "BDLIM_n"
        }

    }
  }else{
      # for GLM
      for(Gmodel in runmods){
        if(!missing(seed)) set.seed(seed)
        cat(paste0("\nFitting: BDLIM-",substring(Gmodel,7,15),"\n"))
        fit[[Gmodel]] <- bdlimglmall(Y=Y,X=x,Z=z1,G=G,B=B,model=substring(Gmodel,7,15),niter=niter,nburn=nburn,nthin=nthin,prior=prior,family=family)

        fit[[Gmodel]]$model <- Gmodel

      }
  }





  fit[[Gmodel]]$inter.model <- Gmodel
  fit[[Gmodel]]$family <- family$family
  fit[[Gmodel]]$link <- family$link

  #summarize model fit
  modelfit <- NULL
  for(Gmodel in runmods){
    modelfit <- rbind(modelfit,subset(fit[[Gmodel]]$DIC,G=="Overall"))
  }
  row.names(modelfit) <- runmods
  probs <- NULL
  for(i in 1:nrow(modelfit)) probs <- c(probs,1/sum(exp(modelfit$Dbar[i]-modelfit$Dbar)))
  modelfit$modelprob <- probs
  modelfit <- modelfit[,c("DIC","pD","modelprob")]
  fit$modelfit <- modelfit[order(-modelfit$modelprob),]



  fit$family <- family




  fit$B <- B
  fit$basis.opts <- basis.opts
  fit$prior <- prior
  fit$call <- match.call()

  class(fit) <- "bdlim"

  return(fit)
}

