#-------------------------------------------------------------------------------------------------
#' BDLIM-bw for linear model
#'
#' This estimates the model for a group specific weight funciton and group specific effects
#' @param Y Outcome vector
#' @param X Exposure matrix
#' @param Z Matrix of covariates. An intercept will be added
#' @param G Vector indicating group membership
#' @param B Basis object from build.basis
#' @param niter Number of MCMC iterations
#' @param nburn Number of MCMC iterations to be discarded as burning
#' @param nthin Number of draws taken to obtainone sampl
#' @param prior List with the entries: sigma = a numeric 2-vector with the shape and rate paramters for the pirior in the error precision (1/sigma^2); betavar = the prior variance for beta; and gamma = the prior variance for the covarites. The priors on beta and gamma are iid normal mean zero.
#' @author Ander Wilson
#' @importFrom GIGrvg rgig
#'





bdlimlmbw <- function(Y,X,Z,G,B,niter,nburn,nthin,prior){

  #prepare design matrix for groups
  grps <- levels(G)
  x.all <- NULL
  for(g in 1:nlevels(G)){
    temp <- X
    colnames(temp) <- paste0("g",g,colnames(X))
    temp[which(G!=grps[g]),] <- 0
    x.all <- cbind(x.all,temp)
    rm("temp")
  }
  X <- x.all
  rm(list=c("x.all"))


  #quantities needed for updates
  E <- eigen( t(X)%*%X)
  Exy <- t(E$vectors)%*%(t(X)%*%Y)
  Exz <- t(E$vectors)%*%(t(X)%*%Z)
  EZ <- eigen( t(Z)%*%Z)
  EZzy <- t(EZ$vectors)%*%(t(Z)%*%Y)
  EZzx <- t(EZ$vectors)%*%(t(Z)%*%X)


  #starting values
  sig2inv <-1
  thetastar <- rnorm(ncol(X)*nlevels(G))
  gamma <- rnorm(ncol(Z))
  kappa <- rep(1,nlevels(G))

  #place to store estiamtes
  thetastar.keep <- matrix(NA,niter,ncol(X)*nlevels(G))
  gamma.keep <- matrix(NA,niter,ncol(Z))
  sigma.keep <- rep(NA,niter)
  res.keep <- matrix(NA,niter,length(Y))


  pb <- txtProgressBar(min=0,max=niter, style=3, width=20)

  #MCMC
  for(i in 1:niter){
    setTxtProgressBar(pb, i)
    for(j in 1:nthin){
      #update thetastar
      c1 = scale(E$vectors,E$values + rep(1/(prior$beta*kappa*sig2inv),each=ncol(X)/nlevels(G)), center=FALSE)
      c2 = scale(E$vectors,sqrt(sig2inv*E$values + rep(1/(prior$beta*kappa),each=ncol(X)/nlevels(G))), center=FALSE)
      thetastar <- drop(c1%*%Exy-c1%*%Exz%*%gamma  + c2%*%rnorm(ncol(X)))

      if(prior$beta!=Inf) for(g in 1:nlevels(G)) kappa[g] <-  rgig(1,lambda= -(ncol(X)/nlevels(G)-1)/2, chi=sum(thetastar[(g-1)*ncol(X)/nlevels(G)+1:(ncol(X)/nlevels(G))]^2)/prior$beta, psi=1)


      #update gamma
      c1z = scale(EZ$vectors,EZ$values + c(rep(0,nlevels(G)),rep(1/(prior$gamma*sig2inv),ncol(Z)-nlevels(G))), center=FALSE)
      c2z = scale(EZ$vectors,sqrt(sig2inv*EZ$values + c(rep(0,nlevels(G)),rep(1/prior$gamma,ncol(Z)-nlevels(G)))), center=FALSE)
      gamma <- drop(c1z%*%EZzy-c1z%*%EZzx%*%thetastar + c2z%*%rnorm(ncol(Z)))

      #update sig2inv
      sig2inv <- rgamma( 1, prior$sigma[1] + length(Y)/2 , prior$sigma[2] + sum((Y-X%*%thetastar-Z%*%gamma)^2)/2 )

    }
    thetastar.keep[i,] <- thetastar
    gamma.keep[i,] <- gamma
    sigma.keep[i] <- sig2inv
    res.keep[i,] <- Y-X%*%thetastar-Z%*%gamma
  }


  #DIC for each observation. this should be done before the rescaling.
  Dbar <- log(2*pi) -mean(log(sigma.keep[(nburn+1):niter])) + colMeans(t(scale(t(res.keep[(nburn+1):niter,]^2),center=FALSE,scale=1/sigma.keep[(nburn+1):niter])))
  D <- log(2*pi)-log(mean(sigma.keep[(nburn+1):niter])) + mean(sigma.keep[(nburn+1):niter])*colMeans(res.keep[(nburn+1):niter,])^2
  pD <- Dbar - D
  DIC <- pD+Dbar


  #partition theta star into beta and theta and other rescaling/transforming
  sigma.keep <- 1/sqrt(sigma.keep)

  beta.keep <- matrix(NA,niter,nlevels(G))
  theta <- list()
  for(g in 1:nlevels(G)){
    beta.keep[,g] <- sqrt(rowSums(thetastar.keep[,grep(paste0("g",g,"x"),colnames(X))]^2))/sqrt(nrow(B$psi)) * sign(colSums(B$psi%*%t(thetastar.keep[,grep(paste0("g",g,"x"),colnames(X))])))
    theta[[as.character(grps[g])]] <- thetastar.keep[,grep(paste0("g",g,"x"),colnames(X))]/drop(beta.keep[,g])
    theta[[as.character(grps[g])]] <- theta[[as.character(grps[g])]][(nburn+1):niter,]

  }
  beta.keep <- beta.keep[(nburn+1):niter,]
  colnames(beta.keep) <- grps
  colnames(gamma.keep) <- colnames(Z)

  #save output
  out <- list(beta=beta.keep,
              theta=theta,
              gamma=gamma.keep[(nburn+1):niter,],
              sigma=sigma.keep[(nburn+1):niter]
  )


  #save DIC
  out$DIC <- data.frame(G="Overall",DIC=sum(DIC),pD=sum(pD),Dbar=sum(Dbar),D=sum(D))
  if(!is.null(G)) out$DIC <- rbind(out$DIC,aggregate(cbind(DIC,pD,Dbar,D), by=list(G=as.character(G)), sum))

  return(out)
}

