#-------------------------------------------------------------------------------------------------
#' bdlimlmoverall
#'
#' This estimates the model for a single group or the overall effect
#' @param y Outcome vector
#' @param X Exposure matrix
#' @param Z Matrix of covariates. An intercept will be added
#' @param B Basis object from build.basis
#' @param niter Number of MCMC iterations
#' @param nburn Number of MCMC iterations to be discarded as burning
#' @param nthin Number of draws taken to obtainone sampl
#' @param prior A vector of lenght 2 whose elements (a,b) are the hyper parameters of a Gamma prior on sigma^(-2) with mean a/b.
#' @author Ander Wilson


bdlimlmoverall <- function(Y,X,Z,G,B,niter,nburn,nthin,prior){

  #quantities needed for updates
  E <- eigen(t(X)%*%X)
  Exy <- t(E$vectors)%*%(t(X)%*%Y)
  Exz <- t(E$vectors)%*%(t(X)%*%Z)
  EZ <- eigen( t(Z)%*%Z)
  EZzy <- t(EZ$vectors)%*%(t(Z)%*%Y)
  EZzx <- t(EZ$vectors)%*%(t(Z)%*%X)

  #starting values
  sig2inv <-1
  thetastar <- rnorm(ncol(X))
  gamma <- rnorm(ncol(Z))
  kappa <- 0

  #place to store estiamtes
  thetastar.keep <- matrix(NA,niter,ncol(X))
  gamma.keep <- matrix(NA,niter,ncol(Z))
  sigma.keep <- rep(NA,niter)
  res.keep <- matrix(NA,niter,length(Y))

  pb <- txtProgressBar(min=0,max=niter, style=3, width=20)

  #MCMC
  for(i in 1:niter){
    setTxtProgressBar(pb, i)
    for(j in 1:nthin){
      #update thetastar
      c1 = scale(E$vectors,E$values+kappa/sig2inv, center=FALSE)
      c2 = scale(E$vectors,sqrt(sig2inv*E$values + kappa), center=FALSE)
      thetastar <- drop(c1%*%Exy-c1%*%Exz%*%gamma  + c2%*%rnorm(ncol(X)))

      #update beta2
      if(prior$beta!=Inf) kappa <- rgig(1,lambda=-(ncol(X)-1)/2, chi=sum(thetastar^2)/prior$beta, psi=1)

      #update gamma
      c1z = scale(EZ$vectors,EZ$values + (c(rep(0,nlevels(G)),rep(1/prior$gamma*sig2inv,ncol(Z)-nlevels(G)))), center=FALSE)
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

  #DIC for each observation.
  Dbar <- log(2*pi) -mean(log(sigma.keep[(nburn+1):niter])) + colMeans(t(scale(t(res.keep[(nburn+1):niter,]^2),center=FALSE,scale=1/sigma.keep[(nburn+1):niter])))
  D <- log(2*pi)-log(mean(sigma.keep[(nburn+1):niter])) + mean(sigma.keep[(nburn+1):niter])*colMeans(res.keep[(nburn+1):niter,])^2
  pD <- Dbar - D
  DIC <- pD+Dbar

  #partition theta star into beta and theta and other rescaling/transforming
  beta.keep <-  sqrt(rowSums(thetastar.keep^2))/sqrt(nrow(B$psi)) * sign(colSums(B$psi%*%t(thetastar.keep)))
  theta.keep <- thetastar.keep/beta.keep
  sigma.keep <- drop(sqrt(sigma.keep))
  colnames(gamma.keep) <- colnames(Z)

  #save output
  out <- list(beta=beta.keep[(nburn+1):niter],
              theta=theta.keep[(nburn+1):niter,],
              gamma=gamma.keep[(nburn+1):niter,],
              sigma=sigma.keep[(nburn+1):niter]
  )

  #save DIC
  out$DIC <- data.frame(G="Overall",DIC=sum(DIC),pD=sum(pD),Dbar=sum(Dbar),D=sum(D))
  if(!is.null(G)) out$DIC <- rbind(out$DIC,aggregate(cbind(DIC,pD,Dbar,D), by=list(G=as.character(G)), sum))

  return(out)
}


