#-------------------------------------------------------------------------------------------------
#' BDLIM-b for linear model
#'
#' This estimates the model for a common weight funciton but group specific effects
#' @param y Outcome vector
#' @param X Exposure matrix
#' @param Z Matrix of covariates. An intercept will be added
#' @param G Vector indicating group membership
#' @param B Basis object from build.basis
#' @param niter Number of MCMC iterations
#' @param nburn Number of MCMC iterations to be discarded as burning
#' @param nthin Number of draws taken to obtainone sampl
#' @param prior.sigma A vector of lenght 2 whose elements (a,b) are the hyper parameters of a Gamma prior on sigma^(-2) with mean a/b.
#' @author Ander Wilson





bdlimlmb <- function(Y,X,Z,G,B,niter,nburn,nthin,prior){


  #prepare design matrix for groups
  grps <- levels(G)
  Xg <- matrix(0,length(Y),nlevels(G))
  for(g in 1:nlevels(G))  Xg[G==grps[g],g] <- 1
  colnames(Xg) <- paste0("g",1:nlevels(G),"theta")


  #quantities needed for updates
  EZ <- eigen( t(Z)%*%Z)
  hyperplane <- colSums(B$psi)

  #starting values
  beta=rnorm(nlevels(G))
  theta <- lm(rep(1,nrow(B$psi))~B$psi-1)$coef
  theta <- theta*sqrt(nrow(B$psi))/sqrt(sum(theta^2))
  Xpsitheta <- drop(X%*%theta)
  gamma <- drop(lm(Y~Z-1)$coef)
  res <- lm(Y~Z-1)$residuals
  v <- drop(1/(t(Xg)%*%(Xpsitheta^2)/mean(res^2)))
  beta <- drop((1/mean(res^2))*diag(v)%*%(t(Xg)%*%drop(Xpsitheta*res)) + rnorm(nlevels(G))*sqrt(v))
  sig2inv <- rgamma( 1, prior$sigma[1] + length(Y)/2 , prior$sigma[2] + sum((Y-(Xg*Xpsitheta)%*%beta-Z%*%gamma)^2)/2 )

  #place to store estiamtes
  theta.keep <- matrix(NA,niter,ncol(X))
  gamma.keep <- matrix(NA,niter,ncol(Z))
  beta.keep <- matrix(NA,niter,nlevels(G))
  sigma.keep <- rep(NA,niter)
  res.keep <- matrix(NA,niter,length(Y))


  pb <- txtProgressBar(min=0,max=niter, style=3, width=20)

  #MCMC
  for(i in 1:niter){
    setTxtProgressBar(pb, i)
    for(j in 1:nthin){
      res <- Y-Z%*%gamma

      #update beta and variance
      v <- drop(1/(t(Xg)%*%(Xpsitheta^2)*sig2inv+1/prior$beta))
      beta <- drop(sig2inv*diag(v)%*%(t(Xg)%*%drop(Xpsitheta*res)) + rnorm(nlevels(G))*sqrt(v))

      #update theta
      Kg <- Xg%*%beta
      yslice <- -sig2inv*sum((res-Kg*Xpsitheta)^2)/2 + sum(theta^2)/2 + log(runif(1))
      vbeta <-  rnorm(ncol(X))
      slicemax <- runif(1)*2*pi
      slicemin <- slicemax-2*pi
      notaccepted <- TRUE

      while(notaccepted){
        angle = runif(1,slicemin,slicemax)
        theta0 = theta*cos(angle) + vbeta*sin(angle)
        theta0 <- theta0*sqrt(nrow(B$psi)/sum(theta0^2))
        if(hyperplane%*%theta0 > 0){
          if(-sig2inv*sum((res-Kg*(X%*%theta0))^2)/2 + sum(theta0^2)/2  > yslice){
            theta = theta0
            notaccepted = FALSE
          }
        }
        if(angle<0){
          slicemin = angle
        }else{
          slicemax = angle
        }
      }
      Xpsitheta <- drop(X%*%theta)


      #update gamma
      c2z = scale(EZ$vectors,sqrt(sig2inv*EZ$values + c(rep(0,nlevels(G)),rep(1/prior$gamma,ncol(Z)-nlevels(G)))), center=FALSE)
      gamma <- drop(sig2inv*(c2z%*%t(c2z))%*%(t(Z)%*%(Y-(Xg*Xpsitheta)%*%beta)) + c2z%*%rnorm(ncol(Z)))

      #update sig2inv
      sig2inv <- rgamma( 1, prior$sigma[1] + length(Y)/2 , prior$sigma[2] + sum((Y-(Xg*Xpsitheta)%*%beta-Z%*%gamma)^2)/2 )


    }
    theta.keep[i,] <- theta
    gamma.keep[i,] <- gamma
    beta.keep[i,] <- beta
    sigma.keep[i] <- sig2inv
    res.keep[i,] <- Y-(Xg*Xpsitheta)%*%beta-Z%*%gamma
  }

  #DIC for each observation. this should be done before the rescaling.
  Dbar <- log(2*pi) -mean(log(sigma.keep[(nburn+1):niter])) + colMeans(t(scale(t(res.keep[(nburn+1):niter,]^2),center=FALSE,scale=1/sigma.keep[(nburn+1):niter])))
  D <- log(2*pi)-log(mean(sigma.keep[(nburn+1):niter])) + mean(sigma.keep[(nburn+1):niter])*colMeans(res.keep[(nburn+1):niter,])^2
  pD <- Dbar - D
  DIC <- pD+Dbar


  #partition theta star into beta and theta and other rescaling/transforming
  sigma.keep <- drop(sqrt(sigma.keep))
  colnames(beta.keep) <- grps
  colnames(gamma.keep) <- colnames(Z)

  #save output
  out <- list(beta=beta.keep[(nburn+1):niter,],
              theta=theta.keep[(nburn+1):niter,],
              gamma=gamma.keep[(nburn+1):niter,],
              sigma=sigma.keep[(nburn+1):niter]
  )

  #save DIC
  out$DIC <- data.frame(G="Overall",DIC=sum(DIC),pD=sum(pD),Dbar=sum(Dbar),D=sum(D))
  if(!is.null(G)) out$DIC <- rbind(out$DIC,aggregate(cbind(DIC,pD,Dbar,D), by=list(G=as.character(G)), sum))

  return(out)
}
