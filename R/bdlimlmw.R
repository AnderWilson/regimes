#-------------------------------------------------------------------------------------------------
#' BDLIM-none for linear model
#'
#' This estimates the model for group specific weight funcitons but a common effect
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





bdlimlmw <- function(Y,X,Z,G,B,niter,nburn,nthin,prior){

  #prepare design matrix for groups
  grps <- levels(G)
  Xg <- matrix(0,length(Y),nlevels(G))
  for(g in 1:nlevels(G))  Xg[G==grps[g],g] <- 1
  colnames(Xg) <- paste0("g",1:nlevels(G),"theta")

  #quantities needed for updates
  EZ <- eigen( t(Z)%*%Z)
  hyperplane <- colSums(B$psi)

  #starting values
  theta <- lm(rep(1,nrow(B$psi))~B$psi-1)$coef
  theta <- matrix(theta,length(theta),nlevels(G),byrow=FALSE)
  theta <- theta*sqrt(nrow(B$psi))/sqrt(colSums(theta^2))
  Xpsitheta <- rowSums((X%*%theta)*Xg)

  gamma <- drop(lm(Y~Xpsitheta+Z-1)$coef)
  res <- lm(Y~Xpsitheta+Z-1)$residuals
  beta <- gamma[1]
  gamma <- gamma[-1]
  sig2inv <- rgamma( 1, prior$sigma[1] + length(Y)/2 , prior$sigma[2] + sum(res^2)/2 )


  #place to store estiamtes
  theta.keep <- matrix(NA,niter,ncol(X)*nlevels(G))
  gamma.keep <- matrix(NA,niter,ncol(Z))
  beta.keep <- rep(NA,niter)
  sigma.keep <- rep(NA,niter)
  res.keep <- matrix(NA,niter,length(Y))


  pb <- txtProgressBar(min=0,max=niter, style=3, width=20)

  #MCMC
  for(i in 1:niter){
    setTxtProgressBar(pb, i)
    for(j in 1:nthin){
      res <- Y-Z%*%gamma

      #update beta
      beta <- sum(res*Xpsitheta)/(sum(Xpsitheta^2)+1/prior$beta) + rnorm(1)/sqrt(sig2inv*sum(Xpsitheta^2)+1/prior$beta)

      #update theta
      for(g in 1:nlevels(G)){
      yslice <- -sig2inv*sum((res[Xg[,g]==1]-beta*Xpsitheta[Xg[,g]==1])^2)/2 + sum(theta[,g]^2)/2 + log(runif(1))
      vbeta <-  rnorm(ncol(X))
      vbeta <- vbeta*sqrt(nrow(B$psi))/sqrt(sum(vbeta^2))
      slicemax <- runif(1)*2*pi
      slicemin <- slicemax-2*pi
      notaccepted <- TRUE

      while(notaccepted){
        angle = runif(1,slicemin,slicemax)
        theta0 = theta[,g]*cos(angle) + vbeta*sin(angle)
        theta0 <- theta0*sqrt(nrow(B$psi))/sqrt(sum(theta0^2))
        if(hyperplane%*%theta0 > 0){
          if(-sig2inv*sum((res[Xg[,g]==1]-beta*(X[Xg[,g]==1,]%*%theta0))^2)/2 + sum(theta0^2)/2  > yslice){
            theta[,g] = theta0
            notaccepted = FALSE
            Xpsitheta[Xg[,g]==1] <- X[Xg[,g]==1,]%*%theta0
          }
        }
        if(angle<0){
          slicemin = angle
        }else{
          slicemax = angle
        }
      }
      }

      #update gamma
      c2z = scale(EZ$vectors,sqrt(sig2inv*EZ$values +  c(rep(0,nlevels(G)),rep(1/prior$gamma,ncol(Z)-nlevels(G)))), center=FALSE)
      gamma <- drop(sig2inv*(c2z%*%t(c2z))%*%(t(Z)%*%(Y-Xpsitheta*beta)) + c2z%*%rnorm(ncol(Z)))


      #update sig2inv
      sig2inv <- rgamma( 1, prior$sigma[1] + length(Y)/2 , prior$sigma[2] + sum((Y-Xpsitheta*beta-Z%*%gamma)^2)/2 )


    }
    theta.keep[i,] <- c(theta)
    gamma.keep[i,] <- gamma
    beta.keep[i] <- beta
    sigma.keep[i] <- sig2inv
    res.keep[i,] <- Y-Xpsitheta*beta-Z%*%gamma
  }

  #DIC for each observation. this should be done before the rescaling.
  Dbar <- log(2*pi) -mean(log(sigma.keep[(nburn+1):niter])) + colMeans(t(scale(t(res.keep[(nburn+1):niter,]^2),center=FALSE,scale=1/sigma.keep[(nburn+1):niter])))
  D <- log(2*pi)-log(mean(sigma.keep[(nburn+1):niter])) + mean(sigma.keep[(nburn+1):niter])*colMeans(res.keep[(nburn+1):niter,])^2
  pD <- Dbar - D
  DIC <- pD+Dbar


  #partition theta star into beta and theta and other rescaling/transforming
  sigma.keep <- 1/sqrt(sigma.keep)
  colnames(theta.keep) <- paste0(rep(paste0("g",1:nlevels(G)),each=ncol(B$psi)),"_",1:ncol(B$psi))
  theta.keep2 <- list()
  for(g in 1:nlevels(G)) theta.keep2[[as.character(grps[g])]] <- theta.keep[(nburn+1):niter,1:ncol(B$psi)+ncol(B$psi)*(g-1)]

  colnames(gamma.keep) <- colnames(Z)

  #save output
  out <- list(beta=beta.keep[(nburn+1):niter],
              theta=theta.keep2,
              gamma=gamma.keep[(nburn+1):niter,],
              sigma=sigma.keep[(nburn+1):niter]
  )

  #save DIC
  out$DIC <- data.frame(G="Overall",DIC=sum(DIC),pD=sum(pD),Dbar=sum(Dbar),D=sum(D))
  if(!is.null(G)) out$DIC <- rbind(out$DIC,aggregate(cbind(DIC,pD,Dbar,D), by=list(G=as.character(G)), sum))

  return(out)
}



