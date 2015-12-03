#-------------------------------------------------------------------------------------------------
#' BDLIM for glm
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
#'





bdlimglmoall <- function(Y,X,Z,G,B,model,niter,nburn,nthin,prior,post){

  #usefull stuff
  px = ncol(X)
  pz = ncol(Z)
  n <- length(Y)
  hyperplane <- colSums(B$psi)



  ug <- levels(G)
  if(model=="bw"){
    ngb <- nlevels(G)
    ngw <- nlevels(G)
    Gb <- as.numeric(as.factor(G))
    Gw <- as.numeric(as.factor(G))
    ugw <- 1:ngw
    ugb <- 1:ngb
  }else if(model=="b"){
    ngb <- nlevels(G)
    ngw <- 1
    Gb <- as.numeric(as.factor(G))
    Gw <- rep(1,length(G))
    ugw <- rep(1,ngb)
    ugb <- 1:ngb
  }else if(model=="w"){
    ngb <- 1
    ngw <- nlevels(G)
    Gb <- rep(1,length(G))
    Gw <- as.numeric(as.factor(G))
    ugw <- 1:ngw
    ugb <- rep(1,ngw)
  }else{
    ngb <- 1
    ngw <- 1
    Gb <- rep(1,length(G))
    Gw <- rep(1,length(G))
    ugw <- 1
    ugb <- 1
  }
  ng <- max(ngb,ngw)

  #starting values
  theta <- unname(lm(rep(1,nrow(B$psi))~B$psi-1)$coef)
  theta <- theta*sqrt(nrow(B$psi)/sum(theta^2))
  theta <- rep(theta,ngw)
  Xtheta <- matrix(0,n,ngb)
  for(g in 1:ng) Xtheta[Gb==g,ugb[g]] <- X[Gb==g,] %*% theta[(ugw[g]-1)*px+1:px]
  glmfit <- glm(Y~Xtheta+Z-1)
  kappa <- glmfit$coef[1:ngb]
  gamma <- glmfit$coef[-c(1:ngb)]
  mu <-  Z%*%gamma + Xtheta%*%kappa


  #place to store estiamtes
  theta.keep <- matrix(NA,niter,px*ngw)
  kappa.keep <- matrix(NA,niter,ngb)
  gamma.keep <- matrix(NA,niter,pz)
  res.keep <- matrix(NA,niter,n)


  pb <- txtProgressBar(min=0,max=niter, style=3, width=20)

  #MCMC
  for(i in 1:niter){
    setTxtProgressBar(pb, i)
    for(j in 1:nthin){

      #update gamma
      yslice <- sum(post(Y,mu)) - sum(gamma^2)/2 + log(runif(1))
      vgamma <-  rnorm(pz)*sqrt(prior$gamma)
      ang <- slicemax <- runif(1)*2*pi
      slicemin <- slicemax-2*pi
      notaccepted <- TRUE
      while(notaccepted){
        gamma0 = gamma*cos(ang) + vgamma*sin(ang)
        mu0 <- mu - Z%*%gamma + Z%*%gamma0
        if(sum(post(Y,mu0)) - sum(gamma0^2)/2  > yslice){
          gamma <- gamma0
          mu <- mu0
          notaccepted <- FALSE
        }
        if(ang<0){
          slicemin = ang
        }else{
          slicemax = ang
        }
        ang = runif(1,slicemin,slicemax)
      }


      #update theta (parameters for weight function)
      for(g in 1:ngw){
        yslice <- sum(post(Y[Gw==g],mu[Gw==g])) - sum(theta[(ugw[g]-1)*px+1:px]^2)/2 + log(runif(1))
        vtheta <- rnorm(px)
        vtheta <- vtheta*sqrt(nrow(B$psi)/sum(vtheta^2))
        ang <- slicemax <- runif(1)*2*pi
        slicemin <- slicemax-2*pi
        notaccepted <- TRUE
        Xtheta0 <- Xtheta
        while(notaccepted){
          theta0 = theta[(ugw[g]-1)*px+1:px]*cos(ang) + vtheta*sin(ang)
          theta0 <- theta0*sqrt(nrow(B$psi)/sum(theta0^2))
          if(hyperplane%*%theta0 > 0){

            for(gb in ugb[which(ugw==g)]) Xtheta0[Gb==gb & Gw==g,ugb[gb]] <- X[Gb==gb & Gw==g,] %*% theta0

            mu0[Gw==g] <- mu[Gw==g] - as.matrix(Xtheta[Gw==g,])%*%kappa + as.matrix(Xtheta0[Gw==g,])%*%kappa
            if(sum(post(Y[Gw==g],mu0[Gw==g])) - sum(theta0^2)/2  > yslice){
              theta[(ugw[g]-1)*px+1:px] <- theta0
              mu[Gw==g] <- mu0[Gw==g]
              notaccepted <- FALSE
              Xtheta<- Xtheta0
            }
          }
          if(ang<0){
            slicemin = ang
          }else{
            slicemax = ang
          }
          ang = runif(1,slicemin,slicemax)
        }
      }


      #update kappa
      yslice <- sum(post(Y,mu)) + log(runif(1))
      vkappa <-  rnorm(ngb)*sqrt(prior$beta)
      ang <- slicemax <- runif(1)*2*pi
      slicemin <- slicemax-2*pi
      notaccepted <- TRUE
      while(notaccepted){
        kappa0 = kappa*cos(ang) + vkappa*sin(ang)
        mu0 <- mu - Xtheta%*%kappa + Xtheta%*%kappa0
        if(sum(post(Y,mu0)) > yslice){
          kappa <- kappa0
          mu <- mu0
          notaccepted <- FALSE
        }
        if(ang<0){
          slicemin = ang
        }else{
          slicemax = ang
        }
        ang = runif(1,slicemin,slicemax)
      }




    }

    theta.keep[i,] <- theta
    gamma.keep[i,] <- gamma
    kappa.keep[i,] <- kappa
    res.keep[i,] <- mu
  }


  #DIC for each observation. this should be done before the rescaling.
  D_thetabar <- -2*post(Y,colMeans(res.keep[(nburn+1):niter,]))
  Dbar <- rep(0,n)
  for(i in 1:n) Dbar[i] <- Dbar[i] +  -2*mean(post(as.numeric(Y[i]),res.keep[(nburn+1):niter,i]))
  pD <- Dbar - D_thetabar
  DIC <- pD+Dbar


  if(ngw>1){
    theta <- list()
    for(g in 1:ngw){
      theta[[as.character(ug[g])]] <- theta.keep[(nburn+1):niter,(which(ug==ug[g])-1)*px+1:px]
    }
  }else{
    theta <- theta.keep[(nburn+1):niter,]
  }

  if(ngb>1) colnames(kappa.keep) <- ug
  colnames(gamma.keep) <- colnames(Z)

  #save output
  out <- list(beta=kappa.keep[(nburn+1):niter,],
              theta=theta,
              gamma=gamma.keep[(nburn+1):niter,]
  )

  #save DIC
  D <- D_thetabar
  out$DIC <- data.frame(G="Overall",DIC=sum(DIC),pD=sum(pD),Dbar=sum(Dbar),D=sum(D))
  if(!is.null(G)) out$DIC <- rbind(out$DIC,aggregate(cbind(DIC,pD,Dbar,D), by=list(G=as.character(G)), sum))

  return(out)
}


