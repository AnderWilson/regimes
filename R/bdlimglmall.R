#-------------------------------------------------------------------------------------------------
#' BDLIM for glm
#'
#' This estimates the model for a single group or the overall effect
#' @param Y Outcome vector
#' @param X Exposure matrix
#' @param Z Matrix of covariates. An intercept will be added
#' @param G Vector indicating group membership
#' @param B Basis object from build.basis
#' @param model Indicator of how G enters the model. If 'bw' both the betas and weight functions will vary between groups. If 'b' then only the betas will vary between groups. If 'w' then only the wieght function will vary by group. If "n" then neither beta or the wieght function will vary by group. If a vector of any combination of the four choices is provided then those models will be run and compared. The option 'all' runs all four models and compares them.
#' @param niter Number of MCMC iterations
#' @param nburn Number of MCMC iterations to be discarded as burning
#' @param nthin Number of draws taken to obtain one sample
#' @param prior List with the entries:  betavar = the prior variance for beta; and gamma = the prior variance for the covarites. The priors on beta and gamma are iid normal mean zero.
#' @param family Family as a funciton for glm
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @author Ander Wilson
#'




# Y=Y;X=x;Z=z1;G=drop(G);B=B;model=substring(Gmodel,7,15);niter=niter;nburn=nburn;nthin=nthin;prior=prior;family=family
bdlimglmall <- function(Y,X,Z,G,B,model,niter,nburn,nthin,prior,family=family){

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
    Gb <- rep(1,n)
    Gw <- rep(1,n)
    ugw <- 1
    ugb <- 1
  }
  ng <- max(ngb,ngw)

  #starting values
  theta <- unname(lm(rep(1,nrow(B$psi))~B$psi-1)$coef)  # start with flat weight
  theta <- theta*sqrt(nrow(B$psi)/sum(theta^2))  #scale starting weights
  theta <- rep(theta,ngw)  # repeat it needed nuber of times  ( should have  ncol(B$psi)*ngw columns )
  Xtheta <- matrix(0,n,ngb)  # weighted exposures ( should have ng columns )
  for(g in 1:ng) Xtheta[Gb==g,ugb[g]] <- X[Gb==g,] %*% theta[(ugw[g]-1)*px+1:px]  #populate matrix
  glmfit <- glm(Y~Xtheta+Z-1)  # starting values
  kappa <- glmfit$coef[1:ngb]   # starting values for 
  gamma <- glmfit$coef[-c(1:ngb)]  # coef for covariates
  mu <-  Z%*%gamma + Xtheta%*%kappa  # predictor


  #place to store estiamtes
  theta_keep <- matrix(NA,niter,px*ngw)
  kappa_keep <- matrix(NA,niter,ngb)
  gamma_keep <- matrix(NA,niter,pz)
  res_keep <- matrix(NA,niter,n)


  pb <- txtProgressBar(min=0,max=niter, style=3, width=20)
  
  #MCMC
  for(i in 1:niter){
    setTxtProgressBar(pb, i)
    for(j in 1:nthin){

      #update gamma
      yslice <- -sum(family$dev.resids(Y,family$linkinv(mu),1))/2 - sum(gamma^2)/2 + log(runif(1))
      vgamma <-  rnorm(pz)*sqrt(prior$gamma)
      ang <- slicemax <- runif(1)*2*pi
      slicemin <- slicemax-2*pi
      notaccepted <- TRUE
      while(notaccepted){
        gamma0 = gamma*cos(ang) + vgamma*sin(ang)
        mu0 <- mu - Z%*%gamma + Z%*%gamma0
        if(-sum(family$dev.resids(Y,family$linkinv(mu0),1))/2 - sum(gamma0^2)/2  > yslice){
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
        yslice <- -sum(family$dev.resids(Y[Gw==g],family$linkinv(mu[Gw==g]),1))/2 - sum(theta[(ugw[g]-1)*px+1:px]^2)/2 + log(runif(1))
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
            if(-sum(family$dev.resids(Y[Gw==g],family$linkinv(mu[Gw==g]),1))/2 - sum(theta0^2)/2  > yslice){
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
      yslice <- -sum(family$dev.resids(Y,family$linkinv(mu),1))/2 + log(runif(1))
      vkappa <-  rnorm(ngb)*sqrt(prior$beta)
      ang <- slicemax <- runif(1)*2*pi
      slicemin <- slicemax-2*pi
      notaccepted <- TRUE
      while(notaccepted){
        kappa0 = kappa*cos(ang) + vkappa*sin(ang)
        mu0 <- mu - Xtheta%*%kappa + Xtheta%*%kappa0
        if(-sum(family$dev.resids(Y,family$linkinv(mu0),1))/2 > yslice){
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

    theta_keep[i,] <- theta
    gamma_keep[i,] <- gamma
    kappa_keep[i,] <- kappa
    res_keep[i,] <- mu
  }


  #DIC for each observation. this should be done before the rescaling.
  D_thetabar <- family$dev.resids(Y,family$linkinv(colMeans(res_keep[(nburn+1):niter,])),1)
  Dbar <- rep(0,n)
  for(i in 1:n) Dbar[i] <- Dbar[i] +  mean(family$dev.resids(rep(Y[i],niter-nburn),family$linkinv(res_keep[(nburn+1):niter,i]),1))
  pD <- Dbar - D_thetabar
  DIC <- pD+Dbar

  # store thetas
  if(ngw>1){
    theta <- list()
    for(g in 1:ngw){
      theta[[as.character(ug[g])]] <- theta_keep[(nburn+1):niter,(which(ug==ug[g])-1)*px+1:px]
    }
  }else{
    theta <- theta_keep[(nburn+1):niter,]
  }
  # format kappa_keep
  if(ngb>1) colnames(kappa_keep) <- ug
  colnames(gamma_keep) <- colnames(Z)

  #save output
  out <- list(beta=kappa_keep[(nburn+1):niter,],
              theta=theta,
              gamma=gamma_keep[(nburn+1):niter,]
  )

  #save DIC
  D <- D_thetabar
  out$DIC <- data.frame(G="Overall",DIC=sum(DIC),pD=sum(pD),Dbar=sum(Dbar),D=sum(D))
  if(!is.null(G)){
    bysubgroup <- aggregate(cbind(DIC,pD,Dbar,D), by=list(G=as.character(G)), sum)
    colnames(bysubgroup) <- colnames(out$DIC)
  } 
  out$DIC <- rbind(out$DIC,bysubgroup)

  return(out)
}


