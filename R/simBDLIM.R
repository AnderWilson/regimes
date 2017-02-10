

#' Simulate data from BDLIM
#'
#' This function uses a simular design to scenario 2 in the BDLIM paper but with different data.
#'
#' @param n Number of observations must be between 100 and 1000
#' @param design Scenario from paper, simulation B, scenarios 1-5.

simBDLIM <- function(n,design){
  
  data('AirPollWeekly', package='regimes', envir=environment())
  AirPollWeekly <- get("AirPollWeekly", envir=environment()) 
  
  if(missing(n)) n <- 500
  nstar <- min(max(n,100),1000)
  X <- as.matrix(AirPollWeekly[1:nstar,paste0("PM25_",1:37)])
  G <- floor(runif(nstar)*2)
  Z <- matrix(rnorm(nstar*10),nstar,10)
  t <- seq(0,1,length=ncol(X))
  if(design==1){
    w0 = sqrt(dbeta(t,5,5))
    w1 = sqrt(dbeta(t,5,5))
    beta0 <- 0.1
    beta1 <- 0.1
  }else if(design==2){
    w0 = sqrt(dbeta(t,5,5))
    w1 = sqrt(dbeta(t,5,5))
    beta0 <- 0.1
    beta1 <- -0.2
  }else if(design==3){
    w0 = sqrt(dbeta(t,5,5))
    w1 = sqrt(dbeta(t,5,5))
    beta0 <- 0.1
    beta1 <- 0
  }else if(design==4){
    w0 = sqrt(dbeta(t,5,5))
    w1 = sin(t*pi - pi/4)/sqrt(mean(sin(t*pi - pi/4)^2))
    beta0 <- 0.1
    beta1 <- 0.2
  }else if(design==5){
    w0 = sqrt(dbeta(t,5,5))
    w1 = sin(t*pi - pi/4)/sqrt(mean(sin(t*pi - pi/4)^2))
    beta0 <- 0.1
    beta1 <- 0.1
  }
  
  
  gamma <- rnorm(ncol(Z))
  Y <- Z%*%gamma + beta0*(X%*%w0)*(G==0) + beta1*(X%*%w1)*(G==1) + rnorm(nrow(X))*6
  
  beta<-c(beta0,beta1)
  names(beta) <- c("beta0","beta1")
  wt<-cbind(w0,w1)
  colnames(wt) <- c("wt0","wt1")
  
  return(list(Y=Y, X=X, Z=Z, G=G, beta=beta, wt=wt, gamma=gamma, opts=list(n=n, formulation="y = beta_g*(X*wt_g) + Z*gamma + epsilon")))
}