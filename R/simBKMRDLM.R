




#' Simulate data for BKMR-DLM.
#'
#' @param n Number of observations.
#' @param scenario Simulation scenario (A, B, C, D, E, F).
#' @param sd Error standard deviation.
#' @param seed An optional seed.
#' @param dta A list with data to use. The elements should be X, a list of exposure matrices, and Z a matrix of covariates. If not data is provided then data will be simulated automatically.
#'
#' @return A list containing simulated data
#' @importFrom utils data
#' @importFrom stats dnorm rnorm
#' @examples  
#' library(regimes)
#' #simulate data from scenario A
#' dat <- simBKMRDLM(n = 200, scenario="A", sd=1, seed=1234)
#' # Estimate model
#' # This may take a few minutes
#' # Increase iterations for a real analysis
#' fit <- bkmrdlm(y=dat$y,
#'                x=dat$x,
#'                z=dat$z,
#'                niter=100,
#'                gaussian=FALSE,
#'                polydegree=2)
#'                
#' summary(fit)
#' plot(fit)
#' @export

simBKMRDLM <- function(n = 200, scenario="A", sd=1, seed, dta){

  if(!missing(seed)){
    set.seed(seed)
  }
  
  if(missing(dta)){
    
    #load exposures
    data('AirPollWeekly', package='regimes', envir=environment())
    AirPollWeekly <- get("AirPollWeekly", envir=environment()) 
    
    if(n>nrow(AirPollWeekly)){
      stop(paste0("maximum number of observations in a simulated data sets is ",nrow(AirPollWeekly)))
    }
    
    obs <- sample(nrow(AirPollWeekly), n, replace=FALSE)
    
    # prep exposures
    x1 <- as.matrix(AirPollWeekly[obs,grep("PM",colnames(AirPollWeekly))])
    x2 <- as.matrix(AirPollWeekly[obs,grep("NO2",colnames(AirPollWeekly))])
    Xout <- list(x1,x2)
    if(as.character(scenario)%in%c("b","B","2","d","D","4","e","E","5","f","F","6")){
      x3 <- as.matrix(AirPollWeekly[obs,grep("CO",colnames(AirPollWeekly))])
      x4 <- as.matrix(AirPollWeekly[obs,grep("O3",colnames(AirPollWeekly))])
      x5 <- as.matrix(AirPollWeekly[obs,grep("SO2",colnames(AirPollWeekly))])
      Xout <- list(x1,x2,x3,x4,x5)
    }
    
    z <- matrix(rnorm(n*5),n,5)
    
  
  }else{
    n <- min(n,unlist(lapply(dta$X,nrow)),nrow(dta$Z))
    obs <- 1:n
    Xout <- lapply(dta$X,function(x) x[obs,])
    
    x1 <- Xout[[1]][obs,]
    x2 <- Xout[[2]][obs,]
    if(as.character(scenario)%in%c("a","A","1")){
      Xout <- Xout[1:2]
    }else if(as.character(scenario)%in%c("b","B","2","c","C","3","d","D","4")){
      x3 <- Xout[[3]][obs,]
    }
    
    z <- as.matrix(dta$Z)[1:n,]
    
  }

  if(as.character(scenario)%in%c("a","A","1")){
    
    

    # make true weight function
    t <- seq(0,1,length=ncol(x1))
    w1 <- dnorm(t,.5,.1)
    w1 = sign(w1)*sqrt(abs(w1))/sqrt(mean(w1^2))
    w1 <- w1 / sqrt(mean(w1^2))
    w2 = 1/(1+exp(-(t-.5)*20))
    w2 = sign(w2)*sqrt(abs(w2))/sqrt(mean(w2^2))
    w2 <- w2 / sqrt(mean(w2^2))


    # exposure reponse
    wx1 <- scale(x1%*%w1)
    wx2 <- scale(x2%*%w2)
    h1 <- 3/(1+exp(-2*wx1))
    h2 <- 2*wx2*(wx2>0)#log(+.5-min(wx2)+wx2)
    h12 <- - wx1*wx2
    h <- h1 + h2 + h12
    # plot(h~wx1)
    # plot(h~wx2)
    # plot(h1~wx1)
    # plot(h2~wx2)
    # summary(lm(h~wx1+wx2))
    # #
    # covariates
    gamma <- rnorm(ncol(z))

    #responses
    y <- h + z%*%gamma + rnorm(n)*sd

    out <- list(y=y, h=h, z=z, gamma=gamma, x=list(x1=x1, x2=x2), w=list(w1=w1, w2=w2))
  }else  if(as.character(scenario)%in%c("d","D","4")){
    
    # make true weight function
    t <- seq(0,1,length=ncol(x1))
    w1 <- dnorm(t,.5,.1)
    w1 = sign(w1)*sqrt(abs(w1))/sqrt(mean(w1^2))
    w1 <- w1 / sqrt(mean(w1^2))
    w2 = 1/(1+exp(-(t-.5)*20))
    w2 = sign(w2)*sqrt(abs(w2))/sqrt(mean(w2^2))
    w2 <- w2 / sqrt(mean(w2^2))
    w3 <- w2
    
    
    
    # exposure reponse
    wx1 <- scale(x1%*%w1)
    wx2 <- scale(x2%*%w2)
    wx3 <- scale(x3%*%w3)
    h1 <- 3/(1+exp(-2*wx1))
    h2 <- 2*wx2*(wx2>0)#log(+.5-min(wx2)+wx2)
    h3 <- -2*wx3*(wx3>0)#log(+.5-min(wx2)+wx2)
    h12 <- - wx1*wx2
    h <- h1 + h2 + h12 + h3
    
    # covariates
    gamma <- rnorm(ncol(z))
    
    
    #responses
    y <- h + z%*%gamma + rnorm(n)*sd
    
    out <- list(y=y, h=h, z=z, gamma=gamma, x=Xout, w=list(w1=w1, w2=w2, w3=w3, w4=rep(1,37), w5=rep(1,37)))
  }else if(as.character(scenario)%in%c("c","C","3")){
    
    # make true weight function
    t <- seq(0,1,length=ncol(x1))
    B <- svd(ns(t, df=4, intercept=TRUE))$u
    w1_coef <- drop( t(B) %*% dnorm(t,.5,.1) )
    w1_coef <- w1_coef / sqrt(sum(w1_coef^2)/length(t))
    w1 <- B %*% w1_coef
    w2_coef = drop( t(B) %*% c(1/(1+exp(-(t-.5)*20))))
    w2_coef <- w2_coef / sqrt(sum(w2_coef^2)/length(t))
    w2 = B %*% w2_coef
    
    
    # exposure reponse
    wx1 <- scale(x1%*%w1)
    wx2 <- scale(x2%*%w2)
    h1 <- 3/(1+exp(-2*wx1))
    h2 <- 2*wx2*(wx2>0)#log(+.5-min(wx2)+wx2)
    h12 <- - wx1*wx2
    h <- h1 + h2 + h12
    # plot(h~wx1)
    # plot(h~wx2)
    # plot(h1~wx1)
    # plot(h2~wx2)
    # summary(lm(h~wx1+wx2))
    # #
    # covariates
    gamma <- rnorm(ncol(z))
    
    #responses
    y <- h + z%*%gamma + rnorm(n)*sd
    
    out <- list(y=y, h=h, z=z, gamma=gamma, x=list(x1=x1, x2=x2), w=list(w1=w1, w2=w2))
  # }else  if(as.character(scenario)%in%c("d","D","4")){
  #   
  #   # make true weight function
  #   t <- seq(0,1,length=ncol(x1))
  #   B <- svd(ns(t, df=4, intercept=TRUE))$u
  #   w1_coef <- drop( t(B) %*% dnorm(t,.5,.1) )
  #   w1_coef <- w1_coef / sqrt(sum(w1_coef^2)/length(t))
  #   w1 <- B %*% w1_coef
  #   w2_coef = drop( t(B) %*% c(1/(1+exp(-(t-.5)*20))))
  #   w2_coef <- w2_coef / sqrt(sum(w2_coef^2)/length(t))
  #   w2 = B %*% w2_coef
  #   w3 <- w2
  #   
  #   # exposure reponse
  #   wx1 <- scale(x1%*%w1)
  #   wx2 <- scale(x2%*%w2)
  #   wx3 <- scale(x3%*%w3)
  #   h1 <- 3/(1+exp(-2*wx1))
  #   h2 <- 2*wx2*(wx2>0)#log(+.5-min(wx2)+wx2)
  #   h3 <- -2*wx3*(wx3>0)#log(+.5-min(wx2)+wx2)
  #   h12 <- - wx1*wx2
  #   h <- h1 + h2 + h12 + h3
  #   
  #   # covariates
  #   gamma <- rnorm(ncol(z))
  #   
  #   
  #   #responses
  #   y <- h + z%*%gamma + rnorm(n)*sd
  #   
  #   out <- list(y=y, h=h, z=z, gamma=gamma, x=Xout, w=list(w1=w1, w2=w2, w3=w3, w4=rep(1,37), w5=rep(1,37)))
  }else  if(as.character(scenario)%in%c("b","B","2")){
    
    # make true weight function
    t <- seq(0,1,length=ncol(x1))
    w1 <- dnorm(t,.5,.1)
    w1 = sign(w1)*sqrt(abs(w1))/sqrt(mean(w1^2))
    w1 <- w1 / sqrt(mean(w1^2))
    w2 = 1/(1+exp(-(t-.5)*20))
    w2 = sign(w2)*sqrt(abs(w2))/sqrt(mean(w2^2))
    w2 <- w2 / sqrt(mean(w2^2))
    w3 <- w2
    
    
    
    # exposure reponse
    wx1 <- scale(x1%*%w1)
    wx2 <- scale(x2%*%w2)
    wx3 <- scale(x3%*%w3)
    h1 <- wx1
    h2 <- wx2
    h3 <- wx3
    h12 <- wx1*wx2
    h13 <- wx1*wx3
    h23 <- wx2*wx3
    h123 <- wx1*wx2*wx3
    h <- h1 + h2 + h3 + h12 + h13 + h23 + h123
    # plot(h~wx1)
    # plot(h~wx2)
    # plot(h1~wx1)
    # plot(h2~wx2)
    # summary(lm(h~wx1+wx2))
    # #
    # covariates
    gamma <- rnorm(ncol(z))
    
    
    #responses
    y <- h + z%*%gamma + rnorm(n)*sd
    
    out <- list(y=y, h=h, z=z, gamma=gamma, x=Xout, w=list(w1=w1, w2=w2, w3=w3, w4=rep(1,37), w5=rep(1,37)))
    
  }else{
    stop("invalid scenario selected.")
  }

  return(out)
}
