


#' Simulate data for ACPME
#'
#' This function simulated data used in the paper.
#'
#' @param n Integer number of observations.
#' @param model Takes values 1 (default) or 2 to indicate the choice of model to simulate data from.
#' @param p Number of covariates. Default is 100 and must be at least 35.
#' @param m.main Number of main effects for the exposure. Only used for model 2 and defaults to 2.



simACPME <- function(n, model=1,p,m.main){

  if(missing(n)) n <- 500

  if(model==1){
    if(missing(p)) p <- 100
    if(p<35) p <- 35


    C <- matrix(rnorm(n*p),n,p)
    X1 <- rowSums(C[,1:10])/sqrt(11) + rnorm(n)/sqrt(11)
    X2 <- 1/sqrt(21)*rowSums(C[,6:15]) + 1/sqrt(42)*X1*rowSums(C[,16:20]) + 1/sqrt(21)*rowSums(C[,31:35]) + rnorm(n)/sqrt(21)

    X <- cbind(X1,X2,X1*X2)
    beta <- runif(ncol(X),0.2,0.5)
    eta <- runif(30,0.2,0.5)
    Y <- X%*%beta + C[,1:30]%*%eta + rnorm(n)
    return(list(Y=Y, Z=X, C=C, beta=beta, eta=eta, opts=list(n=n,p=p, formulation="y=Z*beta+C[,1:30]*eta+C[,31:p]*0 + epsilon")))

  }else if(model==2){

    if(missing(m.main)) m.main <- 2
    if(missing(p)) p <- 100
    if(p<35) p <- 35

    C <- matrix(rnorm(n*p),n,p)

    hj_1_15 <- sample(1:m.main,15,TRUE)
    hj_16_25 <- sample(1:round(m.main/2),10,TRUE)
    qj <- sample((round(m.main/2)+1):m.main,10,TRUE)
    X <- matrix(rnorm(n*m.main),n,m.main)
    for(m in 1:m.main) for(j in 1:15) X[,m] <- X[,m] + C[,j]*(hj_1_15[j]==m)
    for(m in 1:m.main) for(j in 16:25) X[,m] <- X[,m] + C[,j]*(hj_16_25[j-15]==m)*X[,qj[j-15]]
    X <- scale(X)
    for(m in 2:m.main) for(k in 1:(m-1)) X <- cbind(X, X[,m]*X[,k])


    beta <- runif(ncol(X),0.2,0.5)
    eta <- runif(30,0.2,0.5)
    Y <- X%*%beta + C[,1:30]%*%eta + rnorm(n)
    return(list(Y=Y, Z=X, C=C, beta=beta, eta=eta, opts=list(n=n,p=p,m.main=m.main, formulation="y=Z*beta+C[,1:30]*eta+C[,31:p]*0 + epsilon")))
  }
}
