

#' Draw posterior samples for acpme
#'
#' @param number number of samples
#' @param y y
#' @param X X or Z
#' @param C C
#' @param int Logical indicating there is an intercept 
#' @param scale scale for X
#' @param phi phi
#' @param nu nu
#' @param lambda lambda
#'
#' @return A posterior sample of regression coeficiences for X
#'
#'
drawpost <- function(number,y,X,C,int, scale,phi,nu,lambda){
  
  m <- ncol(X)
  if(int){
    W <- cbind(X,1,C)
  }else{
    W <- cbind(X,C)
  }

  df <- nrow(W)-ncol(W)+nu
  phiIWW <- chol2inv(chol(diag(ncol(W))/phi^2 + t(W)%*%W))

  Wy <- t(W)%*%y
  E <- as.numeric(phiIWW%*%Wy)
  sig2hat <- as.numeric((nrow(W)-1 - t(Wy) %*% E - lambda*nu)/df)
  Vchol <- chol((phiIWW[1:m,1:m] * sig2hat))
  beta <- NULL
  for(j in 1:number) beta <- rbind(beta, (E[1:m] + (rnorm(m)%*%Vchol)/sqrt(rchisq(m,df)/df)) * scale)

  return(beta)
}







