


#' Make penalty
#'
#' This function computes the penalty weights (omega) for each covaraite.
#'
#' @param Z Matrix of exposures including interactions.
#' @param C A n x p matrix of covaraites.
#' @param pen.type Choice of penalty. The default is "eigen." Other options are "correlation" and "projection."
#' @export
#' @examples
#' dat_acpme1 <- simregimes(scenario="acpme1", seed=1234, n=200, p=100)
#' omega <- makepen(Z=scale(dat_acpme1$Z),C=scale(dat_acpme1$C))
#' par(mfrow=c(2,1))
#' plot(omega$omega)
#' pr <- exp(omega$omega*omega$lambda)/(1+exp(omega$omega*omega$lambda))
#' plot(pr)

makepen <- function(Z,C,pen.type="eigen"){

  if(missing(Z) | missing(C)){
    message("Error: Z and C must be provided in makepen ")
    return()
  }
 if(pen.type!="eigen" & pen.type!="correlation" & pen.type!="projection"){
    message("Invalid penalty type specificed.  Will default of eigen weights. ")
    pen.type <- "eigen"
  }


  X <- scale(Z)
  C <- scale(C)
  n <- nrow(C)

  out<-list()

  if(pen.type=="correlation"){
    XC <- t(X)%*%C
    out$omega <- as.numeric(drop( diag(t(XC)%*%XC) / (n-1)^2 ))
  }else if(pen.type=="projection"){
    XC <- t(X)%*%C
    out$omega <- as.numeric(drop( diag(t(XC) %*% chol2inv(chol(t(scale(X))%*%scale(X))) %*% XC) / (n-1) ))
  }else if(pen.type=="eigen"){
    XX <- t(X)%*%X
    XC <- t(X)%*%C
    CPxC <- t(XC)%*%chol2inv(chol(XX))%*%XC
    eig <- eigen( CPxC)
    out$omega <- as.numeric(drop( eig$values[1:min(ncol(X),ncol(C))] %*% (t(eig$vectors[,1:min(ncol(X),ncol(C))]) %*% chol2inv(chol(t(C)%*%C-CPxC)))^2 ))
  }
  out$lambda <- length( out$omega )*log( n )/( 2 * sum( out$omega ) )
  return(out)
}







