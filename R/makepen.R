


#' Make penalty
#'
#' This function computes the penalty weights (omega) for each covaraite.
#'
#' @param Z Matrix of exposures including interactions.
#' @param C A n x p matrix of covaraites.
#' @param pen.type Choice of penalty. The default is "eigen." Other options are "correlation" and "projection."
#' @export
#' @examples
#' dat <- simregimes(scenario="acpme1", seed=1234)
#' pen <- makepen(Z=scale(dat$Z),C=scale(dat$C))
#' par(mfrow=c(2,1))
#' plot(pen$omega)
#' plot(exp(pen$omega*pen$lambda)/(1+exp(pen$omega*pen$lambda)), ylab="expit(pen$omega*pen$lambda)")

makepen <- function(Z,C,pen.type="eigen"){

  if(missing(Z) | missing(C)){
    message("Error: Z and C must be provided in Make.Pen. ")
    return()
  }
  if(missing(pen.type)){
    message("Invalid penalty type specificed.  Will default of eigen weights. ")
    pen.type <- "eigen"
  }else if(pen.type!="eigen" & pen.type!="correlation" & pen.type!="projection"){
    message("Invalid penalty type specificed.  Will default of eigen weights. ")
    pen.type <- "eigen"
  }


  Z <- scale(Z)
  C <- scale(C)
  n <- nrow(C)

  out<-list()

  if(pen.type=="correlation"){
    XC <- t(Z)%*%C
    out$omega <- as.numeric(drop( diag(t(XC)%*%XC) / (n-1)^2 ))
  }else if(pen.type=="projection"){
    XC <- t(Z)%*%C
    out$omega <- as.numeric(drop( diag(t(XC) %*% chol2inv(chol(t(scale(Z))%*%scale(Z))) %*% XC) / (n-1) ))
  }else if(pen.type=="eigen"){
    XX <- t(Z)%*%Z
    XC <- t(Z)%*%C
    CPxC <- t(XC)%*%chol2inv(chol(XX))%*%XC
    eig <- eigen( CPxC)
    out$omega <- as.numeric(drop( eig$values[1:min(ncol(Z),ncol(C))] %*% (t(eig$vectors[,1:min(ncol(Z),ncol(C))]) %*% chol2inv(chol(t(C)%*%C-CPxC)))^2 ))
  }
  out$lambda <- length( out$omega )*log( n )/( 2 * sum( out$omega ) )
  return(out)
}







