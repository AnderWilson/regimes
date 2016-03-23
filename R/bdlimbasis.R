

#-------------------------------------------------------------------------------------------------
#' Build BDLIM basis
#'
#' \code{bdlimbasis} builds the basis for thge BDLIM model using either FACE of natural cubic splines.
#' @param X A n x p matrix of observations. Each row has the observations for one individual. The observations should be evenly spaced over a grid.
#' @param knots The number of knots to use or a vector of knots.
#' @param df Degrees of freedom for the natrual spline basis.
#' @param pve Proportion of variance explained by the PC basis using FACE. Must be in (0,1].
#' @param type The type of basis expansion to use.  Default is 'face' corresponding the the FACE method. Alternative method is 'ns' for a natrual cubic spline basis.
#' @return A list with elements:
#' \item{psi}{Matrix with eigenfunctions in the columns.}
#' \item{lambda}{Vector of eigen values.}
#' \item{knots}{Knots used (if applicable).}
#' \item{df}{Degrees of freedom used (if applicable).}
#' \item{type}{The type of basis constructed.}
#' @seealso \code{\link{fpca.face}} and \code{\link{ns}} for details on the methods called.
#' @author Ander Wilson
#' @importFrom splines ns
#' @importFrom refund fpca.face


bdlimbasis <- function(X,knots=NULL, df=NULL,pve=.99, type="face"){


  #account for missing input in basis function
  if(is.null(df) & is.null(knots) & toupper(type)=="NS") df <- round(ncol(X)/5)
  if(is.null(knots) & toupper(type)=="FACE") knots <- round(ncol(X)/3)

  #correct pve if needed.
  if(!is.numeric(pve)){
    pve=.99
  }else{
    if(pve<0 | pve>1) pve <- .99
  }

  if(toupper(type)%in%c("FACE","F")){
    fitted <- fpca.face(X, knots=knots,pve=pve)
    lambda <- fitted$evalues
    return(list(psi=fitted$efunctions, lambda=lambda, knots=knots,pve=pve,type=type))
  }else if(toupper(type)%in%c("NS","N")){
    B1 <- ns(seq(1,ncol(X)),df=df, intercept=TRUE)
    fitted <- B1 %*% qr.solve(B1,t(X))
    svd <- svd(fitted)
    return(list(psi=svd$u[,1:ncol(B1)], lambda=svd$d[1:ncol(B1)], df=df,type=type))
  }
}
