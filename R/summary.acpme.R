



#' Default summary for bldim
#'
#' @param object An object of class 'bdlim'.
#' @param inter.model Model to be summarized.  The default is \code{inter.model}=1 indicating to summarize the best fitting model.  \code{inter.model}=2, 3, or 4 indicates to summarized the second, third, or fourth best fitting model respectively. Model fit is determined by posterior probability. Alternative, 'BDLIM_n', 'BDLIM_bw', 'BDLIM_b', or 'BDLIM_w' can be entered to return a specific model.
#' @param ciprob The probability contained by the posterior intervals.
#' @param hpd.interval Logical indicating if highest posterior density intervals should be computed (TRUE) or symmetric intervals (FALSE, default)
#' @param ... additional arguments
#'
#' @return A object of class 'summary.bdlim'.
#' @export
#'
#'
summary.acpme <- function(object, ciprob=.95, hpd.interval=FALSE, estimate, ...){

  if(!missing(estimate)){
    betaest <- object$beta %*% estimate
  }else{
    betaest <- object$beta
  }
  
  
  estimate <- data.frame(mean=colMeans(betaest))
  
  
  if(hpd.interval){
    temp <- apply(as.matrix(betaest),2,hpd,ciprob)
    estimate$lower=temp["lower",]
    estimate$upper=temp["upper",]
  }else{
    estimate$lower=apply(as.matrix(betaest),2,quantile,(1-ciprob)/2)
    estimate$upper=apply(as.matrix(betaest),2,quantile,1-(1-ciprob)/2)
  }
  
  
  confounders <- data.frame(posterior=colMeans(object$alpha), prior=1/(1+exp(-object$omega*object$pen.lambda)))

  out <- list(estimate=estimate,
              confounders=confounders,
              lambda=object$pen.lambda,
              call=object$call)

  class(out) <- "summary.acpme"
  return(out)

}


#' Default print for bdlim.summary object
#' @param x bdlim.summary object to print
#' @param ... additional arguments
#' @export
#'
print.summary.acpme <- function(x, ...) {

  cat("Call:\n")
  print(x$call)
  cat("\nEstimates:\n")
  print(round(x$estimate,3))
  cat("\n\nCovariate inclusion probabilities:\n")
  print(round(x$confounders,3))
  
}
