



#' Default summary for acpme
#'
#' @param object An object of class 'acpme'.
#' @param ciprob The probability contained by the posterior intervals.
#' @param hpd.interval Logical indicating if highest posterior density intervals should be computed (TRUE) or symmetric intervals (FALSE, default)
#' @param estimate A covtor or matrix defining a linear combination of the regression coeficients for the exposure.
#' @param ... additional arguments
#'
#' @return A object of class 'summary.acpme'.
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


#' Default print for acpme.summary object
#' @param x acpme.summary object to print
#' @param minpr The confounder inclusion probabilities for covariates with posterior inclusion probability at least minpr will be printed.
#' @param ... additional arguments
#' @export
#'
print.summary.acpme <- function(x, minpr=.5, ...) {

  cat("Call:\n")
  print(x$call)
  cat("\nEstimates:\n")
  print(round(x$estimate,3))
  cat("\n\nCovariate inclusion probabilities:\n")
  print(round(subset(x$confounders, posterior>=minpr),3))

}




#' Default print for acpme object
#' @param x object of class acpme.
#' @export
#'
print.acpme <- function(x) {
  
  cat("Call:\n")
  print(x$call)
  cat("\nEstimates:\n")
  print(round(colMeans(object$beta),3))

}
