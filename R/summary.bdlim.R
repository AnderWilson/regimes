



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
summary.bdlim <- function(object, inter.model, ciprob=.95, hpd.interval=FALSE, ...){

  if(missing(inter.model)) inter.model <- 1
  m <- NULL
  if(toupper(paste0("BDLIM_",inter.model))%in% toupper(row.names(object$modelfit))){
    m <- row.names(object$modelfit)[which(toupper(row.names(object$modelfit))==toupper(paste0("BDLIM_",inter.model)))]
  }else if(toupper(inter.model)%in% toupper(row.names(object$modelfit))){
    m <- row.names(object$modelfit)[which(toupper(row.names(object$modelfit))==toupper(inter.model))]
  }else if(is.numeric(inter.model[1])){
    if(round(inter.model[1])<= nrow(object$modelfit) & round(inter.model[1])>0) m <- row.names(object$modelfit)[round(inter.model[1])]
  }
  if(is.null(m)){
    inter.model <- 1
    m <- row.names(object$modelfit)[1]
  }
  

  out <- list(beta=beta(object,inter.model=inter.model, ciprob=ciprob, hpd.interval=hpd.interval),
       cumulative=cumulative(object,inter.model=inter.model, ciprob=ciprob, hpd.interval=hpd.interval),
       w=w(object,inter.model=inter.model, ciprob=ciprob, hpd.interval=hpd.interval),
       bw=bw(object,inter.model=inter.model, ciprob=ciprob, hpd.interval=hpd.interval),
       model=m,
       modelfit=object$modelfit,
       call=object$call)

  class(out) <- "summary.bdlim"
  return(out)

}


#' Default print for bdlim.summary object
#' @param x bdlim.summary object to print
#' @param ... additional arguments
#' @export
#'
print.summary.bdlim <- function(x, ...) {

  cat("Call:\n")
  print(x$call)
  cat("\nModel fit statistics:\n")
  print(round(x$modelfit,3))
  cat(paste0("\n\nResults for ",x$model,":\n"))
  cat("\nBeta:\n")
  print(x$beta,...)
  cat("\nCumulative:\n")
  print(x$cumulative,...)

}
