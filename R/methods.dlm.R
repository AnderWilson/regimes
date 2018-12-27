


#' Default print for dlm object
#' @param x dlm object to print
#' @param ... additional arguments
#' @export
#'
print.dlm <- function(x, ...) {

  cat("Call:\n")
  print(x$call)
  cat("\nModel fit statistics:\n")
  print(round(x$modelfit,3), row.names = FALSE)
  cat(paste0("\n\nWindows:\n"))
  if(any(which(x$dlm$lower>0 | x$dlm$upper<0))){
    print(x$dlm[which(x$dlm$lower>0 | x$dlm$upper<0),], row.names = FALSE)
  }else{
    print("none")
  }
  cat("\nCumulative:\n")
  print(x$cumulative, row.names = FALSE)
  cat("\n\nCoefficients for covariates:\n\n")
  print(summary(x$fit)$coefficients[-grep("dlm",names(x$fit$coefficients)),])
}



#' Plot for dlm
#'
#' @param x An object of class 'dlm'.
#' @param ... additional arguments for plot
#'
#' @export
#'
plot.dlm <- function(x, ...){
 matplot(x$dlm[,c("estimate","lower","upper")], 
         lty=c(1,2,2), las=1, type="l", col="black",
         ylab="Estimate", xlab="time", ...)
}




