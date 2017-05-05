



#' Default summary for bldim
#'
#' @param object An object of class 'bdlim'.
#' @param inter.model Model to be summarized.  The default is \code{inter.model}=1 indicating to summarize the best fitting model.  \code{inter.model}=2, 3, or 4 indicates to summarized the second, third, or fourth best fitting model respectively. Model fit is determined by posterior probability. Alternative, 'BDLIM_n', 'BDLIM_bw', 'BDLIM_b', or 'BDLIM_w' can be entered to return a specific model.
#' @param alphalevel The alpha level for the posterior intervals.
#' @param hpd.interval Logical indicating if highest posterior density intervals should be computed (TRUE) or symmetric intervals (FALSE, default)
#' @param ... additional arguments
#'
#' @return A object of class 'summary.bdlim'.
#' @export
#'
#'
summary.bdlim <- function(object, inter.model, alphalevel=.05, hpd.interval=FALSE, ...){

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
  
  ## summarize posterior
  out <- list(beta=beta(object,inter.model=inter.model, alphalevel=alphalevel, hpd.interval=hpd.interval),
       cumulative=cumulative(object,inter.model=inter.model, alphalevel=alphalevel, hpd.interval=hpd.interval),
       w=w(object,inter.model=inter.model, alphalevel=alphalevel, hpd.interval=hpd.interval),
       bw=bw(object,inter.model=inter.model, alphalevel=alphalevel, hpd.interval=hpd.interval),
       model=m,
       modelfit=object$modelfit,
       call=object$call)


  ## identify critical windows
  windows <- data.frame(windows=rep(NA,length(unique(out$bw$G))),row.names=unique(out$bw$G))
    for(g in 1:length(unique(out$bw$G))){
      sig <- out$bw$t[as.character(out$bw$G)==row.names(windows)[g]][ which((out$bw$lower[as.character(out$bw$G)==row.names(windows)[g]] > 0)|(out$bw$upper[as.character(out$bw$G)==row.names(windows)[g]] < 0)) ]
      if(length(sig)==0){
        windows[g,"windows"] <- "none"
      }else if(length(sig)==1){
        windows[g,"windows"] <- sig
      }else{

        windowstemp <- sig[1]
        for(t in 2:(length(sig[])-1)){
          if(sig[t-1]!=(sig[t]-1)){
            windowstemp <- c(windowstemp,Inf,sig[t])
          }else{
            if(sig[t]!=(sig[t+1]-1)){
              windowstemp <- c(windowstemp,-Inf,sig[t])
            }
          }
        }
        if(sig[length(sig)-1]!=(sig[length(sig)]-1)){
          windowstemp <- c(windowstemp,Inf,sig[length(sig)])
        }else{
          windowstemp <- c(windowstemp,-Inf,sig[length(sig)])
        }
        windowstemp <- as.character(windowstemp)
        if(length(which(windowstemp=="-Inf"))>0) windowstemp[which(windowstemp=="-Inf")] <- "-"
        if(length(which(windowstemp=="Inf"))>0) windowstemp[which(windowstemp=="Inf")] <- ", "
        windows[g,"windows"] <-  paste(windowstemp, collapse="")
      }
    }

  colnames(windows) <- NULL
  out$windows <- windows
  
  
  # summaize other regression coeficients
  out$coefficients <- data.frame(PostMean=colMeans(object[[m]]$gamma), 
                                 PostSD=apply(object[[m]]$gamma,2,sd), 
                                 lower=NA, 
                                 upper=NA, 
                                 pr=colMeans((object[[m]]$gamma>0)) )
  
  if(hpd.interval){
    int <- apply(object[[m]]$gamma,2,hpd,alphalevel)
    out$coefficients$upper <- int["upper",]
    out$coefficients$lower <- int["lower",]
  }else{
    out$coefficients$upper <- apply(object[[m]]$gamma,2,quantile,1-alphalevel/2)
    out$coefficients$lower <- apply(object[[m]]$gamma,2,quantile,alphalevel/2)
  }
  colnames(out$coefficients) <- c("Post. Mean", "Post. SD", paste0("q",100*alphalevel/2), paste0("q",100-100*alphalevel/2), "Pr>0")
  colnames(out$w)[which(colnames(out$w)=="mean")] <- "Post. Mean"
  colnames(out$w)[which(colnames(out$w)=="lower")] <- paste0("q",100*alphalevel/2)
  colnames(out$w)[which(colnames(out$w)=="upper")] <- paste0("q",100-100*alphalevel/2)
  colnames(out$bw)[which(colnames(out$bw)=="mean")] <- "Post. Mean"
  colnames(out$bw)[which(colnames(out$bw)=="lower")] <- paste0("q",100*alphalevel/2)
  colnames(out$bw)[which(colnames(out$bw)=="upper")] <- paste0("q",100-100*alphalevel/2)
  
  
  
  class(out) <- "summary.bdlim"
  return(out)

}


#' Default print for summary.bdlim object
#' @param x summary.bdlim object to print
#' @param ... additional arguments
#' @export
#'
print.summary.bdlim <- function(x, ...) {

  cat("\nModel fit statistics:\n")
  print(round(x$modelfit,3))
  cat(paste0("\n\nResults for ",x$model,":\n"))
  cat("\nBeta:\n")
  print(x$beta,...)
  cat("\nCumulative:\n")
  print(x$cumulative,...)
  cat("\nCritical windows identified weigh the weighted exposures, beta*w(t):\n")
  print(x$windows,...)
  cat("\n\nCoefficients for covariates:\n\n")
  print(x$coefficients)
}




