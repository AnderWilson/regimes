beta <- function(x, inter.model, ciprob, hpd.interval) UseMethod("beta")

#' Summary of the beta for BDLIM
#'
#' @param fit An object of class 'bdlim'.
#' @param inter.model Model to be summarized.  The default is \code{inter.model}=1 indicating to summarize the best fitting model.  \code{inter.model}=2, 3, or 4 indicates to summarized the second, third, or fourth best fitting model respectively. Model fit is determined by posterior probability. Alternative, 'BDLIM_n', 'BDLIM_bw', 'BDLIM_b', or 'BDLIM_w' can be entered to return a specific model.
#' @param ciprob The probability contained by the posterior intervals.
#' @param hpd.interval Logical indicating if highest posterior density intervals should be computed (TRUE) or symmetric intervals (FALSE, default)
#'
#' @return Data.frame summarizing the posterior distribution.
#' @export
#'
#'
beta.bdlim <- function(fit, inter.model, ciprob=.95, hpd.interval=FALSE){

  if(missing(inter.model)) inter.model <- 1
  m <- NULL
  if(toupper(paste0("BDLIM_",inter.model))%in% toupper(row.names(fit$modelfit))){
    m <- row.names(fit$modelfit)[which(toupper(row.names(fit$modelfit))==toupper(paste0("BDLIM_",inter.model)))]
  }else if(toupper(inter.model)%in% toupper(row.names(fit$modelfit))){
    m <- row.names(fit$modelfit)[which(toupper(row.names(fit$modelfit))==toupper(inter.model))]
  }else if(is.numeric(inter.model[1])){
    if(round(inter.model[1])<= nrow(fit$modelfit) & round(inter.model[1])>0) m <- row.names(fit$modelfit)[round(inter.model[1])]
  }
  if(is.null(m)){
    inter.model <- 1
    m <- row.names(fit$modelfit)[1]
  }

  if(hpd.interval){
    temp <- apply(as.matrix(fit[[m]]$beta),2,hpd,ciprob)
    lower=temp["lower",]
    upper=temp["upper",]
  }else{
    lower=apply(as.matrix(fit[[m]]$beta),2,quantile,(1-ciprob)/2)
    upper=apply(as.matrix(fit[[m]]$beta),2,quantile,1-(1-ciprob)/2)
  }

  out <- data.frame(mean=colMeans(as.matrix(fit[[m]]$beta)),
                    sd=apply(as.matrix(fit[[m]]$beta),2,sd),
                    lower=lower,
                    upper=upper
  )

  if(nrow(out)==1){
    row.names(out) <- "beta"
  }else{
    row.names(out) <- colnames(fit[[m]]$beta)
  }

  return(out)
}


