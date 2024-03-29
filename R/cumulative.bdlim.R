cumulative <- function(fit, inter.model, alphalevel, hpd.interval) UseMethod("cumulative")

#' Summary of the cumulative effect for BDLIM
#'
#' @param fit An object of class 'bdlim'.
#' @param inter.model Model to be summarized.  The default is \code{inter.model}=1 indicating to summarize the best fitting model.  \code{inter.model}=2, 3, or 4 indicates to summarized the second, third, or fourth best fitting model respectively. Model fit is determined by posterior probability. Alternative, 'BDLIM_n', 'BDLIM_bw', 'BDLIM_b', or 'BDLIM_w' can be entered to return a specific model.
#' @param alphalevel The alpha level for the posterior intervals.
#' @param hpd.interval Logical indicating if highest posterior density intervals should be computed (TRUE) or symmetric intervals (FALSE, default)
#'
#' @return Data.frame summarizing the posterior distribution.
#' @importFrom coda effectiveSize
#' @export
#'
#'
#'
#'
cumulative.bdlim <- function(fit, inter.model, alphalevel=0.05, hpd.interval=FALSE){

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

  temp <- NULL
  if(m=="BDLIM_w"){
    for(g in names(fit[[m]]$theta)){
      what <- fit$B$psi%*%t(fit[[m]]$theta[[g]])
      bwhat <- scale(what, center=FALSE, scale=1/fit[[m]]$beta)
      temp <- cbind(temp,colSums(bwhat))
    }
  }else if(m=="BDLIM_bw"){
    for(g in names(fit[[m]]$theta)){
      what <- fit$B$psi%*%t(fit[[m]]$theta[[g]])
      bwhat <- scale(what, center=FALSE, scale=1/fit[[m]]$beta[,g])
      temp <- cbind(temp,colSums(bwhat))
    }
  }else if(m=="BDLIM_b"){
    what <- fit$B$psi%*%t(fit[[m]]$theta)
    for(g in colnames(fit[[m]]$beta)){
      bwhat <- scale(what, center=FALSE, scale=1/fit[[m]]$beta[,g])
      temp <- cbind(temp,colSums(bwhat))
    }
  }else if(m=="BDLIM_n"){
    what <- fit$B$psi%*%t(fit[[m]]$theta)
    bwhat <- scale(what, center=FALSE, scale=1/fit[[m]]$beta)
    temp <- cbind(temp,colSums(bwhat))
  }


  if(hpd.interval){
    temp <- apply(as.matrix(temp),2,hpd,1-alphalevel)
    lower=temp["lower",]
    upper=temp["upper",]
  }else{
    lower=apply(as.matrix(temp),2,quantile,alphalevel/2)
    upper=apply(as.matrix(temp),2,quantile,1-alphalevel/2)
  }

  out <- data.frame(mean=colMeans(as.matrix(temp)),
                    sd=apply(as.matrix(temp),2,sd),
                    lower=lower,
                    upper=upper,
                    pr <- colMeans(as.matrix(temp)>0),
                    n_eff=effectiveSize(temp)
  )
  colnames(out) <- c("mean", "sd", paste0("q",100*alphalevel/2), paste0("q",100-100*alphalevel/2), "Pr>0","n_eff")

  if(nrow(out)==1){
    row.names(out) <- "cumulative"
  }else if(m%in%c("BDLIM_bw","BDLIM_b")){
    row.names(out) <- colnames(fit[[m]]$beta)
  }else if(m%in%c("BDLIM_w")){
    row.names(out) <- names(fit[[m]]$theta)
  }


    return(out)
}






