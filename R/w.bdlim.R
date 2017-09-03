w <- function(x, inter.model, alphalevel, hpd.interval) UseMethod("w")

#' Summary of the weight function for BDLIM
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

w.bdlim <- function(fit, inter.model, alphalevel=0.05, hpd.interval=FALSE){

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

  out <- NULL
  if(m%in%c("BDLIM_bw","BDLIM_w")){
    for(g in names(fit[[m]]$theta)){
      what <- fit$B$psi%*%t(fit[[m]]$theta[[g]])

      if(hpd.interval){
        temp <- apply(what,1,hpd,alphalevel)
        lower=temp["lower",]
        upper=temp["upper",]
      }else{
        lower=apply(what,1,quantile,alphalevel/2)
        upper=apply(what,1,quantile,1-alphalevel/2)
      }

      out <- rbind(out,
                 data.frame(G=g,
                            t=1:nrow(what),
                            mean=rowMeans(what)/sqrt(mean(rowMeans(what)^2)),
                            lower=lower,
                            upper=upper,
                            pr_gr0 = rowMeans(what>0),
                            n_eff=effectiveSize(t(what))
                 ))

    }

      }else{
    what <- fit$B$psi%*%t(fit[[m]]$theta)

    if(hpd.interval){
      temp <- apply(what,1,hpd,alphalevel)
      lower=temp["lower",]
      upper=temp["upper",]
    }else{
      lower=apply(what,1,quantile,alphalevel/2)
      upper=apply(what,1,quantile,1-alphalevel/2)
    }

    out <- data.frame(t=1:nrow(what),
                    mean=rowMeans(what)/sqrt(mean(rowMeans(what)^2)),
                    lower=lower,
                    upper=upper,
                    pr_gr0 = rowMeans(what>0),
                    n_eff=effectiveSize(t(what))
    )

  }

  row.names(out) <- NULL
  return(out)
}
