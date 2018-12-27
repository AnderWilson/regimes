


#' Default summary for bkmrdlm
#'
#' @param object An object of class 'bkmrdlm'.
#' @param alpha The alpha level of significance to be used.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @return A object of class 'summary.bkmrdlm'
#' @importFrom stats quantile
#' @export
#'

summary.bkmrdlm <- function(object, alpha=0.05, ...){



    windows <- data.frame(windows=rep(NA,length(object$theta)),row.names=paste0("X",1:length(object$theta)))
    weights <- NULL
    for(m in 1:length(object$theta)){
      w <- object$basis[[m]]$psi%*%t(object$theta[[m]])
      df <- data.frame(m=m, t=1:nrow(w),mean=rowMeans(w), lower=apply(w,1,quantile,0.025), upper=apply(w,1,quantile,0.975))
      df$mean <- df$mean/sqrt(mean(df$mean^2))
      weights <- rbind(weights,df)
      sig <- which((df$lower>0)|(df$upper<0))
      if(length(sig)==0){
        windows[m,"windows"] <- "none"
      }else if(length(sig)==1){
        windows[m,"windows"] <- sig
      }else{

        windowstemp <- sig[1]
        if(length(sig)>2){
          for(t in 2:(length(sig)-1)){
            if(sig[t-1]!=(sig[t]-1)){
              windowstemp <- c(windowstemp,Inf,sig[t])
            }else{
              if(sig[t]!=(sig[t+1]-1)){
                windowstemp <- c(windowstemp,-Inf,sig[t])
            }
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
        windows[m,"windows"] <-  paste(windowstemp, collapse="")
      }
    }

    if(!is.null(names(object$theta))) rownames(windows) <- names(object$theta)
  
  colnames(windows) <- NULL

  out <- list(call=object$call,
              windows=windows,
              weights=weights,
              sdres = mean(sqrt(object$sigma2)))

  out$coefficients <- data.frame(PostMean=colMeans(object$beta), PostSD=apply(object$beta,2,sd), lower=apply(object$beta,2,quantile,alpha/2), upper=apply(object$beta,2,quantile,1-alpha/2), pr=colMeans((object$beta>0)) )
  colnames(out$coefficients) <- c("Post. Mean", "Post. SD", paste0("q",100*alpha/2), paste0("q",100-100*alpha/2), "Pr>0")

  if(!is.null(colnames(object$beta))){
    row.names(out$coefficients) <- colnames(object$beta)
  }else{
    row.names(out$coefficients) <- paste0("Z",1:nrow(out$coefficients))
  }

  class(out) <- "summary.bkmrdlm"
  return(out)
}






#' Default print for summary.bkmrdlm object
#' @param x summary.bkmrdlm object to print
#' @param ... additional arguments
#' @export
#'
print.summary.bkmrdlm <- function(x, ...) {

  cat("\nCall:\n\n")
  print(x$call)


  cat("\n\nSignificant windows identified:\n")
  print(x$windows)

  cat("\n\nCoefficients for covariates:\n\n")
  print(x$coefficients)

  cat("\n\nPosterior mean residuals standard deviation:",x$sdres,"\n")
}
