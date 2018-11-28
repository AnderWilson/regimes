#' Plot functions for BKMR-DLM
#'
#' @param x An object of class 'bkmrdlm'.
#' @param print A logical.  If TRUE then each plot will be printed. If FALSE then a list of plots will be returned.
#' @param ... Additional arguments for ggplot theme.
#'
#' @return If print=FALSE then an object of class ggplot is returned.
#' @import ggplot2 stats
#' @export
#'

plot.bkmrdlm <- function(x, print=TRUE, ...){

  theme_regimes <- function (base_size = 20, base_family = "", ...){
    theme_grey (base_size = base_size) +
      theme (axis.title = element_text(size = base_size),
             axis.text = element_text(size = base_size),
             panel.background = element_rect(fill=NA, colour=NA),
             panel.grid = element_blank(),
             panel.border = element_blank()
      )
  }


  
  df <- NULL

  if(!is.null(names(x$theta))){
    Mvec <- names(x$theta)
  }else{
    Mvec <- 1:length(x$theta)
  }
    
  for(m in Mvec){
    w <- x$basis[[m]]$psi%*%t(x$theta[[m]])
    df <- rbind(df,data.frame(M=m,t=1:nrow(w),mean=rowMeans(w), lower=apply(w,1,quantile,0.025), upper=apply(w,1,quantile,0.975)) )
  }

  p <- ggplot(df, aes_string(x="t",y="mean",ymin="lower", ymax="upper"))
  p <- p + geom_hline(yintercept=0, size=1)
  p <- p + geom_ribbon(fill="grey60", alpha=.5) + geom_line(size=2)
  p <- p + theme_regimes()
  p <- p + ylab("weight function, w(t)")
  p <- p + facet_wrap(~M)




  if(print){
    print(p)
  }else if(!print){
    return(p)
  }
}
