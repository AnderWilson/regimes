
#' Plot for bdlim summary
#'
#' @param x An object of class 'bdlim'.
#' @param bs Base font size
#' @param print A logical.  If TRUE then each plot will be printed. If FALSE then a list of plots will be returned.
#' @param ... additional arguments for ggplot theme.
#'
#' @return If print=FALSE then an object of class ggplot is returned.
#' @import ggplot2
#' @export
#'
#'
plot.bdlim <- function(x, bs=14, print=TRUE, ...){

  theme_regimes <- function (base_size = bs, base_family = "", ...){
    theme_grey (base_size = base_size) +
      theme (axis.title = element_text(size = base_size),
             axis.text = element_text(size = base_size),
             panel.background = element_rect(fill=NA, colour=NA),
             panel.grid = element_blank(),
             panel.border = element_blank()
      )
  }

  

  x$modelfit$name <- row.names((x$modelfit))
  p.prob <- ggplot(x$modelfit) + geom_bar(aes_string(x="name", y="modelprob"), stat="identity")
  p.prob <- p.prob + theme_regimes() + xlab("")
  p.prob <- p.prob + ylab("Model Probability") + scale_y_continuous(limits=c(0,1))
  
  if(print){
    print(p.prob)
  }else if(!print){
    return(p.prob)
  }
}



#' Default print for bdlim object
#' @param x bdlim.summary object to print
#' @param ... additional arguments
#' @export
#'
print.bdlim <- function(x, ...) {
  
  cat("Call:\n")
  print(x$call)
  cat("\nModel fit statistics:\n")
  print(round(x$modelfit,3))
  
}