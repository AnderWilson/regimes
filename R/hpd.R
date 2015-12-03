


#' Highest posterior density posterior intervals of a distribution
#'
#' @param x Posterior sample
#' @param prob Probability to be contained in the interval.
#'
#' @return lower and upper bounds
#'
#'
hpd <- function(x, prob=0.95){
  x <- x[order(x)]
  size <- round(length(x)*prob)
  width <- NULL
  for(i in 1:(length(x)-size)) width <- c(width, x[i+size]-x[i])
  h <- c(x[which.min(width)],x[which.min(width)+size])
  names(h)<-c("lower","upper")
  return(h)
}
