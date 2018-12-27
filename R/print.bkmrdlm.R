





#' Default print for summary.bkmrdlm object
#' @param x summary.bkmrdlm object to print
#' @param ... additional arguments
#' @export
print.bkmrdlm <- function(x,...){

  cat("\nCall:\n\n")
  print(x$call)

  cat("\nThe following methods are available for bkmrdlm: summary, plot, predict\n")

}
