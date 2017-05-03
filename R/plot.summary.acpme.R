
#' Plot for acpme
#'
#' @param x An object of class 'summary.acpme'.
#' @param print A logical.  If TRUE then each plot will be printed. If FALSE then a list of plots will be returned.
#' @param ... additional arguments for ggplot theme.
#'
#' @return If print=FALSE then a list of plots is returned.
#' @import ggplot2
#' @importFrom grDevices devAskNewPage
#' @export
#'
plot.summary.acpme <- function(x,print=TRUE, ...){

theme_regimes <- function (base_size = 20, base_family = "", ...){
   theme_grey (base_size = base_size) + 
              theme (axis.title = element_text(size = base_size),
              axis.text = element_text(size = base_size),
              panel.background = element_rect(fill=NA, colour=NA),
              panel.grid = element_blank(),
              panel.border = element_blank()
              )
}


if(print){
  oask <- devAskNewPage(TRUE)
  on.exit(devAskNewPage(oask))
}else{
  out <- list()
}



x$estimate$Group <- row.names(x$estimate)
p.beta <- ggplot(x$estimate, aes_string(x="Group", y="mean", ymin="lower", ymax="upper")) + geom_point() + geom_errorbar(width=.1)
p.beta <- p.beta + theme_regimes()
p.beta <- p.beta+ylab("Estimated Exposure Effect") + xlab("") + ggtitle("Estimated Exposure Effect")
if(print){
  print(p.beta)
}else{
  out$estimate <- p.beta
}

x$confounders$Covariate <- row.names(x$confounders)
x$confounders$num <- 1:nrow(x$confounders)
temp1 <- x$confounders[,c("Covariate","posterior","num")]
temp2 <- x$confounders[,c("Covariate","prior","num")]
temp1$Type <- "Posterior"
temp2$Type <- "Prior"
colnames(temp1)[2] <- colnames(temp2)[2] <- "Probability"
p.covar <- ggplot(rbind(temp1,temp2), aes_string(x="reorder(Covariate,num)", y="Probability", color="Type")) + geom_point() 
p.covar <- p.covar + theme_regimes()
p.covar <- p.covar + theme(axis.text.x = element_text(angle=90, hjust=1), axis.title.x=element_blank())
p.covar <- p.covar + scale_color_manual(values=c("black","gray"))

if(print){
  print(p.covar)
}else{
  out$confounders <- p.covar
}

if(!print){
  return(out)
}
}




#' Plot for acpme
#'
#' @param x An object of class 'acpme'.
#' @param ... additional arguments for ggplot theme.
#'
#' @return message that there is no plot for an object of class acpme.
#' @export
#'
plot.acpme <- function(x, ...){
  message("There is no plot function for an object of class acpme. Please use plot on an object of class summary.appme.")
}
  
