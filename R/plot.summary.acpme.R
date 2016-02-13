
#' Plot for acpme
#'
#' @param x An object of class 'summary.acpme'.
#' @param print A logical.  If TRUE then each plot will be printed. If FALSE then a list of plots will be returned.
#'
#' @return If print=FALSE then a list of plots is returned.
#' @import ggplot2
#' @export
#'
#'
plot.summary.acpme <- function(x,print=TRUE){

theme_regimes <- function (base_size = 20){
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
p.beta <- ggplot(x$estimate, aes(x=Group, y=mean, ymin=lower,ymax=upper)) + geom_point() + geom_errorbar(width=.1)
p.beta <- p.beta + theme_regimes()
p.beta <- p.beta + ylab("Estimated Exposure Effect") + xlab("") + ggtitle("Estimated Exposure Effect")
p.beta
if(print){
  print(p.beta)
}else{
  out$estimates <- p.beta
}

x$confounders$Covariate <- paste0("C",row.names(x$confounders))
temp1 <- x$confounders[,c("prior","Covariate")]
temp2 <- x$confounders[,c("posterior","Covariate")]
temp1$Type <- "Prior"
temp2$Type <- "Posterior"
colnames(temp1)[1] <- colnames(temp2)[1] <- "Probability"
temp <- rbind(temp1,temp2)


p.prob <- ggplot(temp, aes(x=Covariate, y=Probability, color=Type)) + geom_point()
p.prob <- p.prob + theme_regimes()
p.prob <- p.prob + scale_color_manual(values=c("black","gray"))
p.prob <- p.prob + theme(axis.text.x = element_text(angle = 90, hjust = 1))

if(print){
  print(p.prob)
}else{
  out$prob <- p.prob
}

if(!print){
  return(out)
}
}