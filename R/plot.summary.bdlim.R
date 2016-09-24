
#' Plot for bdlim summary
#'
#' @param x An object of class 'summary.bdlim'.
#' @param print A logical.  If TRUE then each plot will be printed. If FALSE then a list of plots will be returned.
#' @param blackwhite A logical.  If TRUE the credible regrion ribbons are grey. If FALSE (default) then they are different colors for each group.
#' @param ... additional arguments for ggplot theme.
#'
#' @return If print=FALSE then a list of plots is returned.
#' @import ggplot2
#' @export
#'
#'
plot.summary.bdlim <- function(x,print=TRUE, blackwhite=FALSE,...){

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



x$beta$Group <- row.names(x$beta)
p.beta <- ggplot(x$beta, aes_string(x="Group", y="mean", ymin="lower", ymax="upper")) + geom_point() + geom_errorbar(width=.1)
p.beta <- p.beta + theme_regimes()
p.beta <- p.beta+ylab("Mean effect size, \u03B2") + xlab("") + ggtitle("Estimated Mean Effect Size, \u03B2")
if(print){
  print(p.beta)
}else{
  out$beta <- p.beta
}

x$cumulative$Group <- row.names(x$cumulative)
p.cumulative <- ggplot(x$cumulative, aes_string(x="Group", y="mean", ymin="lower", ymax="upper")) + geom_point() + geom_errorbar(width=.1)
p.cumulative <- p.cumulative + theme_regimes()
p.cumulative <- p.cumulative+ylab("Cumulative effect") + xlab("")+ ggtitle("Estimated Cumulative Effect")
if(print){
  print(p.cumulative)
}else{
  out$cumulative <- p.cumulative
}

p.bw <- ggplot(x$bw, aes_string(x="t",y="mean",ymin="lower",ymax="upper")) 
if(blackwhite){
  p.w <- p.bw + geom_ribbon(fill="lightgrey", color=NA, alpha=.6)
}else{
  if(any(colnames(x$w)=="G")){
    p.bw <- p.bw + geom_ribbon(aes_string(fill="G"), color=NA, alpha=.6)+ scale_fill_brewer(palette = "Set1")
  }else{
    p.bw <- p.bw + geom_ribbon(fill="blue", color=NA, alpha=.6)
  }
}
p.bw <- p.bw + geom_line(size=1)
p.bw <- p.bw + theme_regimes()
p.bw <- p.bw + ylab("Estimated effect, \u03B2 w(t)") + xlab("time, t") + ggtitle("Estimated Time-Varying Exposure, \u03B2 w(t)")
if(any(colnames(x$bw)=="G")) p.bw <- p.bw + facet_wrap(~G)
if(print){
  print(p.bw)
}else{
  out$bw <- p.bw
}

p.w <- ggplot(x$w, aes_string(x="t",y="mean",ymin="lower",ymax="upper")) 
if(blackwhite){
  p.w <- p.w + geom_ribbon(fill="lightgrey", color=NA, alpha=.6)
}else{
  if(any(colnames(x$w)=="G")){
    p.w <- p.w + geom_ribbon(aes_string(fill="G"), color=NA, alpha=.6)+ scale_fill_brewer(palette = "Set1")
  }else{
    p.w <- p.w + geom_ribbon(fill="blue", color=NA, alpha=.6)
  }
}
p.w <- p.w + geom_line(size=1)
p.w <- p.w + theme_regimes()
p.w <- p.w + ylab("Estimated weight function, w(t)") + xlab("time, t")+ ggtitle("Estimated Weight Function, w(t)")
if(any(colnames(x$w)=="G")) p.w <- p.w + facet_wrap(~G)
if(print){
  print(p.w)
}else{
  out$w <- p.w
}

if(!print){
  return(out)
}
}
