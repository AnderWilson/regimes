
#' Plot for bdlim summary
#'
#' @param x An object of class 'summary.bdlim'.
#' @param print A logical.  If TRUE then each plot will be printed. If FALSE then a list of plots will be returned.
#' @param blackwhite A logical.  If TRUE the credible region ribbons are grey. If FALSE (default) then they are different colors for each group.
#' @param grid If missing then each plot will appear separate. Otherwise, numbers between 1 and 5 correspond to different combinations of figures printed together.
#' @param bs Base font size
#' @param continuous_weight Logical indicating if the weight function interval should be represented with a line and ribbon. If FALSE points and error bars will be used instead.
#' @param ... additional arguments for ggplot theme.
#'
#' @return If print=FALSE then a list of plots is returned.
#' @import ggplot2 grid
#' @importFrom grDevices devAskNewPage
#' @importFrom graphics par plot.new
#' @export
#'
#'
plot.summary.bdlim <- function(x,print=TRUE, blackwhite=FALSE, grid, bs, continuous_weight=TRUE, ...){

  
  if(missing(bs)){
    if(missing(grid)){
      bs <- 20
    }else{
      bs <- 10
    }
  }
  if(!missing(grid)){
    if(grid%in%1:5){
      #print must be true if grid
      print <- TRUE
    }else{
      grid <- 2
    }
    out <- list()
  }
  
  theme_regimes <- function (base_size = bs, base_family = "", ...){
    theme_grey (base_size = base_size) +
      theme (axis.title = element_text(size = base_size),
             plot.title = element_text(size = base_size),
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
p.beta <- ggplot(x$beta, aes_string(x="Group", y="mean", ymin=colnames(x$beta)[3], ymax=colnames(x$beta)[4])) + geom_point() + geom_errorbar(width=0)
p.beta <- p.beta + theme_regimes()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
p.beta <- p.beta+ylab("Mean effect size, \u03B2") + xlab("") + ggtitle("Mean Effect Size, \u03B2")
if(print & missing(grid)){
  print(p.beta)
}else{
  out$beta <- p.beta
}

x$cumulative$Group <- row.names(x$cumulative)
p.cumulative <- ggplot(x$cumulative, aes_string(x="Group", y="mean", ymin=colnames(x$cumulative)[3], ymax=colnames(x$cumulative)[4])) + geom_point() + geom_errorbar(width=0)
p.cumulative <- p.cumulative + theme_regimes() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p.cumulative <- p.cumulative+ylab("Cumulative effect") + xlab("")+ ggtitle("Cumulative Effect")
if(print & missing(grid)){
  print(p.cumulative)
}else{
  out$cumulative <- p.cumulative
}


p.bw <- ggplot(x$bw, aes_string(x="t",y="mean", ymin=colnames(x$bw)[which(colnames(x$bw)=="mean")+1], ymax=colnames(x$bw)[which(colnames(x$bw)=="mean")+2])) 
if(blackwhite){
  if(continuous_weight){
    p.bw <- p.bw + geom_ribbon(fill="lightgrey", color=NA, alpha=.6)  
  }else{
    p.bw <- p.bw + geom_errorbar(width=0)  
  }
  
}else{
  if(any(colnames(x$bw)=="G")){
    if(continuous_weight){
      p.bw <- p.bw + geom_ribbon(aes_string(fill="G"), color=NA, alpha=.6) + scale_fill_brewer(palette = "Set1")
    }else{
      p.bw <- p.bw + geom_errorbar(width=0) + scale_fill_brewer(palette = "Set1")
    }
  }else{
    if(continuous_weight){
      p.bw <- p.bw + geom_ribbon(fill="blue", color=NA, alpha=.6)
    }else{
      p.bw <- p.bw + geom_errorbar(width=0)  
    }
  }
}
if(continuous_weight){
  p.bw <- p.bw + geom_line()  
}else{
  p.bw <- p.bw + geom_point()
}

p.bw <- p.bw + theme_regimes()
p.bw <- p.bw + ylab("Estimated effect, \u03B2 w(t)") + xlab("time, t") + ggtitle("Time-Varying Exposure, \u03B2 w(t)")
if(any(colnames(x$bw)=="G")) p.bw <- p.bw + facet_wrap(~G)
if(print & missing(grid)){
  print(p.bw)
}else{
  out$bw <- p.bw
}

p.w <- ggplot(x$w, aes_string(x="t",y="mean", ymin=colnames(x$w)[which(colnames(x$w)=="mean")+1], ymax=colnames(x$w)[which(colnames(x$w)=="mean")+2])) 
if(blackwhite){
  if(continuous_weight){
    p.w <- p.w + geom_ribbon(fill="lightgrey", color=NA, alpha=.6)
  }else{
    p.w <- p.w + geom_errorbar(width=0)
  }
}else{
  if(any(colnames(x$w)=="G")){
    if(continuous_weight){
      p.w <- p.w + geom_ribbon(aes_string(fill="G"), color=NA, alpha=.6) + scale_fill_brewer(palette = "Set1")
    }else{
      p.w <- p.w + geom_errorbar(width=0) + scale_fill_brewer(palette = "Set1")
    }
  }else{
    if(continuous_weight){
      p.w <- p.w + geom_ribbon(fill="blue", color=NA, alpha=.6)
    }else{
      p.w <- p.w + geom_errorbar(width=0)
    }
  }
}
if(continuous_weight){
  p.w <- p.w + geom_line()
}else{
  p.w <- p.w + geom_point()
}
p.w <- p.w + theme_regimes()
p.w <- p.w + ylab("Estimated weight function, w(t)") + xlab("time, t")+ ggtitle("Weight Function, w(t)")
if(any(colnames(x$w)=="G")) p.w <- p.w + facet_wrap(~G)
if(print & missing(grid)){
  print(p.w)
}else{
  out$w <- p.w
}

if(!missing(grid)){
  if(grid==1){  # cumulative and bw
    par(ask=FALSE)
    plot.new()
    pushViewport(viewport(layout = grid.layout(1, 5)))
    define_region <- function(row, col){
      viewport(layout.pos.row = row, layout.pos.col = col)
    } 
    
    print(p.cumulative, vp=define_region(1, 1))
    print(p.bw, vp=define_region(1, 2:5))
    
  }else if(grid==2){  # beta and w
    par(ask=FALSE)
    plot.new()
    pushViewport(viewport(layout = grid.layout(1, 6)))
    define_region <- function(row, col){
      viewport(layout.pos.row = row, layout.pos.col = col)
    } 
    
    print(p.beta, vp=define_region(1, 1:2))
    print(p.w, vp=define_region(1, 3:6))
    
  }else if(grid==3){ #beta, cumulative, and b
    par(ask=FALSE)
    plot.new()
    pushViewport(viewport(layout = grid.layout(1, 6)))
    define_region <- function(row, col){
      viewport(layout.pos.row = row, layout.pos.col = col)
    } 
    
    print(p.beta, vp=define_region(1, 1))
    print(p.cumulative, vp=define_region(1, 2))
    print(p.w, vp = define_region(1, 3:6))
    
  }else if(grid==4){ #beta, cumulative, w, and bw
    par(ask=FALSE)
    plot.new()
    pushViewport(viewport(layout = grid.layout(2, 5)))
    define_region <- function(row, col){
      viewport(layout.pos.row = row, layout.pos.col = col)
    } 
    
    print(p.beta, vp=define_region(1, 1))
    print(p.cumulative, vp=define_region(2, 1))
    print(p.w, vp = define_region(1, 2:5))
    print(p.bw, vp = define_region(2, 2:5))
    
  }else if(grid==5){ #beta, cumulative,  bw
    par(ask=FALSE)
    plot.new()
    pushViewport(viewport(layout = grid.layout(1, 6)))
    define_region <- function(row, col){
      viewport(layout.pos.row = row, layout.pos.col = col)
    } 
    
    print(p.beta, vp=define_region(1, 1))
    print(p.cumulative, vp=define_region(1, 2))
    print(p.bw, vp = define_region(1, 3:6))
    
  }
    
  
}
          
if(!print){
  return(out)
}
}
