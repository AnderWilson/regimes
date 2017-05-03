
#' Pairwise comparisons betweeng groups for BDLIM
#'
#' @param object An object of class 'bdlim'.
#' @param inter.model Model to be summarized.  The default is \code{inter.model}=1 indicating to summarize the best fitting model.  \code{inter.model}=2, 3, or 4 indicates to summarized the second, third, or fourth best fitting model respectively. Model fit is determined by posterior probability. Alternative, 'BDLIM_n', 'BDLIM_bw', 'BDLIM_b', or 'BDLIM_w' can be entered to return a specific model.
#' @param alphalevel The alpha level for the posterior intervals.
#' @param hpd.interval Logical indicating if highest posterior density intervals should be computed (TRUE) or symmetric intervals (FALSE, default)
#' @param ... additional arguments
#'
#' @return A object of class 'summary.bdlim'.
#' @export
#'
#'
pairwisebdlim <- function(object, inter.model, alphalevel=.05, hpd.interval=FALSE, ...){
  
  if(missing(inter.model)) inter.model <- 1
  m <- NULL
  if(toupper(paste0("BDLIM_",inter.model))%in% toupper(row.names(object$modelfit))){
    m <- row.names(object$modelfit)[which(toupper(row.names(object$modelfit))==toupper(paste0("BDLIM_",inter.model)))]
  }else if(toupper(inter.model)%in% toupper(row.names(object$modelfit))){
    m <- row.names(object$modelfit)[which(toupper(row.names(object$modelfit))==toupper(inter.model))]
  }else if(is.numeric(inter.model[1])){
    if(round(inter.model[1])<= nrow(object$modelfit) & round(inter.model[1])>0) m <- row.names(object$modelfit)[round(inter.model[1])]
  }
  if(is.null(m)){
    inter.model <- 1
    m <- row.names(object$modelfit)[1]
  }
  
  if(inter.model=="BDLIM_n"){
    stop("no pairwise comparisons to be done.")
  }
  
  
  out <- list()
  
  ## beta
  if(m%in%c("BDLIM_b","BDLIM_bw")){
    names <- NULL
    diff <- NULL
    for(g1 in 1:(ncol(object[[m]]$beta)-1)){
      for(g2 in (g1+1):(ncol(object[[m]]$beta))){
        diff <- cbind(diff , object[[m]]$beta[,g2] - object[[m]]$beta[,g1])
        names <- c(names, paste0(colnames(object[[m]]$beta)[g2]," - ",colnames(object[[m]]$beta)[g1]))
        
      } 
    }
    colnames(diff) <- names
    
    
    if(hpd.interval){
      temp <- apply(diff,2,hpd,1-alphalevel)
      lower=temp["lower",]
      upper=temp["upper",]
    }else{
      lower=apply(diff,2,quantile,alphalevel/2)
      upper=apply(diff,2,quantile,1-alphalevel/2)
    }
    
    
    out$beta <- data.frame(mean=colMeans(diff),
                           sd=apply(diff,2,sd),
                           lower=lower,
                           upper=upper,
                           pr <- colMeans(diff>0)
    )
    colnames(out$beta) <- c("Post. Mean", "Post. SD", paste0("q",100*alphalevel/2), paste0("q",100-100*alphalevel/2), "Pr>0")
    
  }
  
  
  ## cumulative
  if(m%in%c("BDLIM_b","BDLIM_bw","BDLIM_w")){
  
  
  
  temp <- NULL
  if(m=="BDLIM_w"){
    for(g in names(object[[m]]$theta)){
      what <- object$B$psi%*%t(object[[m]]$theta[[g]])
      bwhat <- scale(what, center=FALSE, scale=1/object[[m]]$beta)
      temp <- cbind(temp,colSums(bwhat))
    }
  }else if(m=="BDLIM_bw"){
    for(g in names(object[[m]]$theta)){
      what <- object$B$psi%*%t(object[[m]]$theta[[g]])
      bwhat <- scale(what, center=FALSE, scale=1/object[[m]]$beta[,g])
      temp <- cbind(temp,colSums(bwhat))
    }
  }else if(m=="BDLIM_b"){
    what <- object$B$psi%*%t(object[[m]]$theta)
    for(g in colnames(object[[m]]$beta)){
      bwhat <- scale(what, center=FALSE, scale=1/object[[m]]$beta[,g])
      temp <- cbind(temp,colSums(bwhat))
    }
  }
  
 if(m%in%c("BDLIM_bw","BDLIM_b")){
    colnames(temp) <- colnames(object[[m]]$beta)
  }else if(m%in%c("BDLIM_w")){
    colnames(temp) <- names(object[[m]]$theta)
  }
  
  
  names <- NULL
  diff <- NULL
  for(g1 in 1:(ncol(temp)-1)){
    for(g2 in (g1+1):(ncol(temp))){
      diff <- cbind(diff , temp[,g2] - temp[,g1])
      names <- c(names, paste0(colnames(temp)[g2]," - ",colnames(temp)[g1]))
      
    } 
  }
  colnames(diff) <- names
  
  
  if(hpd.interval){
    temp <- apply(diff,2,hpd,1-alphalevel)
    lower=temp["lower",]
    upper=temp["upper",]
  }else{
    lower=apply(diff,2,quantile,alphalevel/2)
    upper=apply(diff,2,quantile,1-alphalevel/2)
  }
  
  
  out$cumulative <- data.frame(mean=colMeans(diff),
                         sd=apply(diff,2,sd),
                         lower=lower,
                         upper=upper,
                         pr <- colMeans(diff>0)
  )
  colnames(out$cumulative) <- c("Post. Mean", "Post. SD", paste0("q",100*alphalevel/2), paste0("q",100-100*alphalevel/2), "Pr>0")
  
  }
  
  class(out) <- "pairwisebdlim"
  return(out)
  
}







#' Default print for pairwise.bdlim object
#' @param x pairwise.bdlim object to print
#' @param ... additional arguments
#' @export
#'
print.pairwisebdlim<- function(x, ...) {
  
  if(!is.null(x$beta)){
    cat("\nPairwise differnces for Beta:\n")
    print(round(x$beta,3))
  }
  if(!is.null(x$beta)){
    cat("\nPairwise differnces for Cumulative:\n")
    print(round(x$cumulative,3))
  }
}




