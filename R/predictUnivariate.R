

#' Predict univariate result for polynomial kernel BKMR-DLM
#'
#' @param object An object of class bkmrdlm.
#' @param points Number of points to predict at
#' @param crossM Exposure to set a cross section quantile for
#' @param qtl quantile for the cross section
#'
#' @return a dataframe containing predicted values.
#' @export
#'

predictUnivariate <- function(object,points =20, crossM, qtl=0.5){
  
  # setup
  M <- length(object$x)
  n <- length(object$y)
  n_iter <- nrow(object$beta)
  
  
  # recale to divide by number of data times and rho
  # this is theta_star
  
  weights <- list()
  weightsums <- matrix(NA,n_iter,M)
  gridpoints <- matrix(NA,points,M)
  for(m in 1:M){
    weights[[m]] <- object$theta[[m]] %*% t(object$basis[[m]]$psi) / sqrt(nrow(object$basis[[m]]$psi)) /sqrt(object$rho[,m])
    weightsums[,m] <- rowSums(weights[[m]]) #user this for multiplying by a constant quantile.
    allpoints <- object$x[[m]] %*% t(object$theta[[m]] %*% t(object$basis[[m]]$psi) / sqrt(nrow(object$basis[[m]]$psi)))
    gridpoints[,m] <- seq(quantile(allpoints,(1)/(points+1)),quantile(allpoints,(points)/(points+1)), length=points)
  }
  rm(list=c("allpoints"))
  
  Xthetastar <- matrix(NA,n,M)
  Xthetastar_new <- matrix(NA,points,M)
  I_tau2_Kold <- matrix(NA,n,n)
  hmean <- hvar <- matrix(0,points,M)
  for(s in 1:n_iter){
    
    if(s%%100 == 0){message(s)}
    
    # resid
    r <- (object$y-object$z%*%object$beta[s,]) 
    
    # weighted exposures for data used to fit the data
    for(m in 1:M){
      Xthetastar[,m] <- object$x[[m]] %*% weights[[m]][s,]
    }
    
    # kernel for old data
    I_tau2_Kold <- matrix(NA,n,n)
    for(i in 1:n){
      I_tau2_Kold[i,] <- object$tau2[s]*(1+ Xthetastar%*%Xthetastar[i,] )^2
    }
    
    # add I to Kernel and invert this is (I-tau^2*K_old)^{-1}
    diag(I_tau2_Kold) <- diag(I_tau2_Kold) +1
    I_tau2_Kold_inv <- chol2inv(chol(I_tau2_Kold))
    
    # make kernel for new data and between new and old data.
    for(m in 1:M){
      
      tau2_Knew    <- matrix(NA,points,points)
      tau2_Knewold <- matrix(NA,points,n)
      
      # Xthetastar_new has all exposures at the median level.
      for(l in 1:M){
        Xthetastar_new[,l] <- median(object$x[[l]]) * weightsums[s,l]
      }
      # replace one exposure (crossM) with exposrues at the designated quantiles
      # Don't do this if predicting the exposure response for exposure crossM.
      if(!missing(crossM)){
        if(m!=crossM){
          Xthetastar_new[,crossM] <- quantile(object$x[[crossM]],qtl) * weightsums[s,crossM]
        } 
      }
      # replace the exposure of interest with spaced quantiles.
      Xthetastar_new[,m] <- gridpoints[,m] /sqrt(object$rho[s,m])
      
      # kernels for new data and covariatnce kernel
      tau2_Koldnew <- matrix(NA,points,n)
      tau2_Knew <- matrix(NA,points,points)
      for(i in 1:points){
        Xthetastar_new
        
        tau2_Knew[i,] <- object$tau2[s]*(1+Xthetastar_new%*%Xthetastar_new[i,])^2
        tau2_Koldnew[i,] <- object$tau2[s]*(1+Xthetastar%*%Xthetastar_new[i,])^2
      }
      
      # predict
      temp <- tau2_Koldnew%*%I_tau2_Kold_inv
      h <- temp %*% r
      hmean[,m] <- hmean[,m] + h/n_iter
      hvar[,m]  <- hvar[,m]  + (h^2)/n_iter + object$sigma2[s]*c(diag(tau2_Knew) - rowSums(temp*tau2_Koldnew))/n_iter
      
    } # end M loop
  }  # end S loop
  
  
  fits <- NULL
  for(m in 1:M){
    temp <- 
      data.frame(m = m,
                 name = names(object$x)[m],
                 mean = hmean[,m],
                 sd = sqrt(hvar[,m]-hmean[,m]^2),
                 E = ((gridpoints[,m] * sum(object$basis[[m]]$psi%*%colMeans(object$theta[[m]])))
                      -mean(object$x[[m]]%*%object$basis[[m]]$psi%*%colMeans(object$theta[[m]])))/sd(object$x[[m]]%*%object$basis[[m]]$psi%*%colMeans(object$theta[[m]])) #scale according to exposure.
      )
    fits <- rbind(fits,temp)
  }
  
  fits$lower <- fits$mean - 1.96*fits$sd
  fits$upper <- fits$mean + 1.96*fits$sd
  if(!missing(crossM)){
    fits$cross_m <- crossM
    fits$cross <- names(object$x)[crossM]
    fits$qtl <- qtl
  }
  
  
  return(fits)
}
