#-------------------------------------------------------------------------------------------------
#' Bayesian kernel machine regression - distributed lag model
#'
#' This estimates the Bayesian kernel machine regression - distributed lag model (BKMR-DLM).
#'
#' @param y Vector of outcomes.
#' @param x A list of matricies. Each Matric should be n by T with each row being the T-vector of exposures for one individuals. When only one exposure is used x can be a matrix instead of a list.
#' @param z a matrix or design frame of covaiates and confounds. This can be ommited.
#' @param niter Number of MCMC iterations including burnin.
#' @param nburn The number of iterations to be discarded as burnin.
#' @param nthin Thining, every nthin-th draw from the posterior will be saved.
#' @param prior_sigma Vector of length 2 with the parameters for the gamma prior on sigma^{-2}
#' @param prior_tau the prior variance on log(tau^2).
#' @param kappa scale parameter, rho/kappa~chisq(1).
#' @param basis.opts List with the entries: type = the type of basis used, either 'face' or "ns" or "bs" for splines or "gam" for presmoothing the exposure with a gam following defaults from mgcv; knots = the number of knots used for method face; pve = the percent of variance explained by the PCs for method face; df = the df for ns method. The default is natural splines.
#' @param gaussian Use a Gaussian kernel (TRUE, default) or a polynomial kernel (FALSE)
#' @param polydegree Degree of polynomial when polynomial kernel is used.  Only applies when gaussian=FALSE.
#' @return An object of class 'bkmrdlm'.
#' @author Ander Wilson
#' @importFrom stats model.matrix sd
#' @importFrom GIGrvg rgig
#' @examples  
#' library(regimes)
#' #simulate data from scenario A
#' dat <- simBKMRDLM(n = 200, scenario="A", sd=1, seed=1234)
#' # Estimate model
#' # This may take a few minutes
#' # Increase iterations for a real analysis
#' fit <- bkmrdlm(y=dat$y,
#'                x=dat$x,
#'                z=dat$z,
#'                niter=100,
#'                gaussian=FALSE,
#'                polydegree=2)
#'                
#' summary(fit)
#' plot(fit)
#' @export



bkmrdlm <- function(y,
                    x,
                    z,
                    niter,
                    nburn=round(niter/2), 
                    nthin=1,  
                    prior_sigma=c(0.1,0.1),
                    prior_tau=0.1,
                    kappa=1, 
                    basis.opts=list(type="ns", df=3),
                    gaussian=TRUE,
                    polydegree=2){



  #####################
  ## other inputs
  #####################
  niter <- round(niter)
  nburn <- round(nburn)
  nthin <- round(nthin)
  if(niter<1){
    stop("niter must be a positive integer")
  }
  if(nburn<0){
    nburn <- 0;
  }
  if(nburn>=niter){
    stop("nburn must less than niter")
  }
  if(nthin<1){
    nthin <- 1;
  }
  if((niter%%nthin)!=0){
    stop("niter must be a multiple of nburn")
  }
  if(!gaussian){
    if(missing(polydegree)){
      stop("Using polynomial kernel of order up to 2")
      polydegree=2
    }
  }

  #####################
  ## checks of data
  #####################
  # sample size
  n <- length(y)
  if(any(is.na(y))){
    stop("missing values are not allowed in y")
  }

  # design matrix for Z
  if(!missing(z)){
    if(is.null(z)){
      stop("current version only with with at least one non-intercept covariate in z")
    }else{
      if(any(is.na(z))){
        stop("missing values are not allowed in z")
      }
      Z <- model.matrix(~z)
      Z <- Z[,qr(Z)$pivot[1:qr(Z)$rank]][,-1]
    }
  }else{
    stop("current version only with with at least one non-intercept covariate in z")
  }
  # check that Z has n obs
  if(nrow(Z)!=n){
    stop("number of observations in y and Z must match")
  }


  # check dimensions of X
  if(is.list(x)){
    xnames <- names(x)
    M <- length(x)
    x <- lapply(x, as.matrix)
    dims <- as.data.frame(do.call(rbind, lapply(x,dim)))
    Tmax <- dims[1,2]
    if(any(dims[,1]!=n)){
      stop("number of observations in y and x must match")
    }
    if(any(dims[,2]!=Tmax)){
      stop("number of columns of each element of x must match")
    }
    # check for missing in x
    if(any(do.call(rbind, lapply(x,is.na)))){
      stop("missing values are not allowed in x")
    }
    mnx <- sdx <- rep(NA,M)
    for(m in 1:M){
      mnx[m] <- mean(x[[m]])
      sdx[m] <- sd(x[[m]])
      x[[m]] <- (x[[m]]-mnx[m])/sdx[m]
    }
  }else{
    stop("x must be a list of matricies")
  }
  
  
  ########################
  ## basis construction
  ########################
  
  X <- B <- list()
  for(m in 1:M){
    if(toupper(basis.opts$type) %in% c("NS","BS","FACE","GAM","MEAN","AVERAGE")){
      B[[m]] <- bdlimbasis(x[[m]],basis.opts)
    }else{
      stop("basis type not recognized.")
    }
    
    X[[m]] <- x[[m]]%*%B[[m]]$psi
  }
  if(!is.null(xnames)) names(B) <- xnames
  


  ########################
  ## fit model
  ########################
  
  # fit multi pollutant model
  if(toupper(basis.opts$type)=="MEAN"){
    stop("Not available")
  }else{
    if(gaussian){
      fit <- bkmrdlm_multi(yz=cbind(y,Z), 
                           Xlist= X, 
                           b1=prior_tau[1], 
                           a1=prior_sigma[1], 
                           a2=prior_sigma[2],
                           kappa=kappa,
                           n_inner=nthin, 
                           n_outer=round(niter/nthin), 
                           n_burn=round(nburn/nthin))  
    }else{z
      fit <- bkmrdlm_multi_polynomial(yz=cbind(y,Z), 
                                      Xlist= X, 
                                      b1=prior_tau[1], 
                                      a1=prior_sigma[1], 
                                      a2=prior_sigma[2],
                                      kappa=kappa,
                                      n_inner=nthin, 
                                      n_outer=round(niter/nthin), 
                                      n_burn=round(nburn/nthin), 
                                      d=polydegree)  
    }
  }
  ########################
  ## remove burnin
  ########################
  #remove burnin for theta
  for(m in 1:M){
    fit$theta[[m]] <- as.matrix(fit$theta[[m]][(nburn/nthin+1):(niter/nthin),])

    # flip sign to impose positivity constraint
    cmw <- rowMeans(fit$theta[[m]]%*%t(B[[m]]$psi))
    if(any(cmw<0)){
      fit$theta[[m]][which(cmw<0),] <- -fit$theta[[m]][which(cmw<0),]
    }


    # recale to divide by number of data times
    fit$theta[[m]] <- fit$theta[[m]]*sqrt(nrow(B[[m]]$psi))
    fit$rho[,m] <- fit$rho[,m]/sqrt(nrow(B[[m]]$psi))
  }
  if(!is.null(xnames)) names(fit$theta) <- xnames


  #remove burnin for other values
  fit$tau2 <- fit$tau2[(nburn/nthin+1):(niter/nthin),]
  fit$rho <- fit$rho[(nburn/nthin+1):(niter/nthin),]
  fit$sigma2 <- fit$sigma2[(nburn/nthin+1):(niter/nthin),]
  fit$beta <- fit$beta[(nburn/nthin+1):(niter/nthin),]
  if(!is.null(colnames(Z))) colnames(fit$beta) <- colnames(Z)





  ########################
  ## return values
  ########################
  fit$y <- y
  fit$x <- x
  fit$z <- Z
  fit$xscale <- list(mean=mnx, sd=sdx)
  fit$basis <- B
  fit$hsummary = data.frame(mean=fit$hmean,
                          sd = sqrt(diag(fit$hcov)),
                          lower = fit$hmean + qnorm(0.05/2) * sqrt(diag(fit$hcov)),
                          upper = fit$hmean + qnorm(1 - 0.05/2) * sqrt(diag(fit$hcov)))
  # fit$hmean <- NULL
  fit$call <- match.call()
  class(fit) <- "bkmrdlm"
  return(fit)



}
