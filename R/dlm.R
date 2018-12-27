

#' Distributed lag model
#'
#' This estimates a distributed lag model (DLM) with a natrual spline basis.  The model is estimated with the glm function form the stats package.
#'
#' @param Y Vector of outcomes.
#' @param X Matrix of exposures over an evenly spaced grid. There should be no missing exposures.
#' @param Z Optional design matrix or data.frame of covariates. An intercept will be added
#' @param df Degrees of freedom for the natrual spline basis
#' @param family A description of the error distribution and link function to be used in the model. 
#' @import stats
#' @importFrom splines ns
#' @return An object of class dlm.  This is a list containing: ``dlm`` the fitted distribued lag function; ``cumulative`` estimates of the cumulative effect; and ``modelfit`` which contains AIC and BIC for the fitted model; ``basis`` is the natrual spline basis for the dlm; and ``fit`` the fitted object returned from glm.
#' @export
#'
dlm <- function(Y,X,Z,df,family=gaussian){
    
    # make family a function
    if(is.character(family)){
      family <- get(family, mode = "function", envir = parent.frame())
    }
    if(is.function(family)){
      family <- family()
    }
    
    # check data inputs.
    Y <- drop(Y)
    X <- as.matrix(X)
    if(nrow(X)!=length(Y)){
      stop("X must be a numeric matrix with the n rows where n is the length of Y.")
    }
    
    
    B <- ns(1:ncol(X),
            df=df,
            intercept=TRUE)
    dlm <- X %*% B
    
    # fit model
    if(missing(Z)){
      fit <- glm(Y ~ dlm, family = family)
    }else{
      fit <- glm(Y ~ dlm + Z, family = family)
    }
 
    
    # find locations DLM coefficients
    dlm_locations <- grep("dlm", names(fit$coefficients))
    
    # estimate and SE
    theta_hat <- B %*% fit$coefficients[dlm_locations]
    theta_var <- B %*% vcov(fit)[dlm_locations,dlm_locations] %*% t(B)
    theta_SE <- sqrt(diag(theta_var))
    
    # Confidence intervals
    theta_lower  <- theta_hat - 
      theta_SE * qt(0.975, fit$df.residual)
    theta_upper  <- theta_hat + 
      theta_SE * qt(0.975, fit$df.residual)
    # save in data frame
    out <- list(dlm = data.frame(time=1:37,
                                 estimate=theta_hat, 
                                 SE=theta_SE, 
                                 lower=theta_lower, 
                                 upper=theta_upper))
    
    #estimate cumulative effect
    cumulative <- data.frame(estimate = sum(theta_hat),
                             SE = sqrt(sum(theta_var)))
    cumulative$t_val <- cumulative$estimate/cumulative$SE
    cumulative$p_val <-  2*pt(abs(cumulative$t_val),
                               df=fit$df.residual,
                               lower.tail = FALSE)
    cumulative$df = fit$df.residual
    
    
    
    out$cumulative <-   cumulative
    out$modelfit <- data.frame(AIC=AIC(fit),BIC=BIC(fit))
    out$basis <- B
    out$call <- match.call()
    out$fit <- fit
    class(out) <- "dlm"
    
  return(out)  
  
}
    