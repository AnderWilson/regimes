



#' Simulate data to demonstrate methods in regimes package.
#'
#' @param scenario Model to be simulated from. Options include: acpme1, acpme2, bdlim1, ..., bdlim5.
#' @param seed An optional random seed.
#' @param ... Additional options for specific simulated data sets.
#'
#' @return A list containing the simulated data, true paramters, and opts which speficies the scenario being simulated from.
#' @export
#'
#' @examples
#' dat_acpme1 <- simregimes(scenario="acpme1", seed=1234, n=200, p=100)
#' dat_acpme2 <- simregimes(scenario="acpme2", seed=1234, n=200, p=100, m.main=3)
#' dat_bdlim1 <- simregimes(scenario="bdlim1", seed=1234, n=200)


simregimes <- function(scenario, seed, ...){

  #list of available models
  mods <- c("acpme1","acpme2", "bdlim1", "bdlim2", "bdlim3", "bdlim4", "bdlim5")


  if(!missing(seed)) set.seed(seed)
  if(missing(scenario)){

    message(paste0("ERROR: No scenario specified. The following models are available:\n",paste(mods, collapse=", ")))
    return(NULL)

  }else if(toupper(scenario)=="ACPME1"){

    out <- simACPME(model=1, ...)

  }else if(toupper(scenario)=="ACPME2"){
    
    out <- simACPME(model=2, ...)
    
  }else if(toupper(scenario)%in%paste0("BDLIM",1:5)){
    
    out <- simBDLIM(design=as.numeric(substring(toupper(scenario),6,6)), ...)
    
  }else{

    message(paste0("ERROR: Unsupported scenario specified. The following models are available:\n",paste(mods, collapse=", ")))
    return(NULL)

  }
  out$opts$scenario=scenario
  return(out)

}
