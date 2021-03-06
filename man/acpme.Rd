% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acpme.R
\name{acpme}
\alias{acpme}
\title{Adjustment for confounding in the presence of multivariate exposure}
\usage{
acpme(
  Z,
  C,
  y,
  niter,
  burnin = round(niter/2),
  pen.lambda = NA,
  pen.type = "eigen"
)
}
\arguments{
\item{Z}{Matrix of exposures. This should include any interactions of other functions of exposures.}

\item{C}{A n x p matrix or data.frame of covaraites.}

\item{y}{An n-vector of observed outcomes.}

\item{niter}{Integer number of MCMC iterations to compute including burnin.}

\item{burnin}{Integer number of MCMC iterations to discard as burning.}

\item{pen.lambda}{Non-negative tuning parameter lambda to control the strength of confounder adjustment (strength of prior or size of penalty). A value of NA (defailt) uses BIC to choose the value.}

\item{pen.type}{Choice of penalty. The default is "eigen." Other options are "correlation" and "projection."}
}
\description{
This function simulates the posterier exposure effect using the Bayesian adjustment for confounding in the presence of multivariate exposures (ACPME) meethod.
}
\examples{
dat <- simregimes(scenario="acpme1", seed=1234, n=200, p=100)
fit <- acpme(Z=dat$Z,C=dat$C,y=dat$Y, niter=1000)
}
