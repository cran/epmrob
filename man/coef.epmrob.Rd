\name{coef.epmrob}
\alias{coef.epmrob}
\title{
Extract Coefficients from the Robust Endogenous Probit Model Fit
}
\description{
Returns the coefficients of the robust two-stage estimator for endogenous probit model.
}
\usage{
\method{coef}{epmrob}(object, ...)
}
\arguments{
  \item{object}{
object of class "\code{\link{epmrob}}".
}
  \item{\dots}{
currently not used.
}
}
\value{
Returns a list of two vectors of parameters of two stages.
  \item{R }{coefficients of the reduced equation.}
  \item{O }{coefficients of the outcome equation.}
}
\author{
Mikhail Zhelonkin, Andre Bik, Andrea Naghi
}
