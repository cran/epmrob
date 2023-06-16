\name{epmrob}
\alias{epmrob}
\title{
Robust Fit of Probit Model with Endogeneity
}
\description{
Fits the endogenous probit model using a robust two-stage estimator.
}
\usage{
epmrob(reduced, outcome, data, control = rob.control())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{reduced}{
formula, the reduced equation.
}
  \item{outcome}{
formula, the outcome equation.
}
  \item{data}{
an optional data frame containing the variables in the model. If not found in data, the variables are taken from \code{environment(formula)}, typically the environment from which \code{epmrob} is called.
}
  \item{control}{
a list of parameterse for controlling the fitting process.
}
}
\details{
Compute robust two-step estimates of the endogenous probit model. The robust linear regression is fitted in the first stage. In the second stage a Mallows-type M-estimator is used instead of the probit MLE. 
The values of the tuning constants and the robustness weights can be modified in \code{\link{rob.control}}.
}
\value{
Object of class "epmrob".
  \item{coefficients}{a named vector of coefficients}
  \item{stage1}{object of class \code{rlm} that contains first stage robust fit}
  \item{stage2}{object of class \code{glmrob} that contains second stage robust probit fit. Note that the standard errors in this object are biased, since they are not corrected for the uncertainty in the first estimation step. Use \code{vcov} below}
  \item{vcov}{variance matrix of the second stage}
  \item{call}{the matched call}
}
\references{
Naghi, A. A., Varadi, M., and Zhelonkin, M. (2022). Robust Estimation of Probit Models with Endogeneity. \emph{Econometrics and Statistics}. \doi{10.1016/j.ecosta.2022.05.001}
}
\author{
Mikhail Zhelonkin, Andre Bik, Andrea Naghi
}
\seealso{
\code{\link[MASS]{rlm}}, \code{\link{rob.control}}
}
\keyword{ Robust estimator }
\keyword{ Endogeneity }
\keyword{ Probit model }
