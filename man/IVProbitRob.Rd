\name{IVProbitRob}
\alias{IVProbitRob}
\title{
Robust Probit Model with Endogeneity
}
\description{
Compute robust two-stage estimates of probit model with endogeneity.
}
\usage{
IVProbitRob(reduced, outcome, data, control = rob.control())
}
\arguments{
  \item{reduced}{
formula, the reduced function.
}
  \item{outcome}{
formula, the outcome function.
}
  \item{data}{
an optional data fram containing the varaibles in the model. If not found in data, the variables are taken from \code{environment(formula)}, typically the environment from which \code{IVProbitRob} is called.
}
  \item{control}{
a list of parameters for controlling the fitting process.
}
}
\details{
Compute robust two-step estimates of the endogenous probit model.
}
\value{
Object of class "epmrob".
}
\references{
Naghi, A. A., Varadi, M., & Zhelonkin, M. (2022). Robust Estimation of Probit Models with Endogeneity. \emph{Econometrics and Statistics}. \doi{10.1016/j.ecosta.2022.05.001}
}
\author{
Mikhail Zhelonkin, Andre Bik, Andrea Naghi
}
\seealso{
\code{\link{epmrob}}
}
\examples{
library(mvtnorm)
N <- 2000
M <- 500
cont.frac = 0.01
rho = 0.5
sigma = matrix(c(1, rho, rho, 1), 2, 2)
gamma1 = 1
gamma2 = c(0.6, 0.4) 
alpha1 = c(0.5)
beta1 = 0.5

set.seed(123)
X1 = rnorm(N,0,1)
X2 = rnorm(N,0,1)
X3 = rnorm(N,0,1)
eps = rmvnorm(N, mean =rep(0,2), sigma = sigma)
Y1 = X1*gamma1 + X2*gamma2[1] + X3*gamma2[2]  + eps[,1]
Y2 = ifelse(X1*beta1 + Y1*alpha1 + eps[,2] > 0, 1, 0)

dat.exmpl <- data.frame(Y2, Y1, X1, X2, X3)
names(dat.exmpl) <- c("int", "endo", "exo", "ivrbl", "ivrbl2")
ivp.fit <- IVProbitRob(endo ~ exo + ivrbl + ivrbl2, int ~ endo + exo, data = dat.exmpl, 
                       control = rob.control(weights.x1 = "hat", weights.x2 = "hat"))
ivp.fit
}
\keyword{ Robust estimator }
\keyword{ Endogeneity }
\keyword{ Probit model }