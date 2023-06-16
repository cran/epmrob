\name{rob.control}
\alias{rob.control}
\title{
Auxiliary for Controlling Robust Fitting
}
\description{
Auxiliary function used for fitting the endogenous probit models. Contains control sequences, tuning constants and robustness weight functions.
}
\usage{
rob.control(acc = 1e-04, maxit = 50, weights.x1 = c("none", "hat", "robCov", "covMcd"),
        weights.x2 = c("none", "hat", "robCov", "covMcd"), clevel1 = 0.95, tcc = 1.345)
}
\arguments{
  \item{acc}{
positive convergence level.
}
  \item{maxit}{
integer giving the maximum number of iterations.
}
  \item{weights.x1}{
vector of robustness weights controlling for the leverage effect in the reduced equation.
}
  \item{weights.x2}{
vector of robustness weights controlling for the leverage effect in the outcome equation.
}
  \item{clevel1}{
value for the critical level for the first stage.
}
  \item{tcc}{
tuning constant c for Huber's psi-function for the first, reduced, stage.
}
}
\value{
A list with the arguments as components.
}
\author{
Mikhail Zhelonkin, Andre Bik, Andrea Naghi
}
\keyword{ Tuning Constant }
\keyword{ Robust weights }
