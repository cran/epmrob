\name{epmrob.vcov}
\alias{epmrob.vcov}
\title{
Extract Asymptotic Variance Covariance Matrix
}
\description{
Extracts the variance covariance matrix of the robust endogenous probit model fit by applying a bootstrap.
}
\usage{
epmrob.vcov(object, B = 200, control = rob.control())
}
\arguments{
  \item{object}{
object of class "\code{\link{epmrob}}".
}
  \item{B}{
the number of samples used in the bootstrapping process used to calculate the variance covariance matrix.
}
  \item{control}{
a list of parameters for controlling the fitting process.
}
}
\details{
The variance covariance matrix is estimated by applying a bootstrap
}
\value{
Variance covariance matrix of the entire estimation procedure. Variance covariance matrix of the reduced stage or outcome stage can be extracted using the \code{vcov} function for the corresponding stage estimator, e.g. \code{vcov(epmrob.object$stage1)} or \code{vcov(epmrob.object$stage2)}.
}
\author{
Mikhail Zhelonkin, Andre Bik, Andrea Naghi
}
