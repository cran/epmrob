\name{xweights}
\alias{xweights}
\title{
Robustness Weights
}
\description{
Auxiliary function. Computation of the leverage weights based on robust Mahalanobis distance.
}
\usage{
xweights(X, weight, clevel1)
}
\arguments{
  \item{X}{
matrix of explanatory variables.
}
  \item{weight}{
method used to calculated the weighting.
}
  \item{clevel1}{
the critical level used for calculating the Mahalanobis distance.
}
}
\value{
Vector of weights.
}
\author{
Mikhail Zhelonkin, Andre Bik, Andrea Naghi
}
\seealso{
\code{\link[MASS]{cov.rob}}
}