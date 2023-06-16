xweights <-
function(X, weight, clevel1){
  tmc = qchisq(clevel1, ncol(X))
  dist=mahalanobis(X,
                   cov.rob(X, method=weight)$center,
                   cov.rob(X, method=weight)$cov)  # robust distance
  xweight=ifelse(dist<tmc, 1, tmc/dist)  #robust weights
  return(xweight)
}
