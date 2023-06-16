rob.control <-
function(acc = 1e-4, maxit = 50, weights.x1 = c("none", "hat", "robCov", "covMcd"),
                     weights.x2 = c("none", "hat", "robCov", "covMcd"), clevel1 = 0.95, tcc = 1.345){
  if (!is.numeric(acc) || acc <= 0) 
    stop("value of acc must be > 0")
  if (!is.numeric(maxit) || maxit <= 0) 
    stop("maximum number of iterations must be > 0")
  if (!is.numeric(tcc) || tcc <= 0) 
    stop("value of the tuning constant c (tcc) must be > 0")
  if (!is.numeric(clevel1) || clevel1 <= 0 || clevel1 >= 1) 
    stop("value of the significance level for step 1 must be > 0 and < 1")
  if (!is.character(weights.x1) || !is.character(weights.x2)) 
    stop("choose the implemented method of the weight function")
  list(acc = acc, maxit = maxit, weights.x1 = weights.x1[1], weights.x2 = weights.x2[1], clevel1 = clevel1, tcc = tcc)
}
