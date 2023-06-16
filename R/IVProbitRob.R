IVProbitRob <-
function(reduced, outcome, data, control=rob.control()) {
  cl <- match.call()
  result <- epmrob(reduced, outcome, data = data, control = control)
  result$call <- cl
  return(result)
}
