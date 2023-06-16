coef.epmrob <-
function(object, ...) {
  coeff <- list()
  coeff$R <- coef(object$stage1)
  coeff$O <- coef(object$stage2)
  class(coeff) <- c("coef.epmrob", class(coeff), class(coeff$S))
  return(coeff)
}
