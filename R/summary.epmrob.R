summary.epmrob <-
function(object, ...)
{
  seR <- sqrt(diag(vcov(object$stage1)))
  seO <- sqrt(diag(epmrob.vcov(object)$cov)) 
  tvalR <- coef(object)$R / seR
  tvalO <- coef(object)$O / seO
  pvalR <- 2 * pnorm(-abs(coef(object)$R / seR))
  pvalO <- 2 * pnorm(-abs(coef(object)$O / seO))
  SignifR <- symnum(pvalR, corr = FALSE, na = FALSE, 
                    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                    symbols = c("***", "** ", "*  ", ".  ", " "))
  SignifO <- symnum(pvalO, corr = FALSE, na = FALSE, 
                    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                    symbols = c("***", "** ", "*  ", ".  ", " "))
  nms = c("Estimate","Std. Error","t value","Pr(>|t|)", "")
  TAB <- list(reduced=data.frame(Estimate=coef(object)$R, Std.Err=seR, t.value=signif(tvalR, digits=4), p.value=signif(pvalR, digits=3), Sign = as.vector(SignifR) ),
              outcome=data.frame(Estimate=coef(object)$O, Std.Err=seO, t.value=signif(tvalO, digits=4), p.value=signif(pvalO, digits=3), Sign= as.vector(SignifO)) )
  colnames(TAB$reduced) <- nms
  colnames(TAB$outcome) <- nms
  res <- list(call = object$call, coefficients = TAB)
  res$nobs <- length(object$stage1$fitted)
  #res$sigma <- object$sigma
  class(res) <- "summary.epmrob"
  return(res)
}
