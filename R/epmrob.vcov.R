epmrob.vcov <-
function(object, B = 200, control = rob.control()){
  YR <- object$stage1$model[, 1]
  XR <- model.matrix(object$stage1)
  YO <- object$stage2$model[, 1]
  XO <- model.matrix(object$stage2)
  xwht1 <- object$x.w1
  n <- length(YO)
  btstrap = matrix(0, B, dim(XO)[2])
  for(i in 1:B){
    b = sample(1:n, n, TRUE)
    XRb = XR[b,] 
    XOb = XO[b,] 
    YRb = YR[b] 
    YOb = YO[b] 
    xwg1 <- xwht1[b]
    if(attributes(XRb)$dimnames[[2]][1] == "(Intercept)") { XRb <- XRb[,-1]; reqn <- YRb ~ XRb} 
    else {reqn <- YRb ~ XRb - 1}
    result <- list()
    if(control$weights.x1 == "none") x.w1 <- rep(1, dim(XR)[1]) else 
      if(control$weights.x1 == "hat") x.w1 <- sqrt(1 - hat(XR)) else 
        if(control$weights.x1 == "robCov") x.w1 <- xweights(XR, "mve", control$clevel1) else
          if(control$weights.x1 == "covMcd") x.w1 <- xweights(XR, "mcd", control$clevel1)
    result$stage1 = rlm(reqn, weights = x.w1, method="M")
    XOb[, dim(XOb)[2]] <- result$stage1$residuals
    if(attributes(XOb)$dimnames[[2]][1] == "(Intercept)") { XOb <- XOb[,-1]; oeqn <- YOb ~ XOb} 
    else {oeqn <- YOb ~ XOb - 1}
    result$stage2 <- glmrob(oeqn, family = binomial(link = probit), method="Mqle", weights.on.x=control$weights.x2, 
                            control = glmrobMqle.control(acc = control$acc, maxit = control$maxit, tcc = control$tcc))
    btstrap[i,] = result$stage2$coefficients
  }
  res <- list()
  res$CI <- data.frame(apply(btstrap, 2, quantile, probs = c(0.025, 0.975)))
  names(res$CI) <- names(object$stage2$coefficients)
  res$cov <- cov(btstrap)
  return(res)
}
