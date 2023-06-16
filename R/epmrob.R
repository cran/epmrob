epmrob <-
function(reduced, outcome, data, control = rob.control()){
  if (!inherits(outcome, "formula")) {
    stop("argument 'outcome' must be a formula")
  }
  else if (length(reduced) != 3) {
    stop("argument 'reduced' must be a 2-sided formula")
  }  
  result <- list()
  result$call <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("reduced", "data", "subset", "weights", "offset"), 
             names(mf), 0)
  mfR <- mf[c(1, m)]
  mfR$na.action <- na.pass
  mfR$drop.unused.levels <- TRUE
  mfR[[1]] <- as.name("model.frame")
  names(mfR)[2] <- "formula"
  mfR <- eval(mfR, parent.frame())
  mtR <- attr(mfR, "terms")
  XR <- model.matrix(mtR, mfR)
  NXR <- ncol(XR)
  YR <- model.response(mfR)
  m <- match(c("outcome", "data", "subset", "weights", "offset"), 
             names(mf), 0)
  mfO <- mf[c(1, m)]
  mfO$na.action <- na.pass
  mfO$drop.unused.levels <- TRUE
  mfO$na.action <- na.pass
  mfO[[1]] <- as.name("model.frame")
  names(mfO)[2] <- "formula"
  mfO <- eval(mfO, parent.frame())
  mtO <- attr(mfO, "terms")
  XO <- model.matrix(mtO, mfO)
  NXO <- ncol(XO)
  YO <- model.response(mfO)
  
  if(attributes(XR)$dimnames[[2]][1] == "(Intercept)") { XR <- XR[,-1]; redeqn <- YR ~ XR} 
  else {redeqn <- YR ~ XR - 1}

  if(control$weights.x1 == "none") x.w1 <- rep(1, dim(XR)[1]) else 
    if(control$weights.x1 == "hat") x.w1 <- sqrt(1 - hat(XR)) else 
      if(control$weights.x1 == "robCov") x.w1 <- xweights(XR, "mve", control$clevel1) else
        if(control$weights.x1 == "covMcd") x.w1 <- xweights(XR, "mcd", control$clevel1)
  result$x.w1 <- x.w1
  result$stage1 <- rlm(redeqn, weights=x.w1, method="M")
  step1.RLMres <- result$stage1$residuals
  if(attributes(XO)$dimnames[[2]][1] == "(Intercept)") { XO <- XO[,-1]; outeqn <- YO ~ XO + step1.RLMres}
  else {outeqn <- YO ~ XO + step1.RLMres - 1}
  result$stage2 <- glmrob(outeqn, family = binomial(link = probit), method="Mqle", weights.on.x=control$weights.x2, 
                          control = glmrobMqle.control(acc = control$acc, maxit = control$maxit, tcc = control$tcc))
  
  if(names(result$stage1$coefficients)[1]=="(Intercept)")
  { 
    nr.coef <- length(result$stage1$coefficients)
    names(result$stage1$coefficients)[2:nr.coef] <- substring(names(result$stage1$coefficients)[2:nr.coef], 3)
  } else
  { 
    nr.coef <- length(result$stage1$coefficients)
    names(result$stage1$coefficients)[1:nr.coef] <- substring(names(result$stage1$coefficients)[1:nr.coef], 3)
  }
  if(names(result$stage2$coefficients)[1]=="(Intercept)")
  { 
    nr.coef <- length(result$stage2$coefficients)
    names(result$stage2$coefficients)[2:(nr.coef-1)] <- substring(names(result$stage2$coefficients)[2:(nr.coef-1)], 3)
    names(result$stage2$coefficients)[nr.coef] <- substring(names(result$stage2$coefficients)[nr.coef], 7)
  } else
  { 
    nr.coef <- length(result$stage2$coefficients)
    names(result$stage2$coefficients)[1:(nr.coef-1)] <- substring(names(result$stage2$coefficients)[1:(nr.coef-1)], 3)
    names(result$stage2$coefficients)[nr.coef] <- substring(names(result$stage2$coefficients)[nr.coef], 7)
  }
  #result$cor <- cor(result$stage1$residuals, result$stage2$residuals)
  result$coefficients <- c(result$stage1$coefficients, result$stage2$coefficients)
  class(result) <- c("epmrob", class(result))
  return(result)
}
