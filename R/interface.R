estimateL5 <- function(x, y, var = "robust", conf.level = 0.95, B = 10^4, seed = 12345) {
  if(!(var == "robust" | var == "asymp")) {
     stop("var argument must be either asymp or robust")
  }

  #Estimate 5-parameter logsictic model
  est <- l5.fit(x, y)

  #Get coefficients and covariance matrices
  coef <- est$coef
  names(coef) <- c("b", "c", "d", "e", "f")
  vcov <- est$vcov
  vcovR <- est$vcov.robust

  #Get confidence intervals for model parameters
  critValue <- qt(1 - (1 - conf.level)/2, est$df.residual)
  if(var == "asymp") {
    seParam <- sqrt(diag(vcov))
  } else if(var == "robust") {
    seParam <- sqrt(diag(vcovR))
  }
  coefCI <- matrix(NA, 5, 2)
  coefCI[, 1] <- coef - critValue * seParam
  coefCI[, 2] <- coef + critValue * seParam
  colnames(coefCI) <- c("CI Lower", "CI Upper")
  rownames(coefCI) <- c("b", "c", "d", "e", "f")

  #Get summary statistics
  slope <- l5.maxSlope(coef)
  lagtime.lin <- l5.lagtime.linear(coef)
  lagtime.acc <- l5.lagtime.acceleration(coef)
  timeTo100 <- l5.timeTo100(est$coef)

  #Calculate standard confidence intervals for summary statistics
  set.seed(seed)
  if(var == "robust") {
    rand <- mvtnorm::rmvnorm(B, coef, vcovR)
  } else if(var == "asymp") {
    rand <- mvtnorm::rmvnorm(B, coef, vcov)
  }

  simSlopeTime <- apply(rand, 1, function(x) l5.maxSlope(x)$time)
  simSlopeSlope <- apply(rand, 1, function(x) l5.maxSlope(x)$slope)
  simSlopeValue <- apply(rand, 1, function(x) l5.maxSlope(x)$value)

  simLagLinTime <- apply(rand, 1, function(x) l5.lagtime.linear(x)$time)
  simLagLinValue <- apply(rand, 1, function(x) l5.lagtime.linear(x)$value)

  simLagAccTime <- apply(rand, 1, function(x) l5.lagtime.acceleration(x)$time)
  simLagAccValue <- apply(rand, 1, function(x) l5.lagtime.acceleration(x)$value)

  simTimeTo100Time <- apply(rand, 1, function(x) l5.timeTo100(x)$time)
  simTimeTo100Value <- apply(rand, 1, function(x) l5.timeTo100(x)$value)

  qLU <- c((1-conf.level)/2, 1-(1-conf.level)/2)
  simSlopeTimeCI <- quantile(simSlopeTime, qLU)
  simSlopeSlopeCI <- quantile(simSlopeSlope, qLU)
  simSlopeValueCI <- quantile(simSlopeValue, qLU)

  simLagLinTimeCI <- quantile(simLagLinTime, qLU)
  simLagLinValueCI <- quantile(simLagLinValue, qLU)

  simLagAccTimeCI <- quantile(simLagAccTime, qLU)
  simLagAccValueCI <- quantile(simLagAccValue, qLU)

  simTimeTo100TimeCI <- quantile(simTimeTo100Time, qLU)
  simTimeTo100ValueCI <- quantile(simTimeTo100Value, qLU)

  CI <- list(slope.time = simSlopeTimeCI,
             slope.slope = simSlopeSlopeCI,
             slope.value = simSlopeValueCI,
             lagtime.lin = simLagLinTimeCI,
             lagvalue.lin = simLagLinValueCI,
             lagtime.acc = simLagAccTimeCI,
             lagvalue.acc = simLagAccValueCI,
             timeTo100.time = simTimeTo100TimeCI,
             timeTo100.value = simTimeTo100ValueCI)

  #Gather the results
  out <- list(data = data.frame(x = x, y = y),
              residuals = est$residuals,
              coef = coef,
              vcov = vcov,
              vcovR = vcovR,
              coefCI = coefCI,
              slope = slope,
              lagtime.lin = lagtime.lin,
              lagtime.acc = lagtime.acc,
              timeTo100 = timeTo100,
              lin1Func = l5.lin1Func(coef),
              lin2Func = l5.lin2Func(coef),
              CI = CI,
              conf.level = conf.level,
              var = var)

  class(out) <- append(class(out), "ProlifAnalysisL5")

  out
}

