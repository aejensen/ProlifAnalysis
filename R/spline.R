spline.fit <- function(x, y, kSeq) {
  data <- data.frame(x = x, y = y)

  bicValues <- sapply(kSeq, function(k) BIC(scam(y ~ s(x, bs="mpi", k = k, m=3), data = data)))
  kOpt <- kSeq[which.min(bicValues)]

  m <- scam::scam(y ~ s(x, bs="mpi", k = kOpt, m=3), data = data)

  d0 <- predict(m)
  d1 <- scam::derivative.scam(m, deriv=1)
  d2 <- scam::derivative.scam(m, deriv=2)

  resid <- residuals(m, type="scaled.pearson")

  list(data = data,
       estimate = as.numeric(d0),
       velocity = as.numeric(d1$d[,1]),
       acceleration = as.numeric(d2$d[,1]),
       residuals = resid,
       BICvalues = list(k = kSeq, bic = bicValues),
       kOpt = kOpt)
}


spline.slope <- function(fit) {
  eF <- approxfun(fit$data$x, fit$estimate)
  vF <- approxfun(fit$data$x, fit$velocity)

  vMax <- optimize(aF, interval=range(fit$data$x), maximum=TRUE)

  time <- vMax$maximum
  slope <- vMax$objective
  value <- eF(vMax$maximum)

  list(time = as.numeric(time),
       value = as.numeric(value),
       slope = as.numeric(slope))
}
