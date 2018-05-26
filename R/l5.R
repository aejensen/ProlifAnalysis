l5.fit <- function(x, y) {
  m <- drc::drm(y ~ x, fct = drc::L.5())

  list(coef = coef(m),
       vcov = vcov(m),
       vcov.robust = sandwich::sandwich(m),
       df.residual = stats::df.residual(m),
       residuals = residuals(m, type="standardised"))
}

l5.func <- function(x, par) {
  #5-parameter logistic function
  b <- par[1]
  c <- par[2]
  d <- par[3]
  e <- par[4]
  f <- par[5]

  c + (d - c) / ((1 + exp(b * (x - e)))^f)
}

l5.velocity <- function(t, par) {
  #First order derivative of 5-parameter logistic function
  b <- par[1]
  c <- par[2]
  d <- par[3]
  e <- par[4]
  f <- par[5]

  b * (c - d) * exp(b * (t - e)) *  (1 + exp(b*(t - e)))^(-1 - f) * f
}

l5.acceleration <- function(t, par) {
  #First order derivative of 5-parameter logistic function
  b <- par[1]
  c <- par[2]
  d <- par[3]
  e <- par[4]
  f <- par[5]

  k <- exp(b*(t-e))
  -b^2 * (c - d) * k * (1 + k)^(-2-f) * f * (k * f - 1)
}


l5.slope <- function(par) {
  #Get the time of maximum slope, the slope and the value
  b <- par[1]
  c <- par[2]
  d <- par[3]
  e <- par[4]
  f <- par[5]

  time <- e + log(1/f) / b
  slope <- b * (c-d) * (1 + 1/f)^(-1-f)
  value <- l5.func(time, par)

  list(time = as.numeric(time),
       value = as.numeric(value),
       slope = as.numeric(slope))
}

getSlopeAtZero <- function(par) {
  b <- par[1]
  c <- par[2]
  d <- par[3]
  e <- par[4]
  f <- par[5]

  slope <- b*(c-d)*exp(-b*e)*(1+exp(-b*e))^(-1-f)*f
  value <- l5.func(0, par)

  list(time = 0,
       value = as.numeric(value),
       slope = as.numeric(slope))
}

l5.lag.lin <- function(par) {
  slope <- l5.slope(par)
  slopeAtZero <- getSlopeAtZero(par)

  f1 <- function(t) (slope$value - slope$slope * slope$time) + slope$slope * t
  #f2 <- function(t) slopeAtZero$value + slopeAtZero$slope * t
  f2 <- function(t) slopeAtZero$value + 0 * t
  time <- stats::uniroot(function(x) f1(x) - f2(x), c(0, 150))$root

  value <- l5.func(time, par)

  list(time = as.numeric(time),
       value = as.numeric(value))
}

getCI <- function(func, coef, vcov, alpha = 0.95, B = 10^4, seed=12345) {
  set.seed(seed)

  rand <- mvtnorm::rmvnorm(B, coef, vcov)
  sim <- apply(rand, 1, func)

  ciL <- stats::quantile(sim, (1-alpha)/2)
  ciU <- stats::quantile(sim, 1 - (1-alpha)/2)

  c(ciL, ciU)
}
