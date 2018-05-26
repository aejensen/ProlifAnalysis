summary.ProlifAnalysis <- function(object, ...) {
  cat("Proliferation analysis - 5 parameter logistic model\n\n")

  cat("Model: y(x) = c + (d - c) / ((1 + exp(b * (x - e)))^f) + epsilon\n")
  cat("Inference method:", object$var, "\n")
  cat("Confidence level = ", object$conf.level * 100, "%\n", sep="")

  cMat <- matrix(NA, 5, 3)
  cMat[, 1] <- object$coef
  cMat[, 2] <- object$coefCI[,1]
  cMat[, 3] <- object$coefCI[,2]
  colnames(cMat) <- c("Estimate", "CI Lower", "CI Upper")
  rownames(cMat) <- c("b:", "c:", "d:", "e:", "f:")

  slopeMat <- matrix(NA, 3, 3)
  slopeMat[1,1] <- object$slope$time
  slopeMat[1,2:3] <- object$CI$slope.time

  slopeMat[2,1] <- object$slope$value
  slopeMat[2,2:3] <- object$CI$slope.value

  slopeMat[3,1] <- object$slope$slope
  slopeMat[3,2:3] <- object$CI$slope.slope
  colnames(slopeMat) <- c("Estimate", "CI Lower", "CI Upper")
  rownames(slopeMat) <- c("Time:", "Value:", "Slope:")

  lagMat <- matrix(NA, 2, 3)
  lagMat[1,1] <- object$lagtime.lin$time
  lagMat[1,2:3] <- object$CI$lagtime.lin
  lagMat[2,1] <- object$lagtime.lin$value
  lagMat[2,2:3] <- object$CI$lagvalue.lin
  colnames(lagMat) <- c("Estimate", "CI Lower", "CI Upper")
  rownames(lagMat) <- c("Time:", "Value:")

  cat("\nSlope\n")
  print.default(slopeMat)

  cat("\nLagtime\n")
  print.default(lagMat)

  cat("\nL5 parameters\n")
  print.default(cMat)
  cat("\n")

  invisible(object)
}

print.ProlifAnalysis <- function(x, ...) {
  summary(x)
}


plot.ProlifAnalysis <- function(object, type = "confluency", ...) {
  if(!(type == "confluency" | type == "velocity" | type == "acceleration")) {
    stop("type must be either confluency, velocity or acceleration")
  }

  if(type == "confluency") {
    plot(y ~ x, object$d, ylim=c(0,100), bty="n", pch=20, col="gray50",
         xlab="Elapsed [hours]", ylab="Confluency [%]")

    #Plot estimated relationship
    lines(object$d$x, l5.func(object$d$x, object$coef), lwd=3, col=1)

    #Plot slope
    points(object$slope$time, object$slope$value, pch=15, col=2, cex=2)
    lines(rep(object$slope$time, 2), c(0, object$slope$value), lty=2, col=2)
    lines(c(0, object$slope$time), rep(object$slope$value, 2), lty=2, col=2)

    #Plot lagtime
    points(object$lagtime.lin$time, object$lagtime.lin$value, pch=15, col=1, cex=2)
    lines(rep(object$lagtime.lin$time, 2), c(0, object$lagtime.lin$value), lty=2, col=1)
    lines(c(0, object$lagtime.lin$time), rep(object$lagtime.lin$value, 2), lty=2, col=1)

    legend("topleft", c("Slope", "Lagtime"), col=c(2,1), pch=15, cex=1, bty="n")
  }

  if(type == "velocity") {
    plot(object$d$x, l5.velocity(object$d$x, object$coef), lwd=2, bty="n", type="l",
         xlab="Elapsed [hours]", ylab="Velocity [%/hour]")

    #Plot slope
    points(object$slope$time, object$slope$slope, pch=15, col=1, cex=2)
    lines(rep(object$slope$time, 2), c(0, object$slope$slope), lty=2)
    lines(c(0, object$slope$time), rep(object$slope$slope, 2), lty=2)
  }

  if(type == "acceleration") {
    plot(object$d$x, l5.acceleration(object$d$x, object$coef), lwd=2, bty="n", type="l",
         xlab="Elapsed [hours]", ylab="Acceleration [%/hour^2]")
    abline(h = 0, lty=2)
  }

  invisible(object)
}
