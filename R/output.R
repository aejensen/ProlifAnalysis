summary.ProlifAnalysisL5 <- function(object, ...) {
  cat("Proliferation analysis - 5 parameter logistic model\n\n")

  cat("Model: y(x) = c + (d - c) / ((1 + exp(b * (x - e)))^f) + epsilon\n")
  cat("Inference method:", object$var, "\n")
  cat("Confidence level = ", object$conf.level * 100, "%\n", sep="")

  #Get matrix of L5 coefficients
  cMat <- matrix(NA, 5, 3)
  cMat[, 1] <- object$coef
  cMat[, 2] <- object$coefCI[,1]
  cMat[, 3] <- object$coefCI[,2]
  colnames(cMat) <- c("Estimate", "CI Lower", "CI Upper")
  rownames(cMat) <- c("b:", "c:", "d:", "e:", "f:")

  #Get matrix of slope results
  slopeMat <- matrix(NA, 3, 3)
  slopeMat[1,1] <- object$slope$time
  slopeMat[2,1] <- object$slope$value
  slopeMat[3,1] <- object$slope$slope

  slopeMat[1,2:3] <- object$CI$slope.time
  slopeMat[2,2:3] <- object$CI$slope.value
  slopeMat[3,2:3] <- object$CI$slope.slope
  colnames(slopeMat) <- c("Estimate", "CI Lower", "CI Upper")
  rownames(slopeMat) <- c("Time:", "Value:", "Slope:")

  #Get matrix of linear lagtime results
  lagLinMat <- matrix(NA, 2, 3)
  lagLinMat[1,1] <- object$lagtime.lin$time
  lagLinMat[2,1] <- object$lagtime.lin$value

  lagLinMat[1,2:3] <- object$CI$lagtime.lin
  lagLinMat[2,2:3] <- object$CI$lagvalue.lin
  colnames(lagLinMat) <- c("Estimate", "CI Lower", "CI Upper")
  rownames(lagLinMat) <- c("Time:", "Value:")

  #Get matrix of acceleration lagtime results
  lagAccMat <- matrix(NA, 2, 3)
  lagAccMat[1,1] <- object$lagtime.acc$time
  lagAccMat[2,1] <- object$lagtime.acc$value

  lagAccMat[1,2:3] <- object$CI$lagtime.acc
  lagAccMat[2,2:3] <- object$CI$lagvalue.acc
  colnames(lagAccMat) <- c("Estimate", "CI Lower", "CI Upper")
  rownames(lagAccMat) <- c("Time:", "Value:")

  #Get matrix of time to 100% results
  timeMat <- matrix(NA, 1, 3)
  timeMat[1,1] <- object$timeTo100$time
  #timeMat[2,1] <- object$timeTo100$value

  timeMat[1,2:3] <- object$CI$timeTo100.time
  #timeMat[2,2:3] <- object$CI$timeTo100.value
  colnames(timeMat) <- c("Estimate", "CI Lower", "CI Upper")
  #rownames(timeMat) <- c("Time:", "Value:")
  rownames(timeMat) <- c("Time:")

  cat("\nMaximum slope\n")
  print.default(slopeMat)

  cat("\nLagtime (acceleration based)\n")
  print.default(lagAccMat)

  cat("\nLagtime (linear approximations)\n")
  print.default(lagLinMat)

  cat("\nTime to 100% confluency\n")
  print.default(timeMat)

  cat("\nL5 parameters\n")
  print.default(cMat)
  cat("\n")

  invisible(object)
}

print.ProlifAnalysisL5 <- function(x, ...) {
  summary(x)
}



plot.ProlifAnalysisL5 <- function(object, type = "confluency", ...) {
  if(!(type == "confluency" | type == "velocity" | type == "acceleration" | type == "residuals")) {
    stop("type must be either confluency, velocity, acceleration or residuals")
  }

  if(type == "confluency") {
    plot(y ~ x, object$d, ylim=c(0,100), bty="n", pch=20, col="gray50",
         xlab="Time", ylab="Confluency [%]")

    #Plot estimated relationship
    lines(object$d$x, l5.func(object$d$x, object$coef), lwd=3, col=1)

    #Plot slope
    lines(object$d$x, object$lin2Func(object$d$x), col=2, lwd=2)
    points(object$slope$time, object$slope$value, pch=15, col="red", cex=2)
    lines(rep(object$slope$time, 2), c(0, object$slope$value), lty=2, col="red")
    lines(c(0, object$slope$time), rep(object$slope$value, 2), lty=2, col="red")

    #Add the tangent line at the slope
    #tangentLine <- function(t) (object$slope$value - object$slope$slope * object$slope$time) + object$slope$slope * t
    #curve(tangentLine(x), min(object$d$x), max(object$d$x), add=TRUE, col=2)

    #Plot lagtime linear
    points(object$lagtime.lin$time, object$lagtime.lin$value, pch=15, col="burlywood", cex=2)
    lines(rep(object$lagtime.lin$time, 2), c(0, object$lagtime.lin$value), lty=2, col="burlywood")
    lines(c(0, object$lagtime.lin$time), rep(object$lagtime.lin$value, 2), lty=2, col="burlywood")

    #Plot lagtime acceleration
    points(object$lagtime.acc$time, object$lagtime.acc$value, pch=15, col="burlywood4", cex=2)
    lines(rep(object$lagtime.acc$time, 2), c(0, object$lagtime.acc$value), lty=2, col="burlywood4")
    lines(c(0, object$lagtime.acc$time), rep(object$lagtime.acc$value, 2), lty=2, col="burlywood4")

    points(object$timeTo100$time, 100, pch=15, col="cornflowerblue", cex=2)
    lines(rep(object$timeTo100$time, 2), c(0, 100), lty=2, col="cornflowerblue")
    lines(c(0, object$timeTo100$time), rep(100, 2), lty=2, col="cornflowerblue")

    legend("topleft", c("Slope", "Lagtime (linear)", "Lagtime (acceleration)", "Time to 100%"),
           col=c("red", "burlywood", "burlywood4", "cornflowerblue"), pch=15, cex=1, bty="n")
  }

  if(type == "velocity") {
    plot(object$d$x, l5.velocity(object$d$x, object$coef), lwd=2, bty="n", type="l",
         xlab="Time", ylab="Velocity [%/hour]")

    #Plot slope
    points(object$slope$time, object$slope$slope, pch=15, col=1, cex=2)
    lines(rep(object$slope$time, 2), c(0, object$slope$slope), lty=2)
    lines(c(0, object$slope$time), rep(object$slope$slope, 2), lty=2)
  }

  if(type == "acceleration") {
    plot(object$d$x, l5.acceleration(object$d$x, object$coef), lwd=2, bty="n", type="l",
         xlab="Time", ylab="Acceleration [%/hour^2]")
    abline(h = 0, lty=2)
  }

  if(type == "residuals") {
    plot(object$d$x, object$residuals, pch=20, bty="n",
         xlab="Time", ylab="Standardised residuals",
         ylim=c(-ceiling(max(object$residuals)), ceiling(max(object$residuals))))
    abline(h = 0, lty=2)
    abline(h = -1.96, lty=3)
    abline(h = 1.96, lty=3)
    lines(lowess(object$d$x, object$residuals), lwd=2)
  }

  invisible(object)
}

