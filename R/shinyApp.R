runShinyProliferation <- function() {
  appDir <- system.file("shiny-examples", "shinyProliferation", package = "ProlifAnalysis")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
