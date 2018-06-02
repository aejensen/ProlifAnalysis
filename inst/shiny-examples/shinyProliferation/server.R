shinyAppServer <- function(input, output, session){
  #d <- read.csv("180524 stat.txt", sep="\t", dec=",", skip=7)

  filedata <- reactive({
    infile <- input$file
    if (is.null(infile)) {
      return(NULL)
    }
    d <- read.csv(infile$datapath, sep="\t", dec=",", skip=7) ###here is some fileformat configurations!

    choiceList <- as.list(1:ncol(d))
    names(choiceList) <- colnames(d)
    shiny::updateSelectInput(session, "selectX", choices = choiceList)
    shiny::updateSelectInput(session, "select", choices = choiceList)

    d
  })

  rv <- shiny::reactiveValues(func=NULL)
  m <- shiny::reactiveValues(func=NULL)

  shiny::observeEvent(input$run, {
    rv$func <- input$cutoff

    d <- filedata()
    if (is.null(d)) {
      return(NULL)
    }

    data <- data.frame(x = d[, as.numeric(input$selectX)], y = d[, as.numeric(input$select)])
    data <- subset(data, x < as.numeric(input$cutoff))
    m$func <- ProlifAnalysis::estimateL5(data$x, data$y)
  })

  funcval <- shiny::reactive({
    input$cutoff
  })

  output$msg = shiny::renderPrint({
       if (is.null(rv$func)) return("not running")
       fv <- funcval()
       sprintf("%.3f, %.3f", rv$func, input$cutoff)
    })

  output$text <- shiny::renderPrint({
       if(is.null(m$func)) {
         cat("No model has been estimated yet.")
       } else {
         summary(m$func)
       }
   })

  output$distPlot <- plotly::renderPlotly({
    d <-filedata()
    if (is.null(d)) {
      return(NULL)
    }
    data <- data.frame(x = d[, as.numeric(input$selectX)], y = d[,as.numeric(input$select)])
    data <- subset(data, x < as.numeric(input$cutoff))
    plotly::plot_ly(data = data, x = ~x, y = ~y, type="scatter", mode = "markers") %>%  plotly::layout(xaxis = list(title="Time"), yaxis = list(title="Confluency [%]"))
  })

  output$plot2 <- renderPlot({
    if(!is.null(m$func)) {
      if(input$plotType == 1) {
        plot(m$func)
      } else if(input$plotType == 2) {
        plot(m$func, type="velocity")
      } else if(input$plotType == 3) {
        plot(m$func, type="acceleration")
      }
    }
  })

  shiny::observeEvent(input$about, {
    shinyalert::shinyalert(
      title = "About",
      text = "Shiny App for easy non-linear modeling of proliferation curves.<br><br>The author assumes no responsibility for the usage of this app.<br><br><br><br>Andreas Kryger Jensen.<br>Biostatistics, Institute of Public Health<br>University of Copenhagen.<br>2018",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "Close",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE)
  })
}

