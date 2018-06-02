shinyAppUI <- shiny::fluidPage(
  shinyalert::useShinyalert(),

  shiny::titlePanel(shiny::h1("Proliferation Analysis")),

  shiny::sidebarLayout(

    sidebarPanel(
      fileInput("file", h3("Choose file"), multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),

      shiny::selectInput("selectX", label = h3("Time variable"), choices = list("Need to load a file" = 1), selected = 4),

      shiny::selectInput("select", label = h3("Response variable"), choices = list("Need to load a file" = 1), selected = 4),

      shiny::div(style="text-align: center;", shinyWidgets::knobInput(inputId = "cutoff", label =  h3("Time cutoff:"), value = 140, min = 0, max = 160,
        displayPrevious = FALSE, lineCap = "round", fgColor = "#428BCA", inputColor = "#428BCA", thickness = 0.2)),

      shiny::div(style="text-align: center;", shinyWidgets::actionBttn(inputId = "run", label = "Estimate!", style = "material-flat", color = "primary", icon = icon("thumbs-up"))),

      shiny::div(style="text-align: center;", shinyWidgets::radioGroupButtons("plotType", label = h3("Output plot type"),
                   choices = list("Estimate" = 1, "Velocity" = 2, "Acceleration" = 3), selected = 1, status="primary")),

     hr(),

     shiny::div(style="text-align: center;", shinyWidgets::actionBttn(inputId = "about", label = "About", style = "minimal", color = "primary"))
    ),

    mainPanel(
      shiny::h3("Selected data"),
      plotly::plotlyOutput(outputId = "distPlot"),
      shiny::h3("Results"),
      shiny::verbatimTextOutput("text"),
      shiny::plotOutput(outputId = "plot2", height="600px")
    ),
  )
)
