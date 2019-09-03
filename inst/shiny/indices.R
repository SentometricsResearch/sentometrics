
indices_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("selectIndex")),
    plotOutput(ns("indexPlot")) %>% withSpinner(color = "#0dc5c1")
  )
}

indices_server <- function(input, output, session, sento_measures) {
  ns <- session$ns

  vals <- reactiveValues(
    sento_measures = NULL,
    selectedIndex = NULL
  )

  observe({
    vals$sento_measures <- sento_measures()
  })

  output$selectIndex <- renderUI({

    if (is.null(sento_measures())) {
      tags$p("Calculate sentiment first...")
    } else {
      selectizeInput(
        inputId = ns("select_index"),
        label = "Select a sentiment measure to display",
        choices = as.list(colnamesMeasures()),
        multiple = TRUE
      )
    }
  })

  observe({
    vals$selectedIndex <- input$select_index
  })

  output$indexPlot <- renderPlot({
    sel <- as.list(stringi::stri_split(vals$selectedIndex, regex = "--"))
    sm <- subset(vals$sento_measures, select = sel)
    plot(sm)
  })

  colnamesMeasures <- reactive({
    if (!is.null(sento_measures())) {
      col <- colnames(as.data.table(vals$sento_measures)[, !"date"])
      names(col) <- col
      col
    }
  })
}

