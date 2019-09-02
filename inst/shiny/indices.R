
indices_server <- function(input, output, session, sentomeasures) {
  ns <- session$ns

  vals <- reactiveValues(
    measures = NULL,
    selectedIndex = NULL
  )

  observe({
    vals$measures <- sentomeasures()
  })

  output$selectIndex <- renderUI({

    if (is.null(sentomeasures())) {
      tags$p("Calculate sentiment first...")
    } else {
      selectizeInput(
        inputId = ns("select_index"),
        label = "Select an index to display",
        choices = as.list(colnamesMeasures()),
        multiple = TRUE
      )
    }
  })

  observe({
    vals$selectedIndex <- input$select_index
  })

  output$indexChart <- renderHighchart({
    xName <- c("date")
    names(xName) <- xName
    colnames <- c(colnamesMeasures()[c(vals$selectedIndex)], xName)
    if (length(colnames) > 1) {
      dataMelted <- melt(as.data.table(vals$measures[, colnames, with = FALSE]),
                         id = "date", value.name = "sentiment", variable.name = "PARAMS")
      dataMelted$date <- datetime_to_timestamp(dataMelted$date)
      hchart(dataMelted, type = 'line', hcaes(y = sentiment, group = PARAMS, x = date)) %>%
        hc_xAxis(type = "datetime")
    }
  })

  colnamesMeasures <- reactive({
    if (!is.null(sentomeasures())) {
      col <- colnames(vals$measures[, !"date"])
      names(col) <- col
      col
    }
  })
}

indices_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("selectIndex")),
    highchartOutput(ns("indexChart"))
  )

}

