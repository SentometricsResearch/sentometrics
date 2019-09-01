indices_server<- function(input, output, session, sentomeasures) {
  ns <- session$ns

  vals <- reactiveValues(
    measures = NULL,
    selectedIndex = NULL
  )

  observe({
    vals$measures <- sentomeasures()
  })

  output$selectIndex<- renderUI({
    if(is.null(sentomeasures())) {
      tags$p("Calculate sentiment first..")
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

  output$indexChart<-  renderHighchart({

    colnames <- colnamesMeasures()[vals$selectedIndex]
    colnames <- c(colnames, "date")
    if(length(colnames) >1) {
      y <- as.data.table(vals$measures[,colnames, with=FALSE])
      x<- as.data.table(vals$measures$date)
      names(x) <- "date"
      data <- merge(y, x)
      dataMelted <- melt(data, id=c("date"), value.name="VALUES", variable.name="PARAMS")
      dataMelted$date<- datetime_to_timestamp(dataMelted$date)
      hchart(dataMelted, type = 'line', hcaes(y = VALUES, group = PARAMS, x = date)) %>%
        hc_xAxis(type = "datetime")

    }



  })

  colnamesMeasures <-reactive({
    if(!is.null(sentomeasures())) {
      col <- colnames(vals$measures[,!"date"])
      names(col) <- col
      col
    }

  })
}

indices_ui <- function(id) {
  ns <-NS(id)
  tagList(
    uiOutput(ns("selectIndex"))  %>% withSpinner(color = "#0dc5c1"),
    highchartOutput(ns("indexChart")) %>% withSpinner(color = "#0dc5c1")

    )

}
