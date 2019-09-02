
sentiment_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("sentimentUI"))

}

sentiment_server <- function(input, output, session, params, corpus, sentoLexicon, calculate) {

  vals <- reactiveValues(
    selectedColumn = NULL,
    sentomeasures = NULL,
    sentiment = NULL
  )

  output$downloadData <- downloadHandler(

    filename = function() paste("sentiment", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      write.csv(vals$sentiment, file, row.names = FALSE)
    }
  )

  output$downloadButtonConditional <- renderUI({
    downloadButton(session$ns("downloadData"), "Download")
  })

  output$sentimentUI <- renderUI({

    output$sentimentTable <- renderDataTable({
      tokeep <- which(sapply(vals$sentiment, is.numeric))
      cols <- colnames(vals$sentiment[, tokeep, with = FALSE])
      DT::datatable(vals$sentiment, options = list(searching = FALSE)) %>% formatRound(columns = cols, digits = 2)
    }, server = FALSE)


    output$sentimentChart <-  renderHighchart({
      colname <- colnamesSentiment()[vals$selectedColumn]
      y <- as.data.table(vals$sentiment[,colname, with = FALSE])
      if (length(names(y) > 0)) {
        names(y)<-"sentiment"
        y[is.na(y)] <- 0
        hc <- highchart() %>%
          hc_xAxis(categories = vals$sentiment$date) %>%
          hc_add_series(name = vals$selectedColumn, data = y$sentiment)
        hc
      }

    })

    output$selectSentiment <- renderUI({
      selectizeInput(
        inputId = session$ns("select_sentiment"),
        label = "Select a sentiment object to display",
        choices = as.list(colnamesSentiment()),
        selected = vals$selectedColumn,
        multiple = FALSE
      )
    })

    observe({
      vals$selectedColumn <- input$select_sentiment
    })

    fluidRow(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          style = "margin: 15px",
          title = "Table",
          tagList(
            dataTableOutput(session$ns("sentimentTable")),
            uiOutput(session$ns("downloadButtonConditional"))
          )
        ),
        tabPanel(
          style = "margin: 15px",
          title = "Graph",
          tagList(
            uiOutput(session$ns("selectSentiment")),
            highchartOutput(session$ns("sentimentChart")) %>% withSpinner(color = "#0dc5c1")
          )
        )
      )
    )
  })

  observeEvent(calculate, {
    if ("sento_corpus" %in% class(corpus())) {
      ctr <- sentometrics::ctr_agg( by ="month" , howWithin = params$how, howTime = c("equal_weight", "exponential"), lag = 4)
      measures<- sentometrics::sento_measures(corpus(), sentoLexicon(), ctr)
      vals$sentomeasures <- measures
      vals$sentiment <- measures$sentiment
      measures
    } else {
      sentiment <- compute_sentiment(corpus(), sentoLexicon(), params$how)
      vals$sentiment <- sentiment
      vals$sentomeasures <- NULL
      return(NULL)
    }
  })

  colnamesSentiment <- reactive({
    sent <- vals$sentiment
    if ("id" %in% colnames(sent)) {
      sent[, id := NULL]
    }
    if ("date" %in% colnames(sent)) {
      sent[, date := NULL]
    }
    col <- colnames(sent)
    names(col) <- col
    col
  })

  return(vals)
}

