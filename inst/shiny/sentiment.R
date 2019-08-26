
sentiment_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("sentimentUI"))
}

sentiment_server <- function(input, output, session, params, corpus, sentoLexicon, calculate) {

  output$downloadData <- downloadHandler(

    filename = function(){paste("sentiment", Sys.Date(), ".csv", sep = "")},
    content = function(file) {
      write.csv(sentiment(), file, row.names = FALSE)
    }
  )

  output$downloadButtonConditional <- renderUI({
    downloadButton(session$ns("downloadData"), "Download")
  })

  output$sentimentUI <- renderUI({

    output$sentimentTable <- renderDataTable({
      tokeep <- which(sapply(sentiment(), is.numeric))
      cols <- colnames(sentiment()[, tokeep, with=FALSE])
      DT::datatable(sentiment(), options = list(searching = FALSE,server = FALSE)) %>% formatRound(columns = cols, digits = 2)
    })

    fluidRow(
      dataTableOutput(session$ns("sentimentTable")),
      uiOutput(session$ns("downloadButtonConditional"))
    )

  })

  sentiment <- reactive({
   calculate
    isolate({
      how <- params$how
      s <-  sentometrics::compute_sentiment(corpus(), sentoLexicon(), how = how)
     s
    })
  })

}

