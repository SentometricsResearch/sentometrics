
corpus_summary_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("corpusSummary"))
}

corpus_summary_server <- function(input, output, session, corpus) {

  myvals <- reactiveValues(
    frequency = "day"
  )

  corpusSummary <- reactive({
    req(corpus())
    sentometrics::corpus_summarize(corpus(), by = myvals$frequency)
  })

  output$summaryStatsTable <- renderDataTable({
    tokeep <- which(sapply(corpusSummary()$stats, is.numeric))
    cols <- colnames(corpusSummary()$stats[, tokeep, with = FALSE])
    DT::datatable(corpusSummary()$stats, server = FALSE, options = list(searching = FALSE)) %>% formatRound(columns = cols, digits = 0)
  })

  output$downloadCorpusSummary <- downloadHandler(
    filename = function(){ paste('corpus_summary_stats', Sys.Date(), '.csv', sep = '')},
    content = function(con) {
      write.csv(corpusSummary()$stats, con)
    },
    contentType = "text/csv"
  )

  output$featurePlot <- renderPlot({
    withProgress(message = 'Drawing feature plot', {
      corpusSummary()$plots$feature_plot
    })
  })

  output$tokenPlot <- renderPlot({
    withProgress(message = 'Drawing token plot', {
      corpusSummary()$plots$token_plot
    })
  })

  output$docPlot <- renderPlot({
    withProgress(message = 'Drawing document plot', {
      corpusSummary()$plots$doc_plot
    })
  })

  observeEvent(input$selectSummaryFrequency, {
    myvals$frequency <- input$selectSummaryFrequency
  })

  output$corpusSummary <- renderUI({

  if ("sentocorpus" %in% class(corpus())) {

    fluidRow(
      tags$div(
        style = "margin-bottom: 15px",
        selectizeInput(
          inputId = session$ns("selectSummaryFrequency"),
          label = "Select the frequency of the summary",
          choices = c("day", "week", "month", "year"),
          selected = "day",
          multiple = FALSE
        )
      ),
      style = "margin: 15px",
      tabsetPanel(
        tabPanel(
          style = "margin: 15px",
          title = "Documents",
          plotOutput(session$ns("docPlot")) %>% withSpinner(color = "#0dc5c1")
        ),
        tabPanel(
          style = "margin: 15px",
          title = "Tokens",
          plotOutput(session$ns("tokenPlot")) %>% withSpinner(color = "#0dc5c1")
        ),
        tabPanel(
          style = "margin: 15px",
          title = "Features",
          plotOutput(session$ns("featurePlot")) %>% withSpinner(color = "#0dc5c1")
        ),
        tabPanel(
          style = "margin: 15px",
          title = "Stats",
          dataTableOutput(session$ns('summaryStatsTable')) %>% withSpinner(color = "#0dc5c1"),
          downloadButton(session$ns("downloadCorpusSummary"), "Download Corpus Stats")
        )
      )
    )

  } else {
   tags$p("A sentocorpus with the columns 'id', 'date' and 'texts' is needed for the summary.")
  }

  })

}

