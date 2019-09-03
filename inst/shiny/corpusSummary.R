
corpus_summary_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("corpusSummary"))
}

corpus_summary_server <- function(input, output, session, corpus) {
  ns <- session$ns

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
    DT::datatable(corpusSummary()$stats, options = list(searching = FALSE)) %>%
      formatRound(columns = cols, digits = 0)
  }, server = FALSE)

  output$downloadCorpusSummary <- downloadHandler(
    filename = function() paste("corpus_summary_stats", Sys.Date(), ".csv", sep = ""),
    content = function(con) {
      write.csv(corpusSummary()$stats, con)
    },
    contentType = "text/csv"
  )

  output$featurePlot <- renderPlot({
    corpusSummary()$plots$feature_plot
  })

  output$tokenPlot <- renderPlot({
    corpusSummary()$plots$token_plot
  })

  output$docPlot <- renderPlot({
    corpusSummary()$plots$doc_plot
  })

  observeEvent(input$selectSummaryFrequency, {
    myvals$frequency <- input$selectSummaryFrequency
  })

  output$corpusSummary <- renderUI({

  if ("sento_corpus" %in% class(corpus())) {

    fluidRow(
      tags$div(
        style = "margin-bottom: 15px",
        selectizeInput(
          inputId = ns("selectSummaryFrequency"),
          label = "Select frequency",
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
          plotOutput(ns("docPlot")) %>% withSpinner(color = "#0dc5c1")
        ),
        tabPanel(
          style = "margin: 15px",
          title = "Tokens",
          plotOutput(ns("tokenPlot")) %>% withSpinner(color = "#0dc5c1")
        ),
        tabPanel(
          style = "margin: 15px",
          title = "Features",
          plotOutput(ns("featurePlot")) %>% withSpinner(color = "#0dc5c1")
        ),
        tabPanel(
          style = "margin: 15px",
          title = "Statistics",
          div(dataTableOutput(ns("summaryStatsTable")) %>%
            withSpinner(color = "#0dc5c1"), style = "font-size:80%"),
          downloadButton(ns("downloadCorpusSummary"), "Download corpus statistics")
        )
      )
    )

  } else {
    tags$p("A sento_corpus object with the columns 'id', 'date' and 'texts' is needed for the summary.")
  }

  })
}

