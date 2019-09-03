
load_corpus_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h4(
      style = "align-text: center",
      "Upload corpus"
    ),
    tags$table(
      id = "inputs-table",
      style = "width: 100%",
      tags$tr(
        tags$td(
          style = "width: 90%",
          fileInput(
            inputId = ns("corpusUpload"),
            label = "Choose .csv file",
            multiple = FALSE,
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
          )
        ),
        tags$td(
          style = "width: 10%; ",
          div(class = "form-group shiny-input-container"
              , actionButton(
                inputId = ns("corpusHelpButton"),
                label = NULL,
                icon = icon("question")
              )
          )
        )
      )
    )
  )
}

load_corpus_server <- function(input, output, session) {

  corpusFile <- reactiveVal(as.data.table(usnews))

  observeEvent(input$corpusUpload, ignoreNULL = TRUE, ignoreInit = TRUE, {

    df <- as.data.table(
      read.csv(input$corpusUpload$datapath,
               header = TRUE,
               sep = ";",
               quote = '"',
               fileEncoding = "UTF-8",
               stringsAsFactors = FALSE)
    )

    w <- match(c("id", "date", "texts"), names(df), nomatch = 0)
    setcolorder(df, colnames(df)[w])

    if ("texts" %in% colnames(df)) {
      corpusFile(df)
    } else {
      showModal(modalDialog(
        title = "Error",
       "No column 'texts' found. Please upload a valid file."
      ))
    }
  })

  observeEvent(input$corpusHelpButton, {
    showModal(modalDialog(
      title = "Upload a corpus",
      "The .csv file should at a very minimun contain a header named 'texts'. However, it is recommended
      to also have an 'id' and a properly formatted 'date' column so a sento_corpus can be created. The
      file can also contain additional columns for (numeric) features. Use ';' for the separation of
      columns in the file."
    ))
  })

  return(corpusFile)
}

render_corpus_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(DT::dataTableOutput(ns("corpus_table")), style = "font-size:80%")
  )
}

render_corpus_server <- function(input, output, session, corpusFile) {

  colNumTexts <- reactive({
    grep("texts", colnames(corpusFile()))
  })

  output$corpus_table <- DT::renderDataTable({

    corp <- corpusFile()
    cols <- colnames(corp[, sapply(corp,is.numeric), with = FALSE])

    DT::datatable(corp, options = list(
      pageLength = 10,
      lengthMenu = c(5, 10, 15, 20),
      columnDefs = list(list(
        targets = colNumTexts(),
        render = JS(
          "function(data, type, row, meta) {",
          "return data.length > 6 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
          "}")
      ))),
      callback = JS("table.page(3).draw(false); "
      )
    ) %>% formatRound(columns = cols, digits = 2)
  }, server = FALSE)

}

create_corpus_server <- function(input, output, session, corpusFile) {
  corpusData <- reactive({
    df <- corpusFile()
    if (all(c("texts", "id", "date") %in% colnames(df))) {
      df$id <- as.character(df$id)
      df$date <- as.character(df$date)
      df$texts <- as.character(df$texts)
      corp <- sentometrics::sento_corpus(df)
    } else {
      corp <- as.character(df$texts)
    }
  })
}

