
library("shiny")
library("shinyWidgets")
library("shinythemes")
library("DT")
library("reactlog")
library("shinycssloaders")
library("sentometrics")
library("quanteda")

options(shiny.reactlog = TRUE)
options(encoding = "UTF-8")

# Define UI for data upload app ----
ui <- fluidPage(theme = "cerulean",

  tags$head(
    tags$style(
      HTML(
        "
        #inputs-table {
        border-collapse: collapse;
        }

        #inputs-table td {
        padding: 5px;

        }


        "
      ) #/ HTML
    ) #/ style
  ), #/ head

  # App title ----


  # Sidebar layout with input and output definitions ----
    # Sidebar panel for inputs ----


    sidebarPanel(
      style = "margin: 15px",
      tags$h4(
        style = "align-text: center",
        "Upload Corpus"
      ),
      tags$table(
        id = "inputs-table",
        style = "width: 100%",
        tags$tr(
          tags$td(
            style = "width: 90%",
            uiOutput("uploadCorpus")
          ),
          tags$td(
            style = "width: 10%; ",
            div(class = "form-group shiny-input-container"
                , actionButton(
                  inputId = "corpusHelpButton",
                  label = NULL,
                  icon = icon("question")
                )

            )
          )
        )
      )
     ,
    hr(),
    uiOutput("lexiconUI"),
    uiOutput("useValenceUI"),
    uiOutput("valenceUI"),
    hr(),
    uiOutput("howUI"),
    uiOutput("calculateSentimentButton")

    ),

    mainPanel(

       uiOutput("mainPanelContent")

    )


  # )
  #
  # ,
  #
  # # Main panel for displaying outputs ----
  #
  # # Output: Data file ----

  #
  #
)


# Define server logic to read selected file ----
data("list_lexicons", package = "sentometrics")
data("list_valence_shifters", package = "sentometrics")
options(shiny.maxRequestSize = 30*1024^2)


server <- function(input, output, session) {

  reactiveValues <- reactiveValues()
  reactiveValues$lexiconList <- list_lexicons
  reactiveValues$valenceShifterList <- list_valence_shifters
  reactiveValues$summFreq <- "day"
  reactiveValues$valenceMethodChoices <- c("Bigram", "Cluster")
  reactiveValues$valenceMethodSelected <- "Bigram"
  reactiveValues$valenceShifterSelected <- names(list_valence_shifters[1])
  reactiveValues$lexiconsSelected <- NULL
  reactiveValues$howSelected <- "counts"
  reactiveValues$buttonClicks <- 0


  corpusFileData <- reactive({
    withProgress(message = 'Loading File', {
    req(input$corpusFile)
    df<- read.csv(input$corpusFile$datapath,
                   header = TRUE,
                   sep = ";",
                   quote = '"',
                   fileEncoding="UTF-8")

    df
    })
  })

  isCorpus <- reactive({
    req(corpusFileData())

    withProgress(message = 'Validating Corpus', {
      df <- corpusFileData()

      if (all(c("texts", "id", "date") %in% colnames(df))) {
       TRUE
      } else {

       FALSE
      }
    })
  })

  corpus <- reactive({
    withProgress(message = 'Creating Corpus', {
      df <- corpusFileData()
      incProgress(0.5)
      if (isCorpus()) {
        df$texts <- as.character(df$texts)
        corp <- sentometrics::sento_corpus(df)
        incProgress(0.5)
      } else {
        corp <- as.character(df$texts)
      }
      corp
    })
  })

  lexiconFileData <- reactive({
    req(input$lexiconFile)
    df <- read.csv(input$lexiconFile$datapath,
                   header = TRUE,
                   sep = ";",
                   quote = "'",
                   fileEncoding="UTF-8-BOM")
    df
  })

  isLexicon <- reactive({
    req(lexiconFileData())
    withProgress(message = 'Validating Lexicon', {
      df <- lexiconFileData()
      if (all(c("x", "y") %in% colnames(df))) {
        TRUE
      } else {
        FALSE
      }
    })
  })

  lexicon <- reactive({

    req(input$selectLexicons)

    valenceShiftersIn <- NULL

    if (input$useValenceCheck) {
        req(input$selectValenceShifters)
        req(input$valenceMethodSwitch)
        valenceShiftersIn <- data.table::as.data.table(reactiveValues$valenceShifterList[[input$selectValenceShifters]])
        if (reactiveValues$valenceMethodSelected == "Bigram") {
          valenceShiftersIn <- valenceShiftersIn[,.(x, y)]
        } else {
          valenceShiftersIn <- valenceShiftersIn[,.(x, t)]
        }

    }

    lexiconsIn <- c(reactiveValues$lexiconList[input$selectLexicons])
    if (is.null(valenceShiftersIn)) {
      lex <- sento_lexicons(lexiconsIn = lexiconsIn)
    } else {
      lex <- sento_lexicons(lexiconsIn = lexiconsIn, valenceIn = valenceShiftersIn)
    }
    lex

  })

  valenceShifterFileData <- reactive({
    req(input$valenceShifterFile)
    df <- read.csv(input$valenceShifterFile$datapath,
                   header = TRUE,
                   sep = ";",
                   quote = '"',
                   fileEncoding="UTF-8")
    df
  })

  isValenceShifter <- reactive({
    req(valenceShifterFileData())
    withProgress(message = 'Validating valence shifters', {
      df <- valenceShifterFileData()
      if (all(c("x", "y") %in% colnames(df)) && !"t" %in% colnames(df) ) {
        TRUE
      } else if (all(c("x", "t") %in% colnames(df)) && !"y" %in% colnames(df) ) {
        TRUE
      } else {
        FALSE
      }
    })
  })


  corpusSummary <- reactive({
    req(corpus)

    withProgress(message = 'Summarizing Corpus', {
      summ <-sentometrics::corpus_summarize(corpus(), by =reactiveValues$summFreq)
    })
  })


  output$contents <- renderUI({
    req(corpusFileData())

    withProgress(message = 'Loading Corpus', {
      colNumTexts <- grep("texts", colnames(corpusFileData()))

      output$corpusTable <- renderDataTable(corpusFileData(), options = list(
        pageLength = 5,
        searching = TRUE,
        lengthMenu = c( 5, 10, 15, 20),
        columnDefs = list(list(
          targets = colNumTexts,
          render = JS(
             "function(data, type, row, meta) {",
             "return data.length > 6 ?",
             "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
             "}")
        ))),
        callback = JS("table.page(3).draw(false); table.on('click.dt', 'td', function() {
            var row_=table.cell(this).index().row;

            var data = [row_];
           Shiny.onInputChange('rows',data );
    });"
        )
      )
    })

    fluidRow(
      dataTableOutput("corpusTable")
    )
  })


  output$lexicon <- renderTable({
    head(lexiconFileData())
  })

  output$sentimentUI <- renderUI({
      output$sentimentTable <- renderDataTable(sentiment(), options = list(searching = FALSE))
      fluidRow(
        dataTableOutput("sentimentTable") %>% withSpinner(color="#0dc5c1"),
        uiOutput("downloadButtonConditional")
      )

  })



  output$corpusSummary <- renderUI({
    req(corpusSummary())
    fluidRow(
      tags$div(
        style = "margin-bottom: 15px",
        selectizeInput(
          inputId ="selectSummaryFrequency",
          label = "Select the frequency of the summary",
          choices = c("day", "week", "month","year"),
          selected = reactiveValues$summFreq,
          multiple = FALSE
        )
      ),
      style = "margin: 15px",
      tabsetPanel(
        tabPanel(
          style = "margin: 15px",
          title = "Documents",
            plotOutput("docPlot")  %>% withSpinner(color="#0dc5c1")
        ),
        tabPanel(
          style = "margin: 15px",
          title = "Tokens",
          plotOutput("tokenPlot")  %>% withSpinner(color="#0dc5c1")
        ),
        tabPanel(
          style = "margin: 15px",
          title = "Features",
          plotOutput("featurePlot")  %>% withSpinner(color="#0dc5c1")
        ),
        tabPanel(
          style = "margin: 15px",
          title = "Stats",
          dataTableOutput('summaryStatsTable')  %>% withSpinner(color="#0dc5c1"),
          downloadButton("downloadCorpusSummary", "Download Corpus Stats")
        )
      )
    )
  })

  output$summaryStatsTable <- renderDataTable(corpusSummary()$stats)

  output$downloadCorpusSummary <- downloadHandler(
      filename = function(){ paste('corpus_summary_stats', Sys.Date(), '.csv', sep= '')},
      content = function(con) {
        write.csv(corpusSummary()$stats, con)
      },
      contentType = "text/csv"

  )

  observeEvent(input$selectSummaryFrequency,{

    reactiveValues$summFreq <- input$selectSummaryFrequency
  })

  output$featurePlot <- renderPlot({
    req(corpusSummary())
    withProgress(message = 'Drawing feature plot', {
      corpusSummary()$plots$feature_plot
    })
  })

  output$tokenPlot <-renderPlot({
    req(corpusSummary())
    withProgress(message = 'Drawing token plot', {
      corpusSummary()$plots$token_plot
    })
  })

  output$docPlot <- renderPlot({
    req(corpusSummary())
    withProgress(message = 'Drawing document plot', {
      corpusSummary()$plots$doc_plot
    })
  })

  output$uploadCorpus <- renderUI({
    fileInput(
      inputId = "corpusFile",
      label = "Choose CSV File",
      multiple = FALSE,
      accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")
    )

  })


  output$downloadData <- downloadHandler(

    filename = function(){paste("sentiment", Sys.Date(), ".csv", sep= "")},
    content = function(file) {
      write.csv(sentiment(), file, row.names = FALSE)
    }
  )

  output$downloadButtonConditional <- renderUI({
    req(sentiment())
    downloadButton("downloadData", "Download")
  })

  output$howUI <- renderUI({
    req(lexicon())
    if (input$useValenceCheck) {
      req(input$selectValenceShifters)
    }
    tags$table(
      id = "inputs-table",
      style = "width: 100%",
      tags$tr(
        tags$td(
          tags$h4(
            style = "align-text: center",
            "Choose within document weighting method"
          )
        )
      ),
      tags$tr(
        tags$td(
          selectInput(
            inputId ="selectHow",
            label = "Select within document aggregation",
            choices = sentometrics::get_hows()$words,
            selected = reactiveValues$howSelected,
            multiple = FALSE
          )
        )
      )
    )
  })

  output$lexiconUI <- renderUI({
    req(corpus())
    tags$table(
      id = "inputs-table",
      style = "width: 100%",
      tags$tr(
        tags$td(
          tags$h4(
            style = "align-text: center",
            "Choose Lexicon"
          )
        )
      ),
      tags$tr(
        tags$td(
          materialSwitch(
            inputId = "uploadLexiconsSwitch",
            value = FALSE,
            label = "Upload Lexicon",
            status = "success"
          )
        )
      ),
      tags$tr(
        tags$td(
          uiOutput("selectLexiconsConditional")
        )
      )
    )

  })

  output$useValenceUI <- renderUI({
    req(corpus())
    tags$table(
      id = "inputs-table",
      style = "width: 100%",
      tags$tr(
        tags$td(
          checkboxInput(
            inputId="useValenceCheck",
            label ="Use valence Shifters",
            FALSE
          )
        )
      )
    )
  })

  output$valenceUI<- renderUI({
    req(input$useValenceCheck)
    if(input$useValenceCheck) {
      tags$table(
        id = "inputs-table",
        style = "width: 100%",
        tags$tr(
          tags$td(
            tags$h4(
              style = "align-text: center",
              "Choose Valence Shifter"
            )
          )
        ),
        tags$tr(
          tags$td(
            materialSwitch(
              inputId = "uploadValenceShiftersSwitch",
              value = FALSE,
              label = "Upload Valence Shifters",
              status = "success"
            )
          )
        ),
        tags$tr(
          tags$td(
            uiOutput("selectValenceShiftersConditional")
          )
        )
      )
    }
  })

  output$selectValenceShiftersConditional <- renderUI({
    if(!input$uploadValenceShiftersSwitch) {

      tags$table(
        id = "inputs-table",
        style = "width: 100%",

        tags$tr(
          tags$td(
            selectizeInput(
              inputId ="selectValenceShifters",
              label = "Select valence Shifters from list",
              choices = names(reactiveValues$valenceShifterList),
              selected = reactiveValues$valenceShifterSelected,
              multiple = FALSE
            )
          )
        ),
        tags$tr(
          tags$td(
            style = "width: 90%",
            sliderTextInput(
              inputId = "valenceMethodSwitch",
              label = "Method",
              grid = FALSE,
              force_edges = FALSE,
              choices = reactiveValues$valenceMethodChoices
            )
          ),
          tags$td(
            style = "width: 10%; ",
            div(class = "form-group shiny-input-container",
                style = "margin-top: 10%;"
                , actionButton(
                  inputId = "valenceMethodHelpButton",
                  label = NULL,
                  icon = icon("question")
                )
            )
          )
        )
      )

    } else {

      tags$table(
        id = "inputs-table",
        style = "width: 100%",
        tags$tr(
          tags$td(
            style = "width: 90%",
            fileInput(
              inputId = "valenceShifterFile",
              label = "Choose CSV File",
              multiple = TRUE,
              accept = c("text/csv", "text/comma-separated-values, text/plain",".csv")
            )
          ),
          tags$td(
            style = "width: 10%; ",
            div(class = "form-group shiny-input-container"
                , actionButton(
                  inputId = "valenceShifterHelpButton",
                  label = NULL,
                  icon = icon("question")
                )
            )
          )
        )
      )
    }

  })

  observeEvent(input$selectHow,{

    reactiveValues$howSelected <-input$selectHow
  })

  observeEvent(input$selectValenceShifters, {
    req(input$selectValenceShifters)
    colnames <- names(reactiveValues$valenceShifterList[[input$selectValenceShifters]])
    reactiveValues$valenceShifterSelected <- input$selectValenceShifters
    if(all(c( "y","t") %in% colnames)) {
      reactiveValues$valenceMethodChoices <- c("Bigram", "Cluster")
      reactiveValues$valenceMethodSelected <- "Bigram"
    } else if("y" %in% colnames) {
      reactiveValues$valenceMethodChoices <- c("Bigram")
      reactiveValues$valenceMethodSelected <- "Bigram"
    } else {
      reactiveValues$valenceMethodChoices <- c("Cluster")
      reactiveValues$valenceMethodSelected <- "Cluster"
    }

   #
  })

  output$selectLexiconsConditional <- renderUI({

    if(!input$uploadLexiconsSwitch) {
     choices <- as.list(iconv(names(reactiveValues$lexiconList), "LATIN2", "UTF-8"))


      selectizeInput(
          inputId ="selectLexicons",
          label = "Select lexicons from list",
          choices = choices,#,
          selected = reactiveValues$lexiconsSelected,
          multiple = TRUE
        )

    } else {

        tags$table(
          id = "inputs-table",
          style = "width: 100%",
          tags$tr(
            tags$td(
              style = "width: 90%",
                fileInput(
                inputId = "lexiconFile",
                label = "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")
              )
            ),
            tags$td(
              style = "width: 10%; ",
              div(class = "form-group shiny-input-container"
                  , actionButton(
                    inputId = "lexiconHelpButton",
                    label = NULL,
                    icon = icon("question")
                  )
              )
            )
          )
      )
    }
  })

  lexiconFileName <- reactive({
    if (!is.null(input$lexiconFile$datapath)) {
      # Extract file name (additionally remove file extension using sub)
      return(sub(".csv$", "", basename(input$lexiconFile$name)))
    } else {
      return(NULL)
    }
  })

  valenceShifterFileName <- reactive({

    if (!is.null(input$valenceShifterFile$datapath)) {
      return(sub(".csv$", "", basename(input$valenceShifterFile$name)))
    } else {
      return(NULL)
    }
  })

  output$calculateSentimentButton <- renderUI({

    actionButton(
      inputId = "calcSentimentButton",
      label = "Calculate Sentiment",
      icon = icon("rocket")
    )
  })

  output$mainPanelContent<- renderUI({
    req(corpus())
    fluidRow(
      style = "margin: 15px",
      tabsetPanel(
       id = "tabs",
                  tabPanel(
                    style = "margin: 15px",
                    title = "Corpus",
                    tableOutput("contents")
                  ),
                  tabPanel(
                    "Sentiment",
                    uiOutput("sentimentUI")
                  ),
                  tabPanel(
                    "Corpus Summary",
                    uiOutput("corpusSummary")
                  )
      )
    )
  })



  sentiment <- eventReactive(input$calcSentimentButton ,{
    isolate({
      req(lexicon())
      req(input$selectHow)
      req(lexicon)
    })

      lexicon <- isolate({lexicon()})
      how <- isolate({input$selectHow})
      corpus <- isolate({corpus()})
      withProgress(message = 'Calculating sentiment', {
        sentometrics::compute_sentiment(corpus, lexicon, how = how)
      })

  })

  observeEvent(input$uploadLexiconsSwitch, {
    reactiveValues$lexiconsSelected <- input$selectLexicons
  })


  observeEvent(input$calcSentimentButton,{
    updateTabsetPanel(session, "tabs", selected = "Sentiment")

   })

  observeEvent(input$lexiconFile,{
    #req(isLexicon())
    if (!isLexicon()) {
      showModal(modalDialog(
        title = "Error",
        paste("Invalid lexicon file.")
      ))
    } else {
      existingLexiconsNames <- names(reactiveValues$lexiconList)
      newLexiconFileName <- lexiconFileName()
      if (newLexiconFileName %in% existingLexiconsNames ){
        showModal(modalDialog(
          title = "Warning",
          paste("A lexicon with the name: '" , newLexiconFileName , "' already exists.")
        ))
      } else {
        x <- list( data.table::data.table(lexiconFileData()))
        names(x)<- newLexiconFileName
        reactiveValues$lexiconList <- c(reactiveValues$lexiconList, x )
        showModal(modalDialog(
          title = "Info",
          paste("The lexicon '" , newLexiconFileName , "' is added to the list of lexicons.")
        ))
        updateSwitchInput(session = session,
                          inputId = "uploadLexiconsSwitch",
                          value = FALSE)
      }
    }
  })

  observeEvent(input$valenceShifterFile,{

    if (!isValenceShifter()) {
      showModal(modalDialog(
        title = "Error",
        paste("Invalid valence shifter file.")
      ))
    } else {
      existingValenceShifterNames <- names(reactiveValues$valenceShifterList)
      newValenceShifterFileName <- valenceShifterFileName()

      if (newValenceShifterFileName %in% existingValenceShifterNames){
        showModal(modalDialog(
          title = "Warning",
          paste("A set of valence shifters with the name: '" , newValenceShifterFileName , "' already exists.")
        ))
      } else {
        x <- list( data.table::data.table(valenceShifterFileData()))
        names(x)<- newValenceShifterFileName
        reactiveValues$valenceShifterList <- c(reactiveValues$valenceShifterList , x )
        showModal(modalDialog(
          title = "Info",
          paste("The set of valence shifters '" , newValenceShifterFileName , "' is added to the list of valence shifters.")
        ))
        updateSwitchInput(session = session,
                          inputId = "uploadValenceShiftersSwitch",
                          value = FALSE)
      }
    }
  })

  observeEvent(input$corpusHelpButton, {
    showModal(modalDialog(
      title = "Upload a corpus",
      "The CSV file should at a very minimun contain a header named 'texts'.
      However, it is recommended to have an 'id' and 'date' column to it possible
      for the system to create a sentocorpus. The file can also contain additional columns for (numeric) features."
    ))
  })

  observeEvent(input$valenceMethodHelpButton, {
    showModal(modalDialog(
      title = "Valence Shifting Method",
      "If both the columns 'y' and 't' are delivered, you need to choose between the bigram or the cluster approach.
      For the bigram approach column 'y' is used. For the cluster approach column 't' is used.
      If both columns are not delivered, only one of the two options will be available. "
    ))
  })

  observeEvent(input$lexiconHelpButton, {
    showModal(modalDialog(
      title = "Upload a lexicon",
      "The CSV file should contain two headers named 'x' and 'y'. Only one lexicon can be uploaded at the same time.
      Once you have uploaded the file, the lexicon will be available in the predefined list. The name of your lexicon will be the filename
      of the uploaded lexicon."
    ))
  })

  observeEvent(input$valenceShifterHelpButton, {
    showModal(modalDialog(
      title = "Upload valence shiters",
      "The CSV file should contain two headers named 'x' and 'y' or 'x' and 't'. Only one set of valence shifters can be uploaded at the same time.
      Once you have uploaded the file, the set of valence shifters will be available in the predefined list. The name of the set of valence shifters will
      be the filename of the uploaded set of valence shifters."
    ))
  })

}
# Run the app ----
shinyApp(ui, server)
