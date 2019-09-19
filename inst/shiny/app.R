
library("shiny")
library("shinyWidgets")
library("shinythemes")
library("shinycssloaders")
library("DT")

library("sentometrics")
library("quanteda")

source("corpus.R")
source("lexicon.R")
source("how.R")
source("valence.R")
source("corpusSummary.R")
source("sentiment.R")
source("sento_lexicon.R")
source("indices.R")

data("list_lexicons", package = "sentometrics")
data("list_valence_shifters", package = "sentometrics")
options(scipen = 999)
options(shiny.maxRequestSize = 50*1024^2)

ui <- fluidPage(
        theme = shinytheme("cerulean"),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "css.css")
    ),
    sidebarPanel(
        style = "margin: 15px",
        load_corpus_ui("load_corpus_csv"),
        lexicon_ui("lexicon_ui"),
        valence_ui("valence_ui"),
        how_ui("how_ui"),
        uiOutput("calculateSentimentButton")
    ),
    mainPanel(
        fluidRow(
            style = "margin: 15px",
            tabsetPanel(
                id = "tabs",
                tabPanel(
                    style = "margin: 15px",
                    title = "Corpus",
                    render_corpus_ui("corpus_table")
                ),
                tabPanel(
                    style = "margin: 15px",
                    title = "Corpus summary",
                    corpus_summary_ui("corpus_summary_ui")
                ),
                tabPanel(
                    style = "margin: 15px",
                    title = "Sentiment",
                    sentiment_ui("sentiment_ui"),
                    value ="sentimentTab"
                ),
                tabPanel(
                    style = "margin: 15px",
                    title = "Indices",
                    indices_ui("indices_ui"),
                    value= "indicesTab"
                )
            )
        )
    )
)

myvals <- reactiveValues(
    selectedLexicons = NULL,
    selectedValence = NULL,
    useValence = FALSE,
    lexiconList = list_lexicons,
    valenceList = list_valence_shifters,
    how = NULL,
    valenceMethod = "Bigram",
    sento_measures = NULL,
    sentiment = NULL
)

server <- function(input, output, session) {

    observe({
        if (is.null(myvals$sento_measures)) {
            hideTab(inputId = "tabs", target = "indicesTab")
        } else {
            showTab(inputId = "tabs", target = "indicesTab")
        }
    })

    observe({
        if (is.null(myvals$sentiment)) {
            hideTab(inputId = "tabs", target = "sentimentTab")
        } else {
            showTab(inputId = "tabs", target = "sentimentTab")
        }
    })

    corpusFile <- callModule(load_corpus_server, "load_corpus_csv")
    corpus <- callModule(create_corpus_server, "", corpusFile)
    callModule(render_corpus_server, "corpus_table", corpusFile)

    lexModule <- callModule(lexicon_server , "lexicon_ui")
    observe({
        myvals$selectedLexicons <- lexModule$selected
        myvals$lexiconList <- lexModule$lexiconList
    })

    valenceModule <- callModule(valence_server, "valence_ui")
    observe({
        myvals$selectedValence <- valenceModule$selected
        myvals$useValence <- valenceModule$useValence
        myvals$valenceMethod <- valenceModule$method
        myvals$valenceList <- valenceModule$valenceList
    })

    howModule <- callModule(how_server, "how_ui")
    observe({
        myvals$how <- howModule$selected
    })

    corpusSummaryModule <- callModule(corpus_summary_server, "corpus_summary_ui", corpus)

    sentoLexicon <- callModule(build_sento_lexicon, "", myvals)

    output$calculateSentimentButton <- renderUI({
            actionButton(
                 inputId = "calcSentimentButton",
                 label = "Calculate sentiment",
                 icon = icon("rocket")
            )
    })

    observeEvent(input$calcSentimentButton, ignoreInit = FALSE, {

            if (is.null(sentoLexicon())) {
                showModal(modalDialog(
                    title = "Error",
                    "Select a corpus and lexicon first..."
                ))
            } else {
                showTab(inputId = "tabs", target = "sentimentTab")
                updateTabsetPanel(session, "tabs", selected = "sentimentTab")
                sentimentModule <- callModule(sentiment_server, "sentiment_ui", myvals,
                                              corpus, sentoLexicon, input$calcSentimentButton)
                observe({
                    if (!is.null(sentimentModule$sento_measures)) {
                        myvals$sento_measures <- sentimentModule$sento_measures
                    } else {
                        myvals$sento_measures <- NULL
                    }
                })
                observe({
                    if (!is.null(sentimentModule$sentiment)) {
                        myvals$sentiment <- as.data.table(sentimentModule$sentiment)
                    } else {
                        myvals$sentiment <- NULL
                    }
                })
            }
    })
    callModule(indices_server, "indices_ui", reactive(myvals$sento_measures))
}

shinyApp(ui = ui, server = server)

