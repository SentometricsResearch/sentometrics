


library(shiny)
library(ggplot2)
library(ggthemes)
library(data.table)
library(shinythemes)
library(extrafont)
library(dplyr)
library(scales)
library(TTR)
library(grDevices)

# extrafont::loadfonts(device = "win", quiet = TRUE)

inputs <- function() {
  wellPanel(fluidRow(
    column(
      width = 4,
      checkboxGroupInput(
        "language",
        "Language",
        c("French (fr)" = "fr",
          "Dutch (nl)" = "nl"),
        selected = c("nl", "fr"),
        inline = FALSE
      )
    ),
    column(
      width = 4,
      checkboxGroupInput(
        "lexicon",
        "Lexicon",
        c("General" = "General",
          "Financial" = "Financial"),
        selected = c("Financial"),
        inline = FALSE
      )
    ),
    column(width = 4,
           checkboxGroupInput(
             "desk",
             "Desk",
             c(
               "Belgium" = "BIN INT",
               "Abroad" = "BTL EXT",
               "Sport" = "SPN SPF"
             ),
             selected = c()
           ))
  ))
}

navbarPage(
  title = "Textual Sentiment Indexation",
  header = "",
  theme = shinytheme("flatly"),
  # readable, journal, cerulean, flatly, etc. (?shinythemes)
  tabPanel(
    'Visualisation',

    tags$style(
      type = "text/css",
      HTML(
        ".shiny-output-error-validation {
        color: blue;
        font-size: 12px;
        }"
      )
      ),
    div(style = "font-size: 11px; padding-top: 0px",
        verticalLayout(
          # tags$h3("The Use of Textual Analysis to Sense Sentiment and Spot Significant Events"),
          # br(),
          fluidRow(column(
            width = 3,
            selectInput(
              "keywords",
              "Select Topic of Interest",
              c(
                "Politics" = "Political",
                "Terrorism" = "Terrorism",
                "Finance" = "Financial",
                "Football" = "Football",
                "Economy" = "Economy"
              ),
              multiple = FALSE,
              width = '100%',
              selected = c("Financial")
            )
          )),

          tabsetPanel(
            type = "tabs",

            tabPanel(
              "Single Index",

              fluidRow(div(
                style = "font-weight: bold",
                column(width = 3,
                       tags$h4("Parameters")),
                column(width = 7,
                       tags$h4(
                         "Textual Sentiment Index (Fully Aggregated)"
                       )),
                column(width = 2,
                       tags$h4("Statistics"))
              )),
              fluidRow(
                column(
                  width = 3,
                  wellPanel(
                    style = "padding: 10px",
                    sliderInput(
                      "wLan",
                      "Weight to French Documents (%)",
                      min = 0,
                      max = 100,
                      value = 50
                    ),
                    sliderInput(
                      "wLex",
                      "Weight to the 'General' Lexicon (%)",
                      min = 0,
                      max = 100,
                      value = 50
                    )
                  ),
                  div(
                    column(
                      width = 5,
                      radioButtons(
                        "normalised2",
                        "Normalisation",
                        c("Yes" = TRUE,
                          "No" = FALSE),
                        inline = TRUE
                      )
                    ),
                    column(
                      width = 7,
                      numericInput(
                        "minWords2",
                        "Min. Words per Document",
                        value = 150,
                        step = 10,
                        width = "90%"
                      )
                    )
                  ),
                  div(
                    column(
                      width = 5,
                      radioButtons(
                        "aggregation2",
                        "Aggregation",
                        c("Mean" = "Mean",
                          "Sum" = "Sum"),
                        inline = TRUE
                      )
                    ),
                    column(
                      width = 7,
                      numericInput(
                        "minDocs2",
                        "Min. Documents per Day",
                        value = 3,
                        step = 1,
                        width = "90%"
                      )
                    )
                  ),
                  div(
                    column(
                      width = 5,
                      radioButtons(
                        "dots2",
                        "Show Points",
                        c("Yes" = "1",
                          "No" = "0"),
                        selected = "0",
                        inline = TRUE
                      )
                    ),

                    column(
                      width = 7,
                      numericInput(
                        "nSMA2",
                        "Moving Average Period",
                        value = 90,
                        min = 5,
                        max = 180,
                        width = "90%"
                      )
                    )
                  )
                ),
                column(
                  width = 7,
                  div(style = "position: relative",
                      plotOutput("plotFull")),
                  div(
                    column(
                      width = 6,
                      dateRangeInput(
                        "dates2",
                        "Select Time Horizon",
                        min = "2010-01-01",
                        max = "2016-12-31",
                        start = "2010-01-01",
                        end = "2016-12-31",
                        format = "dd-mm-yyyy"
                      )
                    ),
                    column(
                      offset = 4,
                      width = 2,
                      downloadButton('exportFull',
                                     'Download Time Series', class = "exp"),
                      tags$head(
                        tags$style(
                          ".exp{background-color:white;} .exp{color: black;}
                          .exp{font-size: 9px;} .exp{padding: 4px}"
                        )
                        )
                      )
                    )
                  ),
                column(
                  width = 2,
                  div(
                    style = "font-size: 100%",
                    uiOutput("meanSent"),
                    uiOutput("nDocsFull"),
                    uiOutput("nWordsFull")
                  )
                )
                )

          ),


          tabPanel(
            "Multiple Indices",

            # hr(), # horizontal line
            fluidRow(div(
              style = "font-weight: bold",
              column(width = 3,
                     tags$h4("Parameters")),
              column(width = 7,
                     tags$h4("Textual Sentiment Indices")),
              column(width = 2,
                     tags$h4("Statistics"))
            )),
            fluidRow(
              column(
                width = 3,
                inputs(),
                div(
                  column(
                    width = 5,
                    radioButtons(
                      "normalised",
                      "Normalisation",
                      c("Yes" = TRUE,
                        "No" = FALSE),
                      inline = TRUE
                    )
                  ),
                  column(
                    width = 7,
                    numericInput(
                      "minWords",
                      "Min. Words per Document",
                      value = 150,
                      step = 10,
                      width = "90%"
                    )
                  )
                ),
                div(
                  column(
                    width = 5,
                    radioButtons(
                      "aggregation",
                      "Aggregation",
                      c("Mean" = "Mean",
                        "Sum" = "Sum"),
                      inline = TRUE
                    )
                  ),
                  column(
                    width = 7,
                    numericInput(
                      "minDocs",
                      "Min. Documents per Day",
                      value = 3,
                      step = 1,
                      width = "90%"
                    )
                  )
                ),
                div(
                  column(
                    width = 5,
                    radioButtons(
                      "dots",
                      "Show Points",
                      c("Yes" = "1",
                        "No" = "0"),
                      selected = "0",
                      inline = TRUE
                    )
                  ),

                  column(
                    width = 7,
                    numericInput(
                      "nSMA",
                      "Moving Average Period",
                      value = 90,
                      min = 5,
                      max = 180,
                      width = "90%"
                    )
                  )
                )
              ),
              column(
                width = 7,
                div(style = "position: relative",
                    plotOutput("plot")),
                div(
                  column(
                    width = 6,
                    dateRangeInput(
                      "dates",
                      "Select Time Horizon",
                      min = "2010-01-01",
                      max = "2016-12-31",
                      start = "2010-01-01",
                      end = "2016-12-31",
                      format = "dd-mm-yyyy"
                    )
                  ),
                  column(
                    offset = 4,
                    width = 2,
                    downloadButton('export',
                                   'Download Time Series', class = "exp"),
                    tags$head(
                      tags$style(
                        ".exp{background-color:white;} .exp{color: black;}
                        .exp{font-size: 9px;} .exp{padding: 4px}"
                      )
                      )
                    )
                  )
                ),
              column(
                width = 2,
                div(
                  style = "font-size: 100%",
                  tableOutput("stats"),
                  uiOutput("nDocs"),
                  uiOutput("nWords")
                )
              )
              )
          ),

          tabPanel(
            "Influential Articles",

            # hr(), # horizontal line
            fluidRow(div(style = "font-weight: bold",
                         column(
                           width = 4,
                           tags$h4("Parameters")
                         ))),
            fluidRow(
              column(
                width = 4,
                numericInput(
                  "minWords3",
                  "Min. Words per Document",
                  value = 150,
                  step = 10,
                  width = "90%"
                )
              ),
              column(
                width = 4,
                numericInput(
                  "nLeads",
                  "Number of Articles",
                  value = 5,
                  step = 1,
                  width = "90%"
                )
              ),
              div(style = "padding-top: 0px",
                  column(
                    width = 3,
                    dateRangeInput(
                      "dates3",
                      "Select Time Horizon",
                      min = "2010-01-01",
                      max = "2016-12-31",
                      start = "2010-01-01",
                      end = "2016-12-31",
                      format = "dd-mm-yyyy"
                    )
                  ))
            ),
            fluidRow(
              div(
                column(width = 12,
                       tags$h4("French Articles")),
                column(width = 6,
                       tags$h5("General Lexicon")),
                column(width = 6,
                       tags$h5("Financial Lexicon")),
                column(width = 6,
                       tableOutput("GenFR")),
                column(width = 6,
                       tableOutput("FinFR"))
              ),
              div(
                column(width = 12,
                       tags$h4("Dutch Articles")),
                div(
                  column(width = 6,
                         tags$h5("General Lexicon")),
                  column(width = 6,
                         tags$h5("Financial Lexicon")),
                  column(width = 6,
                         tableOutput("GenNL")),
                  column(width = 6,
                         tableOutput("FinNL"))
                )
              )
            )
          )
        )
    ))
      ),
  tabPanel(
    "Methodology",
    tags$style(type = "text/css"),
    # br(),
    fluidRow(
      div(
        style = "font-weight: bold; font-size: 11px",
        column(width = 6,
               wellPanel(
                 style = "padding: 2px 7px 2px 7px;",
                 tags$h5("A few words on the methodology employed")
               )),
        column(width = 6,
               wellPanel(
                 style = "padding: 2px 7px 2px 7px;",
                 tags$h5("Which analyses are of interest?")
               ))
      )
    ),
    fluidRow(div(
      style = "font-size: 12px",
      column(width = 6,
             uiOutput("methodology")),
      column(width = 6,
             uiOutput("analysis"))
    ))
  ),
  tabPanel(
    "Team",
    tags$style(type = "text/css"),
    fluidRow(column(width = 12,
                    h3("Academic Team"))),
    fluidRow(
      column(width = 4,
             div(
               column(width = 11,
                      h4("Kris Boudt (Vrije Universiteit Brussel)")),
               div(style = "padding: 6px 0px 0px 0px",
                   column(
                     width = 1,
                     tags$a(imageOutput(
                       "linkedinB", width = "25px", height = "25px"
                     ),
                     href = "https://www.linkedin.com/in/kris-boudt-44b20313/")
                   ))
             )),
      column(width = 4,
             div(
               column(width = 11,
                      h4("David Ardia (Université de Neuchâtel)")),
               div(style = "padding: 6px 0px 0px 0px",
                   column(
                     width = 1,
                     tags$a(imageOutput(
                       "linkedinA", width = "25px", height = "25px"
                     ),
                     href = "https://www.linkedin.com/in/davidardia/")
                   ))
             )),
      column(width = 4,
             div(column(
               width = 12,
               h4("James Thewissen (KU Leuven)")
             )))
    ),
    fluidRow(
      div(
        column(width = 4,
               div(
                 column(width = 3,
                        imageOutput(
                          "boudt", width = "150px", height = "200px"
                        )),
                 column(
                   offset = 1,
                   width = 8,
                   HTML(
                     "Prof. Kris Boudt is an Associate Professor at the <b> Vrije Universiteit Brussel </b> with extensive experience in (financial) econometrics.
                     The past several years, he has been developing statistical tools to analyse sentiment present in texts, like corporate
                     publications. He also holds a position at the Vrije Universiteit Amsterdam."
                   )
                   )
                   )),
        column(width = 4,
               div(
                 column(width = 3,
                        imageOutput(
                          "ardia", width = "150px", height = "200px"
                        )),
                 column(
                   offset = 1,
                   width = 8,
                   HTML(
                     "Prof. David Ardia is an Assistant Professor at the Université de Neuchâtel as well as the Université Laval. He is a specialist in the modelling of financial risks,
                     with very strong computational skills, who has been working with Prof. Boudt for close to a decade."
                   )
                   )
                 )),
        column(width = 4,
               div(
                 column(
                   width = 3,
                   imageOutput("thewissen", width = "150px", height = "200px")
                 ),
                 column(
                   offset = 1,
                   width = 8,
                   HTML(
                     "Prof. James Thewissen contributed with his PhD to the analysis of corporate information, and has a solid knowledge in text mining and
                     statistical textual analysis techniques."
                   )
                   )
                 ))
               ),
      div(
        column(width = 3,
               div(column(
                 width = 12,
                 h4("Keven Bluteau (UniNE)")
               ))),
        column(width = 3,
               div(column(
                 width = 12,
                 h4("Samuel Borms (GSoC)")
               ))),
        column(width = 3,
               div(column(
                 width = 12,
                 h4("Andres Algaba (VUB)")
               ))),
        column(width = 3,
               div(column(
                 width = 12,
                 h4("Wouter Torsin (KUL)")
               ))),
        column(width = 3,
               div(
                 column(width = 3,
                        imageOutput(
                          "bluteau", width = "150px", height = "200px"
                        )),
                 column(
                   offset = 1,
                   width = 8,
                   HTML(
                     "Keven Bluteau is a PhD student, working primarily on how to use textual sentiment analysis to forecast economic and financial
                     variables."
                   )
                   )
                 )),
        column(width = 3,
               div(
                 column(width = 3,
                        imageOutput(
                          "borms", width = "150px", height = "200px"
                        )),
                 column(
                   offset = 1,
                   width = 8,
                   HTML(
                     "Samuel Borms is an upcoming PhD student, currently devoted to the Sentometrics Google Summer of Code (GSoC) project."
                   )
                 )
               )),
        column(width = 3,
               div(
                 column(width = 3,
                        imageOutput(
                          "algaba", width = "150px", height = "200px"
                        )),
                 column(
                   offset = 1,
                   width = 8,
                   HTML(
                     "Andres Algaba is a PhD student who studies the time variation in financial time series, including those who are sentiment-based."
                   )
                 )
               )),
        column(width = 3,
               div(
                 column(width = 3,
                        imageOutput(
                          "torsin", width = "150px", height = "200px"
                        )),
                 column(
                   offset = 1,
                   width = 8,
                   HTML(
                     "Wouter Torsin is a PhD student who has been investigating the tone in a large amount of corporate publications."
                   )
                 )
               ))
               )
        ),
    fluidRow(column(width = 12,
                    h3(
                      "Industrial Partners"
                    ))),
    fluidRow(column(width = 4,
                    div(
                      column(width = 12,
                             h4("Tom Wuytack"))
                    )),
             column(width = 4,
                    div(
                      column(width = 12,
                             h4("Stefan Hartmann"))
                    ))),
    fluidRow(column(width = 4,
                    div(
                      column(width = 3,
                             imageOutput(
                               "wuytack", width = "150px", height = "200px"
                             )),
                      column(
                        offset = 1,
                        width = 8,
                        HTML(
                          "Tom Wuytack is the Chief Information Officer at <b>Belga News Agency</b>, a company he has been working at for over 15 years. He
                          is convinced about the positive impact a trustworthy calculation of textual sentiment will have on extending media analysis services
                          to Belga's clients."
                        )
                        )
                        )),
             column(width = 4,
                    div(
                      column(
                        width = 3,
                        imageOutput("hartmann", width = "150px", height = "200px")
                      ),
                      column(
                        offset = 1,
                        width = 8,
                        HTML(
                          "Stefan Hartmann is Head of Quantitative Research at <b> Finvex </b>. He is knowledgeable on the majority of contemporary investment
                          strategies and products. He has observed a trend towards the explicit incorporation of sentiment expressed in texts
                          in investment decisions and products, which he himself believes has strong potential."
                        )
                        )
                        )))
                      ),
  tabPanel(
    "News",
    tags$style(type = "text/css"),
    fluidRow(column(width = 12,
                    HTML("<b> April 2017 </b>")),
             column(
               width = 12,
               h4(
                 "Green light for first stage of Team Up development of Sentometrics together with Belga and Finvex."
               )
             )),
    hr(),
    fluidRow(column(width = 12,
                    HTML("<b> May 2017 </b>")),
             column(width = 12,
                    h4(helpText(
                      a(
                        "Google Summer of Code funding for Samuel Borms to create the R package Sentometrics.",
                        href = "https://summerofcode.withgoogle.com/projects/#5832364795101184",
                        target = "_blank"
                      )
                    )))),
    hr(),
    fluidRow(column(width = 12,
                    HTML("<b> May 2017 </b>")),
             column(width = 12,
                    h4(helpText(
                      a(
                        "White paper on Sentometrics released on SSRN.",
                        href = "https://www.linkedin.com/feed/update/urn:li:activity:6274895843879456768",
                        target = "_blank"
                      )
                    )))),
    hr(),
    fluidRow(column(width = 12,
                    HTML("<b> June 2017 </b>")),
             column(width = 12,
                    h4(helpText(
                      a(
                        "Swissuniversities funding for Sentometrics research of Keven Bluteau.",
                        href = "https://www.linkedin.com/feed/update/urn:li:activity:6276875594202386432",
                        target = "_blank"
                      )
                    ))))
  )
                    )
