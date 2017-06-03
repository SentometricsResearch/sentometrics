
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
  wellPanel(
    fluidRow(
      column(width = 4,
             checkboxGroupInput("desk", # currently of little value: change to something meaningful if new data comes in
                                "Desk", c("Belgium" = "BIN INT",
                                          "Abroad" = "BTL EXT",
                                          "Sport" = "SPN SPF"),
                                selected = c())),
      column(width = 4,
             checkboxGroupInput("language",
                                "Language", c("French (fr)" = "fr",
                                              "Dutch (nl)" = "nl"),
                                selected = c("nl"))),
      column(width = 4,
             checkboxGroupInput("lexicon",
                                "Lexicon", c("General" = "General",
                                             "Financial" = "Financial"),
                                selected = c("General")))
    )
  )
}

navbarPage(
  title = "Textual Sentiment Indexation",
  header = "",
  theme = shinytheme("readable"), # readable, journal, cerulean, flatly, etc. (?shinythemes)
  tabPanel('Visualisation',
    tags$style(type = "text/css",
               HTML(".shiny-output-error-validation {
                    color: blue;
                    font-size: 12px;
                    }")),
    div(style = "font-size: 11px; padding-top: 0px",
      verticalLayout(
        tags$h3("The Use of Textual Analysis to Sense Sentiment and Spot Significant Events"),
        br(),
        fluidRow(
          column(width = 3,
                 selectInput("keywords",
                             "Select Topic of Interest", c("Brexit" = "Brexit",
                                                            "Trump" = "Trump", 
                                                            "Terrorism" = "Terrorism",
                                                            "Michel" = "Michel",
                                                            "CETA" = "CETA",
                                                            "Financial" = "Financial",
                                                            "Football" = "Football",
                                                            "Economy" = "Economy"),
                             multiple = FALSE,
                             width = '100%',
                             selected = c("Trump", "Football")))
        ),
        hr(), # horizontal line
        fluidRow(
          div(style = "font-weight: bold",
              column(width = 3,
                 tags$h4("Parameters")),
              column(width = 6,
                     tags$h4("Textual Sentiment Indices")),
              column(width = 3,
                     tags$h4("Statistics"))
          )
        ),
        fluidRow(
          column(width = 3,
                 inputs(),
                 div(column(width = 2),
                     column(width = 5,
                            radioButtons("normalised",
                                         "Normalisation", c("Yes" = TRUE,
                                                            "No" = FALSE),
                              inline = TRUE)),
                     column(width = 5,
                            radioButtons("aggregation",
                                         "Aggregation", c("Mean" = "Mean",
                                                          "Sum" = "Sum"),
                            inline = TRUE))),
                 br(),
                 br(),
                 div(type = "padding-top: 0px",
                     column(width = 2),
                     column(width = 10,
                            numericInput("minWords",
                                         "Minimum Number of Words",
                                         value = 150,
                                         step = 10))),
                 div(type = "padding-top: 0px",
                     column(width = 2),
                     column(width = 10,
                            numericInput("minDocs",
                                         "Minimum Number of Documents per Day",
                                         value = 3,
                                         step = 1)))
                 ),
          div(style = "position: relative",
              column(width = 6,
                     plotOutput("plot", hover = hoverOpts("plot_hover", delay = 25, delayType = "debounce")),
                     div(style = "font-size: 120%",
                         uiOutput("hoverInfo")))
              ),
          div(style = "font-size: 100%",
              column(width = 3,
                     tableOutput("stats"),
                     uiOutput("nDocs"),
                     uiOutput("nWords"))
              )
        ),
        br(), # blank line
        fluidRow(
          column(width = 3),
          column(width = 3,
                 dateRangeInput("dates",
                                "Select time horizon", min = "2016-01-01", max = "2016-12-31",
                                start = "2016-01-01", end = "2016-12-31",
                                format = "dd-mm-yyyy"))
        )
      )
    )
  ),
  tabPanel('Methodology',
    tags$style(type = "text/css"),
    br(),
    fluidRow(
      div(style = "font-weight: bold; font-size: 11px",
        column(width = 4,
               wellPanel(style = "padding: 2px 7px 2px 7px;",
                 tags$h5("A few words on the methodology employed"))
               ),
        column(width = 4,
               wellPanel(style = "padding: 2px 7px 2px 7px;",
                         tags$h5("What can be discerned from the visualisations?"))
        ),
        column(width = 4,
               wellPanel( style = "padding: 2px 7px 2px 7px;",
                 tags$h5("Still to be done"))
               )
      )
    ),
    fluidRow(
      div(style = "font-size: 12px",
      column(width = 4,
             uiOutput("methodology")),
      column(width = 4,
             uiOutput("spotted")),
      column(width = 4,
             uiOutput("toDo"))
      )
    )
  )
)

