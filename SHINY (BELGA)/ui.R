
library(shiny)
library(ggplot2)
library(ggthemes)
library(data.table)
library(shinythemes)
library(extrafont)
library(dplyr)
library(scales)
library(shinyjs)
library(TTR)
library(grDevices)

# extrafont::loadfonts(device = "win", quiet = TRUE)

inputs <- function() { 
  wellPanel(
    fluidRow(
      column(width = 5,
         checkboxGroupInput("keywords",
                            "Keywords", c("Brexit" = "Brexit", 
                                          "Michel" = "Michel",
                                          "PS" = "PS",
                                          "Trump" = "Trump",
                                          "Vlaams Belang" = "Vlaams_Belang")), # _ for whitespaces within keywords
         radioButtons('and_or', 
                      'Inclusion', c("AND" = "&", 
                                     "OR" = "|"),
                      inline = TRUE)),
      column(width = 3,
         checkboxGroupInput("language",
                            "Language", c("Dutch" = "nl",
                                          "French" = "fr"), 
                            selected = c())),
      column(width = 4,   
         checkboxGroupInput("lexicon",
                            "Lexicon", c("Harvard General Inquirer" = "GI",
                                         "Loughran and McDonald" = "McDonald",
                                         "pattern (Python)" = "pattern"),
                            selected = c()))
    )
  )
}

fluidPage(
  theme = shinytheme("readable"), # readable, journal, cerulean, flatly, etc. (?shinythemes)
  tags$style(type = "text/css",
             HTML(".shiny-output-error-validation {
                    color: blue;
                    font-size: 13px;
                  }")),
  shinyjs::useShinyjs(), # activate shinyjs
  div(style = "font-size: 10px",
    verticalLayout(
      tags$h2("Textual Sentiment: An Example from Politics"),
      hr(), # horizontal line
      fluidRow(
        column(width = 3, 
               tags$h4("Parameters")),
        column(width = 6, 
               tags$h4("Textual Sentiment Indices")),
        column(width = 3, 
               tags$h4("Statistics"))
      ),
      fluidRow(
        column(width = 3, 
               inputs(),
               div(column(width = 8),
                   column(width = 4, 
                          radioButtons("normalised", 
                            "Normalisation", c("Yes" = TRUE,
                                               "No" = FALSE), 
                            inline = TRUE)))),
        div(style = "position: relative",
            column(width = 6,
                   plotOutput("plot", hover = hoverOpts("plot_hover", delay = 25, delayType = "debounce")),
                   div(style = "font-size: 120%",
                       uiOutput("hover_info"))
                   )),
        div(style = "font-size: 100%",
            column(width = 3,
                   tableOutput("stats"),
                   br(),
                   uiOutput("sel_keywords"))
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
)
