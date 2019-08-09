
#' Shiny App
#'
#' @author Jeroen Van Pelt, Samuel Borms, Andres Algaba
#'
#' @description Shiny app to visually set-up sentiment computation.
#'
#' @details The Shiny app is a visual wrapper around the \code{compute_sentiment} function. You can upload texts or a corpus in csv
#' format, choose built-in lexicons/valence shifters or upload your own lexicons/valence shifters in csv format. All the available
#' weighting schemas that are available in the sentometrics package can be selected and with one click on a button, you can calculate
#' the sentiment. Once calculated, the sentiment can be calculated as a CSV file. Additionally, there is some functionality coming
#' from the \code{corpus_summarize} function to gain insights in the corpus that you have uploaded.
#'
#'
#' @export
sento_app <- function() {
  appDir <- system.file("shiny", package = "sentometrics")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `sentometrics`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}

