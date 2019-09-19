
#' Shiny application for sentiment computation and visualisation
#'
#' @author Jeroen Van Pelt, Samuel Borms
#'
#' @description A Shiny application to showcase many of the package's functionalities.
#'
#' @details This Shiny application demonstrates mainly the \code{compute_sentiment}, \code{corpus_summarize} and
#' \code{aggregate.sentiment} functions to gain insights in an uploaded corpus. The corpus should be uploaded in
#' .csv format (by default the \code{\link{usnews}} dataset is displayed). Lexicons and valence shifters can
#' be chosen from the built-in options or uploaded, and the weighting schemes are those available in the package.
#' All calculated values and statistics can be downloaded as a .csv file also.
#'
#' @export
sento_app <- function() {
  suggests <- sapply(c("shiny", "shinyWidgets", "shinythemes", "shinycssloaders", "DT"),
                     function(pkg) !requireNamespace(pkg, quietly = TRUE))
  if (any(suggests)) {
    stop("Make sure to have installed following packages before running the app: ",
         paste0(names(suggests)[suggests], collapse = ", "), ".")
  }
  appDir <- system.file("shiny", package = "sentometrics")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing the sentometrics package.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}

