
#' Plot sentiment measures
#'
#' @author Samuel Borms
#'
#' @method plot sentomeasures
#'
#' @description Straightforward plotting method that shows all sentiment measures from the provided \code{sentomeasures}
#' object in one plot, or the average along one of the lexicons, features and time weighting dimensions. We suggest to make
#' use of the \code{\link{measures_select}} function when you desire to plot only a subset of the sentiment measures.
#'
#' @param x a \code{sentomeasures} object created using \code{\link{sento_measures}}.
#' @param group a value from \code{c("lexicons", "features", "time", "all")}. The first three choices display the average of
#' all measures from the same group, in a different color. The choice \code{"all"} displays every single sentiment measure
#' in a separate color, but this may look visually overwhelming very fast, and can be quite slow.
#' @param ... not used.
#'
#' @return Returns a simple \code{\link{ggplot}} object, which can be added onto (or to alter its default elements) by using
#' the \code{+} operator (see examples). By default, a legend is positioned at the top if there are at maximum twelve line
#' graphs plotted and \code{group} is different from \code{"all"}.
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' # construct a sentomeasures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 500)
#' l <- setup_lexicons(list_lexicons[c("LM_en")], list_valence_shifters[["en"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpusSample, l, ctr)
#'
#' # plot sentiment measures
#' plot(sentomeasures, group = "features")
#'
#' \dontrun{
#' # adjust appearance of plot
#' p <- plot(sentomeasures)
#' p <- p +
#'   ggthemes::theme_base() +
#'   scale_x_date(name = "month-year") +
#'   scale_y_continuous(name = "newName")
#' p}
#'
#' @import ggplot2
#' @export
plot.sentomeasures <- function(x, group = "all", ...) {

  if (!(group %in% c("lexicons", "features", "time", "all")))
    stop("The 'group' argument should be either 'lexicons', 'features', 'time' or 'all'.")
  # melt sentiment measures for plotting
  sentomeasures <- x
  measures <- get_measures(sentomeasures)
  if (group == "all") {
    measuresMelt <- melt(measures, id.vars = "date", variable.factor = FALSE)
    legendPos <- "none"
  } else {
    measuresMelt <- to_long(measures)[, c("date", group, "value"), with = FALSE]
    measuresMelt <- measuresMelt[, list(value = mean(value)), by = list(date, variable = eval(parse(text = group)))]
    legendPos <- ifelse(length(unique(measuresMelt[["variable"]])) <= 12, "top", "none")
  }
  measuresMelt <- measuresMelt[order(rank(as.character(variable)))]
  p <- ggplot(data = measuresMelt, aes(x = date, y = value, color = variable)) +
    geom_line() +
    geom_hline(yintercept = 0, size = 0.50, linetype = "dotted") +
    scale_x_date(name = "Date", date_labels = "%m-%Y") +
    scale_y_continuous(name = "Sentiment") +
    ggthemes::theme_tufte(base_size = 12) +
    theme(legend.title = element_blank(), legend.position = legendPos)

  return(p)
}

#' Differencing of sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Differences the sentiment measures from a \code{sentomeasures} object.
#'
#' @param x a \code{sentomeasures} object created using \code{\link{sento_measures}}.
#' @param lag a \code{numeric}, see documentation for the generic \code{\link{diff}}.
#' @param differences a \code{numeric}, see documentation for the generic \code{\link{diff}}.
#' @param ... not used.
#'
#' @return A modified \code{sentomeasures} object, with the measures replaced by the differenced measures as well as updated
#' statistics.
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' # construct a sentomeasures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 500)
#' l <- setup_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpusSample, l, ctr)
#'
#' # first-order difference sentiment measures with a lag of two
#' diffed <- diff(sentomeasures, lag = 2, differences = 1)
#'
#' @export
diff.sentomeasures <- function(x, lag = 1, differences = 1, ...) {
  sentomeasures <- x
  dates <- get_dates(sentomeasures)[-1:-(lag * differences)]
  measures <- get_measures(sentomeasures)[, -1] # drop dates
  measuresDiff <- diff(as.matrix(measures), lag = lag, differences = differences)
  sentomeasures$measures <- data.table(date = dates, measuresDiff)
  sentomeasures$stats <- compute_stats(sentomeasures)
  return(sentomeasures)
}

#' @export
nmeasures.sentomeasures <- function(sentomeasures) {
  NCOL(sentomeasures[["measures"]]) - 1 # omit date column
}

#' Get number of sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Returns the number of sentiment measures.
#'
#' @param sentomeasures a \code{sentomeasures} object created using \code{\link{sento_measures}}.
#'
#' @return The number of sentiment measures in the input \code{sentomeasures} object.
#'
#' @export
nmeasures <- function(sentomeasures) {
  UseMethod("nmeasures", sentomeasures)
}

#' @export
nobs.sentomeasures <- function(sentomeasures) {
  NROW(sentomeasures[["measures"]])
}

#' Get number of observations in the sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Returns the number of data points available for the sentiment measures.
#'
#' @param sentomeasures a \code{sentomeasures} object created using \code{\link{sento_measures}}.
#'
#' @return The number of rows (observations/data points) in \code{sentomeasures[["measures"]]}.
#'
#' @export
nobs <- function(sentomeasures) {
  UseMethod("nobs", sentomeasures)
}

#' Scaling and centering of sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Scales and centers the sentiment measures from a \code{sentomeasures} object, column-per-column. By default,
#' the measures are normalized. \code{NA}s are removed first.
#'
#' @details If one of the arguments \code{center} or \code{scale} is a \code{matrix}, this operation will be applied first,
#' and eventual other centering or scaling is computed on that data.
#'
#' @param x a \code{sentomeasures} object created using \code{\link{sento_measures}}.
#' @param center a \code{logical} or a \code{numeric} vector, see documentation for the generic \code{\link{scale}}.
#' Alternatively, one can provide a \code{matrix} of dimensions \code{nobs(sentomeasures)} times \code{1} or
#' \code{nmeasures(sentomeasures)} with values to add to each individual observation.
#' @param scale a \code{logical} or a \code{numeric} vector, see documentation for the generic \code{\link{scale}}.
#' Alternatively, one can provide a \code{matrix} of dimensions \code{nobs(sentomeasures)} times \code{1} or
#' \code{nmeasures(sentomeasures)} with values to divide each individual observation by.
#'
#' @return A modified \code{sentomeasures} object, with the measures replaced by the scaled measures as well as updated
#' statistics.
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' # construct a sentomeasures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 500)
#' l <- setup_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpusSample, l, ctr)
#'
#' # scale sentiment measures to zero mean and unit standard deviation
#' sc1 <- scale(sentomeasures)
#'
#' n <- nobs(sentomeasures) # row dimension
#' m <- nmeasures(sentomeasures) # column dimension
#'
#' # add a matrix
#' sc2 <- scale(sentomeasures, center = matrix(runif(n * m), n, m), scale = FALSE)
#'
#' # divide every row observation based on a one-column matrix, then center
#' sc3 <- scale(sentomeasures, center = TRUE, scale = matrix(runif(n)))
#'
#' @export
scale.sentomeasures <- function(x, center = TRUE, scale = TRUE) {
  sentomeasures <- x
  dates <- get_dates(sentomeasures)
  measures <- get_measures(sentomeasures)[, -1] # drop dates
  if (is.matrix(center)) {
    if (nrow(center) != nobs(sentomeasures) || !(ncol(center) %in% c(1, nmeasures(sentomeasures))))
      stop("The matrix dimensions of the 'center' argument are not correct.")
    measures <- measures + center
    center <- FALSE
  }
  if (is.matrix(scale)) {
    if (nrow(scale) != nobs(sentomeasures) || !(ncol(scale) %in% c(1, nmeasures(sentomeasures))))
      stop("The matrix dimensions of the 'scale' argument are not correct.")
    measures <- measures / scale
    scale <- FALSE
  }
  measuresNorm <- scale(measures, center, scale)
  sentomeasures$measures <- data.table(date = dates, measuresNorm)
  sentomeasures$stats <- compute_stats(sentomeasures)
  return(sentomeasures)
}

#' @export
summary.sentomeasures <- function(object, ...) {
  sentomeasures <- object
  freq <- c("daily", "weekly", "monthly", "yearly")[c("day", "week", "month", "year") %in% sentomeasures$by]
  cat("This sentomeasures object contains", nmeasures(sentomeasures), "textual sentiment time series",
      "with", nobs(sentomeasures), "observations each,", "at a", freq, "frequency.", "\n")
  cat("The corpus has following features:", sentomeasures$features, "\n")
  cat("\n")
  cat("Following lexicons are used to calculate sentiment:", sentomeasures$lexicons, "\n")
  cat("Following scheme is applied for aggregation within documents:", sentomeasures$howWithin, "\n")
  cat("Following scheme is applied for aggregation across documents:", sentomeasures$howDocs, "\n")
  cat("Following schemes are applied for aggregation across time:", sentomeasures$time, "\n")
  cat("\n")
  cat("Aggregate statistics:", "\n")
  print(round(rowMeans(sentomeasures$stats), 5))
}

#' @export
print.sentomeasures <- function(x, ...) {
  sentomeasures <- x
  cat("A sentomeasures object that carries with it", nmeasures(sentomeasures),
      "distinct textual sentiment time series of", nobs(sentomeasures),
      "observations each.")
}

#' Get the dates of the sentiment measures/time series
#'
#' @author Samuel Borms
#'
#' @description Convenience function. Returns the dates of the sentiment time series.
#'
#' @param sentomeasures a \code{sentomeasures} object created using \code{\link{sento_measures}}.
#'
#' @return The \code{"date"} column in \code{sentomeasures[["measures"]]} as a \code{character} vector.
#'
#' @export
get_dates <- function(sentomeasures) {
  check_class(sentomeasures, "sentomeasures")
  sentomeasures$measures[, date]
}

#' Get the dimensions of the sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Convenience function. Returns the components across all three dimensions of the sentiment measures.
#'
#' @param sentomeasures a \code{sentomeasures} object created using \code{\link{sento_measures}}.
#'
#' @return The \code{"features"}, \code{"lexicons"} and \code{"time"} elements in \code{sentomeasures}.
#'
#' @export
get_dimensions <- function(sentomeasures) {
  check_class(sentomeasures, "sentomeasures")
  sentomeasures[c("features", "lexicons", "time")]
}

#' Get the sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Convenience function. Returns the sentiment measures in either wide (by default) or long format.
#'
#' @param sentomeasures a \code{sentomeasures} object created using \code{\link{sento_measures}}.
#' @param format a single \code{character} vector, one of \code{c("wide", "long")}.
#'
#' @return The panel of sentiment measures under \code{sentomeasures[["measures"]]}, in wide or long format.
#'
#' @export
get_measures <- function(sentomeasures, format = "wide") {
  check_class(sentomeasures, "sentomeasures")
  if (format == "wide")
    sentomeasures[["measures"]]
  else if (format == "long")
    to_long(sentomeasures[["measures"]])
  else
    stop("The 'format' argument should be 'wide' or 'long'.")
}

