
#' Add and fill missing dates to sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Adds missing dates between earliest and latest date of a \code{sento_measures} object or two more extreme
#' boundary dates, such that the time series are continuous date-wise. Fills in any missing date with either 0 or the
#' most recent non-missing value.
#'
#' @details The \code{dateBefore} and \code{dateAfter} dates are converted according to the \code{sento_measures[["by"]]}
#' frequency.
#'
#' @param sento_measures a \code{sento_measures} object created using \code{\link{sento_measures}}.
#' @param fill an element of \code{c("zero", "latest")}; the first assumes missing dates represent zero sentiment,
#' the second assumes missing dates represent constant sentiment.
#' @param dateBefore a date as \code{"yyyy-mm-dd"}, to stretch the sentiment time series from up to the first date. Should
#' be earlier than \code{get_dates(sento_measures)[1]} to take effect. The values for these dates are set to those at
#' \code{get_dates(sento_measures)[1]}. If \code{NULL}, then ignored.
#' @param dateAfter a date as \code{"yyyy-mm-dd"}, to stretch the sentiment time series up to this date. Should be
#' later than \code{tail(get_dates(sento_measures), 1)} to take effect. If \code{NULL}, then ignored.
#'
#' @return A modified \code{sento_measures} object.
#'
#' @examples
#' # construct a sento_measures object to start with
#' corpus <- sento_corpus(corpusdf = sentometrics::usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 500)
#' l <- sento_lexicons(sentometrics::list_lexicons[c("LM_en", "HENRY_en")],
#'                     sentometrics::list_valence_shifters[["en"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "day", lag = 7, fill = "none")
#' sento_measures <- sento_measures(corpusSample, l, ctr)
#'
#' # fill measures
#' f1 <- measures_fill(sento_measures)
#' f2 <- measures_fill(sento_measures, fill = "latest")
#' f3 <- measures_fill(sento_measures, fill = "zero",
#'                     dateBefore = get_dates(sento_measures)[1] - 10,
#'                     dateAfter = tail(get_dates(sento_measures), 1) + 15)
#'
#' @export
measures_fill <- function(sento_measures, fill = "zero", dateBefore = NULL, dateAfter = NULL) {
  check_class(sento_measures, "sento_measures")

  by <- sento_measures$ctr$time$weightingParam$by
  dates <- get_dates(sento_measures)

  start <- dates[1]

  if (!is.null(dateBefore)) {
    dateBefore <- convert_date(dateBefore, by = by)
    if (dateBefore < start) start <- dateBefore
  }
  end <- utils::tail(dates, 1)

  if (!is.null(dateAfter)) {
    dateAfter <- convert_date(dateAfter, by = by)
    if (dateAfter > end) end <- dateAfter
  }

  ts <- seq.Date(start, end, by = by) # continuous date series
  dt <- data.table::data.table(date = ts)

  # join and fill as provided into new measures
  measures <- data.table::as.data.table(sento_measures)
  measuresFill <- merge(dt, measures, by = "date", all = TRUE) # fills with NA
  if (fill == "zero") {
    measuresFill[is.na(measuresFill)] <- 0
  } else if (fill == "latest") {
    if (!is.null(dateBefore)) measuresFill[1, 2:ncol(measures)] <- measures[1, -1]
    measuresFill <- cbind(dt, data.table::as.data.table(fill_NAs(as.matrix(measuresFill[, -1]))))
  } else stop("Input variable 'fill' should be either 'zero' or 'latest'.")
  measuresFill <- data.table::data.table(
    date = ts,
    measuresFill[, lapply(.SD, as.numeric), .SDcols = colnames(measures)[-1]]
  )

  sento_measures$measures <- measuresFill
  sento_measures$stats <- compute_stats(sento_measures) # will be overwritten at end of aggregate_time() call

  return(sento_measures)
}

check_agg_dimensions <- function(sento_measures, features = NULL, lexicons = NULL, time = NULL) {
  check_class(sento_measures, "sento_measures")

  if (is.null(features) && is.null(lexicons) && is.null(time))
    warning("No aggregation to perform. Input sento_measures object is returned.", call. = FALSE)

  # check if columns to aggregate exist (missings)
  # check if aggregations have at least two columns to combine and are unique (tooFew)
  missings <- tooFew <- NULL
  if (!is.null(features)) {
    missings <- c(missings, unlist(features)[!(unlist(features) %in% sento_measures$features)])
    for (i in seq_along(features)) {
      if (length(features[[i]]) <= 1 | length(unique(features[[i]])) != length(features[[i]]))
        tooFew <- c(tooFew, names(features)[i])
    }
  }
  if (!is.null(lexicons)) {
    missings <- c(missings, unlist(lexicons)[!(unlist(lexicons) %in% sento_measures$lexicons)])
    for (i in seq_along(lexicons)) {
      if (length(lexicons[[i]]) <= 1 | length(unique(lexicons[[i]])) != length(lexicons[[i]]))
        tooFew <- c(tooFew, names(lexicons)[i])
    }
  }
  if (!is.null(time)) {
    missings <- c(missings, unlist(time)[!(unlist(time) %in% sento_measures$time)])
    for (i in seq_along(time)) {
      if (length(time[[i]]) <= 1 | length(unique(time[[i]])) != length(time[[i]]))
        tooFew <- c(tooFew, names(time)[i])
    }
  }

  # assemble warning messages if any
  msg1 <- msg2 <- NULL
  if (length(missings) > 0) {
    msg1 <- paste0("Following columns to aggregate are not found: ",
                   paste0(missings, collapse = ", "), ".")
  }
  if (length(tooFew) > 0) {
    msg2 <- paste0("Following aggregations have less than two or not all unique columns: ",
                   paste0(tooFew, collapse = ", "), ".")
  }
  if (length(msg1) > 0 | length((msg2) > 0)) stop <- TRUE else stop <- FALSE

  return(list(stop = stop, msg1 = msg1, msg2 = msg2))
}

#' Update sentiment measures
#'
#' @author Jeroen Van Pelt, Samuel Borms, Andres Algaba
#'
#' @description Updates a \code{sento_measures} object based on a new \code{sento_corpus} provided.
#' Sentiment for the unseen corpus texts calculated and aggregated applying the control variables
#' from the input \code{sento_measures} object.
#'
#' @param sento_measures \code{sento_measures} object created with \code{\link{sento_measures}}
#' @param sento_corpus a \code{sento_corpus} object created with \code{\link{sento_corpus}}.
#' @param lexicons a \code{sento_lexicons} object created with \code{\link{sento_lexicons}}.
#
#' @return An updated \code{sento_measures} object.
#'
#' @seealso \code{\link{sento_measures}}, \code{\link{compute_sentiment}}
#'
#' @examples
#' data("usnews", package = "sentometrics")
#'
#' corpus1 <- sento_corpus(usnews[1:500, ])
#' corpus2 <- sento_corpus(usnews[400:2000, ])
#'
#' ctr <- ctr_agg(howTime = "linear", by = "year", lag = 3)
#' l <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")],
#'                     list_valence_shifters[["en"]])
#' sento_measures <- sento_measures(corpus1, l, ctr)
#' sento_measuresNew <- measures_update(sento_measures, corpus2, l)
#'
#' @export
measures_update <- function(sento_measures, sento_corpus, lexicons) {
  check_class(sento_measures, "sento_measures")
  check_class(sento_corpus, "sento_corpus")

  if (!setequal(get_dimensions(sento_measures)$lexicons, names(lexicons)[names(lexicons) != "valence"])) {
    stop(paste0("Provided lexicon names are not the same as lexicons used in input sento_measures object."))
  }

  ctr <- sento_measures$ctr
  sentiment <- sento_measures$sentiment
  partialCorpus <- quanteda::corpus_subset(sento_corpus, !quanteda::docnames(sento_corpus) %in% sentiment$id)
  if (quanteda::ndoc(partialCorpus) > 0) {
    partialSentiment <- compute_sentiment(partialCorpus, lexicons, how = ctr$within$howWithin, nCore = ctr$nCore)
    sentiment <- merge(sentiment, partialSentiment)
  }

  sento_measuresUpdated <- aggregate(sentiment, ctr)
  sento_measuresUpdated
}

