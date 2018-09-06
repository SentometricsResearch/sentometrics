
#' Set up control for aggregation into sentiment measures
#'
#' @author Samuel Borms, Keven Bluteau
#'
#' @description Sets up control object for aggregation of document-level textual sentiment into textual
#' sentiment measures (indices).
#'
#' @details For currently available options on how aggregation can occur (via the \code{howWithin},
#' \code{howDocs} and \code{howTime} arguments), call \code{\link{get_hows}}.
#'
#' @param howWithin a single \code{character} vector defining how aggregation within documents will be performed. Should
#' \code{length(howWithin) > 1}, the first element is used. For currently available options on how aggregation can occur; see
#' \code{\link{get_hows}()$words}.
#' @param howDocs a single \code{character} vector defining how aggregation across documents per date will be performed.
#' Should \code{length(howDocs) > 1}, the first element is used. For currently available options on how aggregation can occur;
#' see \code{\link{get_hows}()$docs}.
#' @param howTime a \code{character} vector defining how aggregation across dates will be performed. More than one choice
#' is possible. For currently available options on how aggregation can occur; see \code{\link{get_hows}()$time}.
#' @param do.ignoreZeros a \code{logical} indicating whether zero sentiment values have to be ignored in the determination of
#' the document weights while aggregating across documents. By default \code{do.ignoreZeros = TRUE}, such that documents with
#' a raw sentiment score of zero or for which a given feature indicator is equal to zero are considered irrelevant.
#' @param by a single \code{character} vector, either \code{"day", "week", "month"} or \code{"year"}, to indicate at what
#' level the dates should be aggregated. Dates are displayed as the first day of the period, if applicable (e.g.,
#' \code{"2017-03-01"} for March 2017).
#' @param lag a single \code{integer} vector, being the time lag to be specified for aggregation across time. By default
#' equal to \code{1L}, meaning no aggregation across time.
#' @param fill a single \code{character} vector, one of \code{c("zero", "latest", "none")}, to control how missing
#' sentiment values across the continuum of dates considered are added. This impacts the aggregation across time,
#' applying the \code{\link{measures_fill}} function before aggregating, except if \code{fill = "none"}. By default equal to
#' \code{"zero"}, which sets the scores (and thus also the weights) of the added dates to zero in the time aggregation.
#' @param alphasExp a \code{numeric} vector of all exponential smoothing factors to calculate weights for, used if
#' \code{"exponential" \%in\% howTime}. Values should be between 0 and 1 (both excluded); see \code{\link{exponentials}}.
#' @param ordersAlm a \code{numeric} vector of all Almon polynomial orders (positive) to calculate weights for, used if
#' \code{"almon" \%in\% howTime}; see \code{\link{almons}}.
#' @param do.inverseAlm a \code{logical} indicating if for every Almon polynomial its inverse has to be added, used
#' if \code{"almon" \%in\% howTime}; see \code{\link{almons}}.
#' @param aBeta a \code{numeric} vector of positive values as first Beta weighting decay parameter; see
#' \code{\link{betas}}.
#' @param bBeta a \code{numeric} vector of positive values as second Beta weighting decay parameter; see
#' \code{\link{betas}}.
#' @param weights optional own weighting scheme(s), used if provided as a \code{data.frame} with the number of rows
#' equal to the desired \code{lag}.
#' @param tokens see \code{\link{compute_sentiment}}.
#' @param nCore see \code{\link{compute_sentiment}}.
#' @param ... not used.
#
#' @return A \code{list} encapsulating the control parameters.
#'
#' @seealso \code{\link{measures_fill}}, \code{\link{almons}}, \code{\link{compute_sentiment}}
#'
#' @examples
#' # simple control function
#' ctr1 <- ctr_agg(howTime = "linear", by = "year", lag = 3)
#'
#' # more elaborate control function (particular attention to time weighting schemes)
#' ctr2 <- ctr_agg(howWithin = "proportionalPol",
#'                 howDocs = "proportional",
#'                 howTime = c("equal_weight", "linear", "almon", "beta", "exponential", "own"),
#'                 do.ignoreZeros = TRUE,
#'                 by = "day",
#'                 lag = 20,
#'                 ordersAlm = 1:3,
#'                 do.inverseAlm = TRUE,
#'                 alphasExp = c(0.20, 0.50, 0.70, 0.95),
#'                 aBeta = c(1, 3),
#'                 bBeta = c(1, 3, 4, 7),
#'                 weights = data.frame(myWeights = runif(20)))
#'
#' # set up control function with one linear and two chosen Almon weighting schemes
#' a <- almons(n = 70, orders = 1:3, do.inverse = TRUE, do.normalize = TRUE)
#' ctr3 <- ctr_agg(howTime = c("linear", "own"), by = "year", lag = 70,
#'                 weights = data.frame(a1 = a[, 1], a2 = a[, 3]))
#'
#' @export
ctr_agg <- function(howWithin = "proportional", howDocs = "equal_weight", howTime = "equal_weight",
                    do.ignoreZeros = TRUE, by = "day", lag = 1L, fill = "zero", alphasExp = seq(0.1, 0.5, by = 0.1),
                    ordersAlm = 1:3, do.inverseAlm = TRUE, aBeta = 1:4, bBeta = 1:4, weights = NULL,
                    tokens = NULL, nCore = 2, ...) {

  if (length(howWithin) > 1) howWithin <- howWithin[1]
  if (length(howDocs) > 1) howDocs <- howDocs[1]

  # check if provided aggregation specifications are supported
  hows <- get_hows() # get all supported options for each aggregation level
  err <- NULL
  if (!(howWithin %in% hows[["words"]])) {
    err <- c(err, paste0(howWithin, " is no current option for aggregation across words."))
  }
  if (!(howDocs %in% hows[["docs"]])) {
    err <- c(err, paste0(howDocs, " is no current option for aggregation across docs."))
  }
  if (!all(howTime %in% hows[["time"]])) {
    err <- c(err, paste0(howTime[!(howTime %in% hows[["time"]])], " is no current option for aggregation across time. "))
  }
  if ("own" %in% howTime && is.null(weights)) {
    err <- c(err, "Provide a 'weights' data.frame if 'own' provided as an option in 'howTime'.")
  }
  if (!("own" %in% howTime) && is.data.frame(weights)) {
    howTime <- c(howTime, "own")
    warning("The option 'own' is added to 'howTime' since a valid (not NULL) 'weights' data.frame was supplied.")
  }
  if ("own" %in% howTime) {
    if (lag != nrow(weights)) {
      lag <- nrow(weights)
      warning("Argument 'lag' is set equal to the number of rows in the 'weights' data.frame.")
    }
    if (!is_names_correct(colnames(weights))) {
      err <- c(err, "The column names in the 'weights' data.frame should not contain any '-'.")
    }
  }
  if ("almon" %in% howTime && any(ordersAlm <= 0)) {
    err <- c(err, "Values in 'ordersAlm' should be positive.")
  }
  if ("beta" %in% howTime && any(c(aBeta, bBeta) <= 0)) {
    err <- c(err, "Values in 'aBeta' and 'bBeta' should be positive.")
  }
  if ("exponential" %in% howTime && max(alphasExp) >= 1 || min(alphasExp) <= 0) {
    err <- c(err, "Values in 'alphasExp' should be between 0 and 1 (both excluded).")
  }
  if (lag <= 0) {
    err <- c(err, "Argument 'lag' should be greater than zero.")
  }
  if (!(by %in% c("year", "month", "week", "day"))) {
    err <- c(err, paste0(by, " is no current 'by' option."))
  }
  if (!(fill %in% c("zero", "latest", "none"))) {
    err <- c(err, paste0(fill, " is no current 'fill' option."))
  }
  if (length(nCore) != 1 || !is.numeric(nCore)) {
    err <- c(err, "The 'nCore' argument should be a numeric vector of size one.")
  } else nCore <- check_nCore(nCore)
  if (!is.null(tokens) && !is.list(tokens)) {
    err <- c(err, "The 'tokens' argument, if not NULL, must be a list.")
  }
  if (!is.null(err)) stop("Wrong inputs. See below for specifics. \n", paste0(err, collapse = "\n"))

  other <- list(alphasExp = alphasExp, ordersAlm = ordersAlm, do.inverseAlm = do.inverseAlm,
                aBeta = aBeta, bBeta = bBeta, weights = weights)

  ctr <- list(howWithin = howWithin,
              howDocs = howDocs,
              howTime = howTime,
              do.ignoreZeros = do.ignoreZeros,
              by = by,
              lag = lag,
              fill = fill,
              tokens = tokens,
              nCore = nCore,
              other = other)

  return(ctr)
}

#' One-way road towards a sentomeasures object
#'
#' @author Samuel Borms, Keven Bluteau
#'
#' @description Wrapper function which assembles calls to \code{\link{compute_sentiment}} and \code{\link{perform_agg}}, and
#' includes the input \code{sentocorpus} and computed sentiment scores in its output. Serves as the most direct way towards a
#' panel of textual sentiment measures as a \code{sentomeasures} object.
#'
#' @details As a general rule, neither the names of the features, lexicons or time weighting schemes may contain
#' any '-' symbol.
#'
#' @param sentocorpus a \code{sentocorpus} object created with \code{\link{sento_corpus}}.
#' @param lexicons a \code{sentolexicons} object created with \code{\link{setup_lexicons}}.
#' @param ctr output from a \code{\link{ctr_agg}} call.
#'
#' @return A \code{sentomeasures} object, which is a \code{list} containing:
#' \item{measures}{a \code{data.table} with a \code{"date"} column and all textual sentiment measures as remaining columns.}
#' \item{features}{a \code{character} vector of the different features.}
#' \item{lexicons}{a \code{character} vector of the different lexicons used.}
#' \item{time}{a \code{character} vector of the different time weighting schemes used.}
#' \item{by}{a single \code{character} vector specifying the time interval of aggregation used.}
#' \item{stats}{a \code{data.frame} with a series of elementary statistics (mean, standard deviation, maximum, minimum, and
#' average correlation with all other measures) for each individual sentiment measure.}
#' \item{sentiment}{the sentiment scores \code{data.table} with \code{"date"}, \code{"word_count"} and lexicon--feature
#' sentiment scores columns.
#' If \code{ctr$do.ignoreZeros = TRUE}, all zeros are replaced by \code{NA}.}
#' \item{howWithin}{a single \code{character} vector to remind how sentiment within documents was aggregated.}
#' \item{howDocs}{a single \code{character} vector to remind how sentiment across documents was aggregated.}
#' \item{fill}{a single \code{character} vector that specifies if and how missing dates have been added before
#' aggregation across time was carried out.}
#' \item{do.ignoreZeros}{a single \code{character} vector to remind if documents with a zero feature-sentiment score
#' have been ignored in the within-document aggregation.}
#' \item{attribWeights}{a \code{list} of document and time weights used in the \code{\link{retrieve_attributions}} function.
#' Serves further no direct purpose.}
#'
#' @seealso \code{\link{compute_sentiment}}, \code{\link{perform_agg}}
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
#' ctr <- ctr_agg(howWithin = "counts",
#'                howDocs = "proportional",
#'                howTime = c("equal_weight", "linear", "almon"),
#'                by = "month",
#'                lag = 3,
#'                ordersAlm = 1:3,
#'                do.inverseAlm = TRUE)
#' sentomeasures <- sento_measures(corpusSample, l, ctr)
#' summary(sentomeasures)
#'
#' @import data.table
#' @export
sento_measures <- function(sentocorpus, lexicons, ctr) {
  check_class(sentocorpus, "sentocorpus")
  toAgg <- compute_sentiment(sentocorpus, lexicons, how = ctr$howWithin, tokens = ctr$tokens, nCore = ctr$nCore)
  sentomeasures <- perform_agg(toAgg, ctr)
  return(sentomeasures)
}

#' Aggregate textual sentiment across documents and time
#'
#' @author Samuel Borms, Keven Bluteau
#'
#' @description Condenses document-level textual sentiment scores into a panel of textual sentiment
#' measures by aggregating across documents and time. This function is called within \code{\link{sento_measures}},
#' applied on the output of \code{\link{compute_sentiment}}.
#'
#' @param sentiment output from a \code{\link{compute_sentiment}} call, computed from a \code{sentocorpus} object.
#' @param ctr output from a \code{\link{ctr_agg}} call. The \code{howWithin} and \code{nCore} arguments are ignored.
#'
#' @return A \code{sentomeasures} object.
#'
#' @seealso \code{\link{compute_sentiment}}, \code{\link{ctr_agg}}, \code{\link{sento_measures}}
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' # computation of sentiment and aggregation into sentiment measures
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 500)
#' l <- setup_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#' sent <- compute_sentiment(corpusSample, l, how = "counts")
#' ctr <- ctr_agg(howTime = c("linear"), by = "year", lag = 3)
#' sentomeasures <- perform_agg(sent, ctr)
#'
#' @export
perform_agg <- function(sentiment, ctr) {
  toAgg <- sentiment
  if (!inherits(toAgg[["corpus"]], "sentocorpus"))
    stop("The 'sentiment' argument should be computed from a sentocorpus object, i.e., include a date dimension.")
  howDocs <- ctr$howDocs
  howTime <- ctr$howTime
  do.ignoreZeros <- ctr$do.ignoreZeros
  by <- ctr$by
  lag <- ctr$lag
  fill <- ctr$fill
  otherVars <- ctr$other # list or empty
  aggDocs <- aggregate_docs(toAgg, by = by, how = howDocs, do.ignoreZeros = do.ignoreZeros)
  sentomeasures <- aggregate_time(aggDocs, lag = lag, fill = fill, how = howTime, otherVars)
  return(sentomeasures)
}

aggregate_docs <- function(toAgg, by, how = get_hows()$docs, do.ignoreZeros = TRUE) {

  features <- toAgg$features
  lexNames <- toAgg$lexicons
  sent <- toAgg$sentiment
  attribWeights <- list(W = NA, B = NA) # list with weights useful in later attribution analysis

  # reformat dates so they can be aggregated at the specified 'by' level, and cast to Date format
  if (by == "year") {
    years <- sapply(stringi::stri_split(sent$date, regex = "-"), "[", 1)
    dates <- as.Date(paste0(years, "-01-01"), format = "%Y-%m-%d")
  } else if (by == "month") {
    months <- unlist(lapply(stringi::stri_split(sent$date, regex = "-"), function(d) return(paste0(d[1:2], collapse = "-"))))
    dates <- as.Date(paste0(months, "-01"), format = "%Y-%m-%d")
  } else if (by == "week") {
    weeks <- ISOweek::ISOweek(sent$date)
    dates <- ISOweek::ISOweek2date(paste(weeks, 1, sep = "-")) # get first day of week based on ISO standard
  } else {
    dates <- as.Date(sent$date, format = "%Y-%m-%d")
  }
  sent$date <- dates

  # ignore documents with zero sentiment in aggregation (if do.ignoreZeros is TRUE)
  if (do.ignoreZeros == TRUE)
    sent[, names(sent)] <- sent[, names(sent), with = FALSE][, lapply(.SD, function(x) replace(x, which(x == 0), NA))]

  # aggregate feature-sentiment per document by date for all lexicon columns
  s <- sent[, -1]
  if (how == "equal_weight") {
    if (do.ignoreZeros == TRUE) {
      docsIn <- s[, lapply(.SD, function(x) (x * 1) / x), by = date] # indicator of 1 if document score not equal to NA
      weights <- docsIn[, lapply(.SD, function(x) x / sum(x, na.rm = TRUE)), by = date][, -1:-2]
    } else {
      weights <- s[, w := 1 / .N, by = date][, "w"]
      weights <- weights[, colnames(s)[-1:-2] := weights][, -1] # drop w column
      s[, w := NULL]
    }
  } else if (how == "proportional") { # proportional w.r.t. words in document vs. total words in all documents per date
    if (do.ignoreZeros == TRUE) {
      docsIn <- s[, lapply(.SD, function(x) (x * word_count) / x), by = date]
      weights <- docsIn[, lapply(.SD, function(x) x / sum(x, na.rm = TRUE)), by = date][, -1:-2]
    } else {
      weights <- s[, word_count / sum(word_count, na.rm = TRUE), by = date][, 2]
      weights <- weights[, colnames(s)[-1:-2] := weights][, -1]
    }
  }
  attribWeights[["W"]] <- data.table(id = sent$id, date = sent$date, weights)
  sw <- data.table(date = s$date, s[, -1:-2] * weights)
  measures <- sw[, lapply(.SD, function(x) sum(x, na.rm = TRUE)), by = date]

  sentomeasures <- list(measures = measures,
                        features = features,
                        lexicons = lexNames,
                        time = NA,
                        by = by,
                        stats = NA,
                        sentiment = sent, # zeros replaced by NAs if do.ignoreZeros = TRUE
                        howWithin = toAgg$howWithin,
                        howDocs = how,
                        fill = NA,
                        do.ignoreZeros = do.ignoreZeros,
                        attribWeights = attribWeights)

  class(sentomeasures) <- "sentomeasures"

  return(sentomeasures)
}

aggregate_time <- function(sentomeasures, lag, fill, how = get_hows()$time, ...) {
  check_class(sentomeasures, "sentomeasures")

  dots <- tryCatch(list(...)[[1]], # extract list from list of list
                   error = function(x) list(...)) # if ... is empty

  # construct all weights and check for duplicated names
  weights <- setup_time_weights(lag, how, dots)
  if (sum(duplicated(colnames(weights))) > 0) {
    duplics <- unique(colnames(weights)[duplicated(colnames(weights))])
    stop(paste0("Names of weighting schemes are not unique. Following names occur at least twice: ",
                paste0(duplics, collapse = ", ")))
  }
  sentomeasures$attribWeights[["B"]] <- copy(weights)

  # apply rolling time window, if not too large, for every weights column and combine all new measures column-wise
  if (!(fill %in% "none")) sentomeasures <- measures_fill(sentomeasures, fill = fill)
  measures <- get_measures(sentomeasures)
  toRoll <- measures[, -1]
  m <- nrow(measures)
  if (lag > m)
    stop("Rolling time aggregation window (= ", lag, ") is too large for number of observations per measure (= ", m, ")")
  for (i in 1:ncol(weights)) {
    name <- colnames(weights)[i]
    add <- RcppRoll::roll_sum(as.matrix(toRoll), n = lag, weights = as.vector(weights[, i]),
                              normalize = FALSE, align = "right", na.rm = TRUE)
    colnames(add) <- paste0(colnames(toRoll), "--", name)
    if (i == 1) measuresAggTime <- add
    else measuresAggTime <- cbind(measuresAggTime, add)
  }
  measuresAggTime <- as.data.table(measuresAggTime)
  if (lag > 1) date <- measures$date[-1:-(lag-1)]
  else date <- measures$date
  measuresAggTime$date <- date
  measuresAggTime <- setcolorder(measuresAggTime, c("date", colnames(measuresAggTime)[-ncol(measuresAggTime)]))

  sentomeasures$measures <- measuresAggTime
  sentomeasures$time <- colnames(weights)
  sentomeasures$stats <- compute_stats(sentomeasures)
  sentomeasures$fill <- fill

  return(sentomeasures)
}

#' Extract dates and documents related to sentiment peaks
#'
#' @author Samuel Borms
#'
#' @description This function extracts the dates and documents for which aggregated sentiment is most
#' extreme (lowest, highest or both in absolute terms). The extracted dates are unique, even when,
#' for example, all most extreme sentiment values (for different sentiment measures) occur on only
#' one date.
#'
#' @param sentomeasures a \code{sentomeasures} object created using \code{\link{sento_measures}}.
#' @param sentocorpus the \code{sentocorpus} object created with \code{\link{sento_corpus}}, used for the construction
#' of the input \code{sentomeasures} object.
#' @param n a \code{numeric} value to indicate the number of dates associated to sentiment peaks to extract.
#' @param type a \code{character} value, either \code{"pos"}, \code{"neg"} or \code{"both"}, respectively to look
#' for the \code{n} dates related to the most positive, most negative or most extreme (in absolute terms) sentiment
#' occurrences.
#' @param do.average a \code{logical} to indicate whether peaks should be selected based on the average sentiment
#' value per date.
#'
#' @return A \code{list} with as elements \code{"dates"}, \code{"ids"} and \code{"documents"}, corresponding to
#' the \code{n} extracted sentiment peak dates and associated document ids and texts.
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
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "month", lag = 3)
#' sentomeasures <- sento_measures(corpusSample, l, ctr)
#'
#' # extract the peaks
#' peaksAbs <- peakdocs(sentomeasures, corpus, n = 5)
#' peaksPos <- peakdocs(sentomeasures, corpus, n = 5, type = "pos")
#' peaksNeg <- peakdocs(sentomeasures, corpus, n = 5, type = "neg")
#'
#' @export
peakdocs <- function(sentomeasures, sentocorpus, n = 10, type = "both", do.average = FALSE) {
  check_class(sentomeasures, "sentomeasures")

  measures <- get_measures(sentomeasures)[, -1] # drop dates
  m <- nmeasures(sentomeasures)
  if (n >= (nobs(sentomeasures) * m)) stop("The parameter 'n' exceeds the total number of sentiment values.")
  if (do.average == TRUE) {
    measures <- rowMeans(measures, na.rm = TRUE)
    dates <- get_dates(sentomeasures)
  } else dates <- rep(get_dates(sentomeasures), m)
  if (type == "both") measures <- abs(measures)
  indx <- order(measures, decreasing = ifelse(type == "neg", FALSE, TRUE))[1:(m * n)]
  peakDates <- unique(dates[indx])[1:n]
  ids <- sentomeasures$sentiment[date %in% peakDates, ]$id # get document IDs
  peakDocs <- quanteda::texts(sentocorpus)[row.names(sentocorpus$documents) %in% ids]
  peaks <- list(dates = peakDates, ids = ids, docs = peakDocs)
  return(peaks)
}

