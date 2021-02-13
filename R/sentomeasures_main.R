
#' Set up control for aggregation into sentiment measures
#'
#' @author Samuel Borms, Keven Bluteau
#'
#' @description Sets up control object for (computation of textual sentiment and) aggregation into textual
#' sentiment measures.
#'
#' @details For available options on how aggregation can occur (via the \code{howWithin},
#' \code{howDocs} and \code{howTime} arguments), inspect \code{\link{get_hows}}. The control parameters
#' associated to \code{howDocs} are used both for aggregation across documents and across sentences.
#'
#' @param howWithin a single \code{character} vector defining how to perform aggregation within
#' documents or sentences. Coincides with the \code{how} argument in the \code{\link{compute_sentiment}} function. Should
#' \code{length(howWithin) > 1}, the first element is used. For available options see \code{\link{get_hows}()$words}.
#' @param howDocs a single \code{character} vector defining how aggregation across documents (and/or sentences) per date will
#' be performed. Should \code{length(howDocs) > 1}, the first element is used. For available options
#' see \code{\link{get_hows}()$docs}.
#' @param howTime a \code{character} vector defining how aggregation across dates will be performed. More than one choice
#' is possible. For available options see \code{\link{get_hows}()$time}.
#' @param do.ignoreZeros a \code{logical} indicating whether zero sentiment values have to be ignored in the determination of
#' the document (and/or sentence) weights while aggregating across documents (and/or sentences). By default
#' \code{do.ignoreZeros = TRUE}, such that documents (and/or sentences) with a raw sentiment score of zero or for which
#' a given feature indicator is equal to zero are considered irrelevant.
#' @param by a single \code{character} vector, either \code{"day", "week", "month"} or \code{"year"}, to indicate at what
#' level the dates should be aggregated. Dates are displayed as the first day of the period, if applicable (e.g.,
#' \code{"2017-03-01"} for March 2017).
#' @param lag a single \code{integer} vector, being the time lag to be specified for aggregation across time. By default
#' equal to \code{1}, meaning no aggregation across time; a time weighting scheme named \code{"dummyTime"} is used in
#' this case.
#' @param fill a single \code{character} vector, one of \code{c("zero", "latest", "none")}, to control how missing
#' sentiment values across the continuum of dates considered are added. This impacts the aggregation across time,
#' applying the \code{\link{measures_fill}} function before aggregating, except if \code{fill = "none"}. By default equal to
#' \code{"zero"}, which sets the scores (and thus also the weights) of the added dates to zero in the time aggregation.
#' @param alphasExp a \code{numeric} vector of all exponential weighting smoothing factors, used if \cr
#' \code{"exponential" \%in\% howTime}. Values should be between 0 and 1 (both excluded); see
#' \code{\link{weights_exponential}}.
#' @param ordersAlm a \code{numeric} vector of all Almon polynomial orders (positive) to calculate weights for, used if
#' \code{"almon" \%in\% howTime}; see \code{\link{weights_almon}}.
#' @param do.inverseAlm a \code{logical} indicating if for every Almon polynomial its inverse has to be added, used
#' if \code{"almon" \%in\% howTime}; see \code{\link{weights_almon}}.
#' @param aBeta a \code{numeric} vector of positive values as first Beta weighting decay parameter; see
#' \code{\link{weights_beta}}.
#' @param bBeta a \code{numeric} vector of positive values as second Beta weighting decay parameter; see
#' \code{\link{weights_beta}}.
#' @param weights optional own weighting scheme(s), used if provided as a \code{data.frame} with the number of rows
#' equal to the desired \code{lag}.
#' @param tokens see \code{\link{compute_sentiment}}.
#' @param nCore see \code{\link{compute_sentiment}}.
#' @param alphaExpDocs a single \code{integer} vector. A weighting smoothing factor, used if \cr
#' \code{"exponential" \%in\% howDocs} or \code{"inverseExponential" \%in\% howDocs}. Value should be between 0 and 1
#' (both excluded); see \code{\link{weights_exponential}}.
#' @param do.sentence see \code{\link{compute_sentiment}}.
#' @param do.inverseExp a \code{logical} indicating if for every exponential curve its inverse has to be added,
#' used if \code{"exponential" \%in\% howTime}; see \code{\link{weights_exponential}}.
#
#' @return A \code{list} encapsulating the control parameters.
#'
#' @seealso \code{\link{measures_fill}}, \code{\link{almons}}, \code{\link{compute_sentiment}}
#'
#' @examples
#' set.seed(505)
#'
#' # simple control function
#' ctr1 <- ctr_agg(howTime = "linear", by = "year", lag = 3)
#'
#' # more elaborate control function (particular attention to time weighting schemes)
#' ctr2 <- ctr_agg(howWithin = "proportionalPol",
#'                 howDocs = "exponential",
#'                 howTime = c("equal_weight", "linear", "almon", "beta", "exponential", "own"),
#'                 do.ignoreZeros = TRUE,
#'                 by = "day",
#'                 lag = 20,
#'                 ordersAlm = 1:3,
#'                 do.inverseAlm = TRUE,
#'                 alphasExp = c(0.20, 0.50, 0.70, 0.95),
#'                 aBeta = c(1, 3),
#'                 bBeta = c(1, 3, 4, 7),
#'                 weights = data.frame(myWeights = runif(20)),
#'                 alphaExp = 0.3)
#'
#' # set up control function with one linear and two chosen Almon weighting schemes
#' a <- weights_almon(n = 70, orders = 1:3, do.inverse = TRUE, do.normalize = TRUE)
#' ctr3 <- ctr_agg(howTime = c("linear", "own"), by = "year", lag = 70,
#'                 weights = data.frame(a1 = a[, 1], a2 = a[, 3]),
#'                 do.sentence = TRUE)
#'
#' @export
ctr_agg <- function(howWithin = "proportional", howDocs = "equal_weight", howTime = "equal_weight",
                    do.sentence = FALSE, do.ignoreZeros = TRUE, by = "day", lag = 1, fill = "zero",
                    alphaExpDocs = 0.1, alphasExp = seq(0.1, 0.5, by = 0.1), do.inverseExp = FALSE,
                    ordersAlm = 1:3, do.inverseAlm = TRUE, aBeta = 1:4, bBeta = 1:4, weights = NULL,
                    tokens = NULL, nCore = 1) {

  if (length(howWithin) > 1) howWithin <- howWithin[1]
  if (length(howDocs) > 1) howDocs <- howDocs[1]

  # check if provided aggregation specifications are supported
  hows <- get_hows() # get all supported options for each aggregation level
  err <- NULL
  if (!(howWithin %in% hows[["words"]])) {
    err <- c(err, paste0(howWithin, " is no current option for aggregation across words."))
  }
  if (!(howDocs %in% hows[["docs"]])) {
    err <- c(err, paste0(howDocs, " is no current option for aggregation across documents."))
  }
  if (lag == 1) {
    message("The choice 'lag = 1' implies no time aggregation, so we added a dummy weighting scheme 'dummyTime'.")
    howTime <- "own"
    weights <- data.frame(dummyTime = 1)
  }
  if (!all(howTime %in% hows[["time"]])) {
    err <- c(err, paste0(howTime[!(howTime %in% hows[["time"]])], " is no current option for aggregation across time. "))
  }
  if ("own" %in% howTime && is.null(weights)) {
    err <- c(err, "Provide a 'weights' data.frame if 'own' provided as an option in 'howTime'.")
  }
  if (!("own" %in% howTime) && is.data.frame(weights)) {
    howTime <- c(howTime, "own")
    message("Option 'own' is added to 'howTime' because a 'weights' data.frame was supplied.")
  }
  if ("own" %in% howTime) {
    if (lag != nrow(weights)) {
      lag <- nrow(weights)
      message("Argument 'lag' is set to the number of rows in the 'weights' data.frame.")
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
  if (howDocs == "exponential" || howDocs == "inverseExponential") {
    if (alphaExpDocs >= 1 || alphaExpDocs <= 0) {
      err <- c(err, "Alpha must be a number between 0 and 1 (both excluded).")
    }
  }
  if (!is.logical(do.sentence)) {
    err <- c(err, "Argument 'do.sentence' should be a logical.")
  }
  if (!is.logical(do.inverseAlm)) {
    err <- c(err, "Argument 'do.inverseAlm' should be a logical.")
  }
  if (!is.logical(do.inverseExp)) {
    err <- c(err, "Argument 'do.inverseExp' should be a logical.")
  }
  if (!is.null(err)) stop("Wrong inputs. See below for specifics. \n", paste0(err, collapse = "\n"))

  ctr <- list(within = list(howWithin = howWithin, do.sentence = do.sentence),
              docs = list(howDocs = howDocs,
                          weightingParam = list(alphaExpDocs = alphaExpDocs,
                                                do.ignoreZeros = do.ignoreZeros)),
              time = list(howTime = howTime,
                          weightingParam = list(by = by,
                                                lag = lag,
                                                fill = fill,
                                                ordersAlm = ordersAlm,
                                                do.inverseAlm = do.inverseAlm,
                                                aBeta = aBeta,
                                                bBeta = bBeta,
                                                alphasExp = alphasExp,
                                                do.inverseExp = do.inverseExp,
                                                weights = weights)),
              tokens = tokens,
              nCore = nCore)

  return(ctr)
}

#' One-way road towards a sento_measures object
#'
#' @author Samuel Borms, Keven Bluteau
#'
#' @description Wrapper function which assembles calls to \code{\link{compute_sentiment}} and \code{\link{aggregate}}.
#' Serves as the most direct way towards a panel of textual sentiment measures as a \code{sento_measures} object.
#'
#' @details As a general rule, neither the names of the features, lexicons or time weighting schemes may contain
#' any `-' symbol.
#'
#' @param sento_corpus a \code{sento_corpus} object created with \code{\link{sento_corpus}}.
#' @param lexicons a \code{sentolexicons} object created with \code{\link{sento_lexicons}}.
#' @param ctr output from a \code{\link{ctr_agg}} call.
#'
#' @return A \code{sento_measures} object, which is a \code{list} containing:
#' \item{measures}{a \code{data.table} with a \code{"date"} column and all textual sentiment measures as remaining columns.}
#' \item{features}{a \code{character} vector of the different features.}
#' \item{lexicons}{a \code{character} vector of the different lexicons used.}
#' \item{time}{a \code{character} vector of the different time weighting schemes used.}
#' \item{stats}{a \code{data.frame} with some elementary statistics (mean, standard deviation, maximum, minimum, and
#' average correlation with the other measures) for each individual sentiment measure. In all computations, NAs are
#' removed first.}
#' \item{sentiment}{the document-level sentiment scores \code{data.table} with \code{"date"},
#' \code{"word_count"} and lexicon-feature sentiment scores columns. The \code{"date"} column has the
#' dates converted at the frequency for across-document aggregation. All zeros are replaced by \code{NA}
#' if \code{ctr$docs$weightingParam$do.ignoreZeros = TRUE}.}
#' \item{attribWeights}{a \code{list} of document and time weights used in the \code{\link{attributions}} function.
#' Serves further no direct purpose.}
#' \item{ctr}{a \code{list} encapsulating the control parameters.}
#'
#' @seealso \code{\link{compute_sentiment}}, \code{\link{aggregate}}, \code{\link{measures_update}}
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' # construct a sento_measures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 500)
#' l <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#' ctr <- ctr_agg(howWithin = "counts",
#'                howDocs = "proportional",
#'                howTime = c("equal_weight", "linear", "almon"),
#'                by = "month",
#'                lag = 3,
#'                ordersAlm = 1:3,
#'                do.inverseAlm = TRUE)
#' sento_measures <- sento_measures(corpusSample, l, ctr)
#' summary(sento_measures)
#'
#' @import data.table
#' @export
sento_measures <- function(sento_corpus, lexicons, ctr) {
  check_class(sento_corpus, "sento_corpus")
  sentiment <- compute_sentiment(sento_corpus, lexicons, how = ctr$within$howWithin, tokens = ctr$tokens,
                                 do.sentence = ctr$within$do.sentence, nCore = ctr$nCore)
  sento_measures <- aggregate(sentiment, ctr)
  return(sento_measures)
}

#' Aggregate textual sentiment across sentences, documents and time
#'
#' @author Samuel Borms, Keven Bluteau
#'
#' @description Aggregates textual sentiment scores at sentence- or document-level into a panel of textual
#' sentiment measures. Can also be used to aggregate sentence-level sentiment scores into
#' document-level sentiment scores. This function is called within the \code{\link{sento_measures}} function.
#'
#' @param x a \code{sentiment} object created using \code{\link{compute_sentiment}} (from a \code{sento_corpus}
#' object) or using \code{\link{as.sentiment}}.
#' @param ctr output from a \code{\link{ctr_agg}} call. The \code{howWithin} and \code{nCore} elements are ignored.
#' @param do.full if \code{do.full = TRUE} (by default), does entire aggregation up to a \code{sento_measures}
#' object, else only goes from sentence-level to document-level. Ignored if no \code{"sentence_id"} column in
#' \code{sentiment} input object.
#' @param ... not used.
#'
#' @return A document-level \code{sentiment} object or a fully aggregated \code{sento_measures} object.
#'
#' @seealso \code{\link{compute_sentiment}}, \code{\link{ctr_agg}}, \code{\link{sento_measures}}
#'
#' @examples
#' set.seed(505)
#'
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' # computation of sentiment
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 500)
#' l1 <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")],
#'                      list_valence_shifters[["en"]])
#' l2 <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")],
#'                      list_valence_shifters[["en"]][, c("x", "t")])
#' sent1 <- compute_sentiment(corpusSample, l1, how = "counts")
#' sent2 <- compute_sentiment(corpusSample, l2, do.sentence = TRUE)
#' sent3 <- compute_sentiment(quanteda::texts(corpusSample), l2,
#'                            do.sentence = TRUE)
#' ctr <- ctr_agg(howTime = c("linear"), by = "year", lag = 3)
#'
#' # aggregate into sentiment measures
#' sm1 <- aggregate(sent1, ctr)
#' sm2 <- aggregate(sent2, ctr)
#'
#' # two-step aggregation (first into document-level sentiment)
#' sd2 <- aggregate(sent2, ctr, do.full = FALSE)
#' sm3 <- aggregate(sd2, ctr)
#'
#' # aggregation of a sentiment data.table
#' cols <- c("word_count", names(l2)[-length(l2)])
#' sd3 <- sent3[, lapply(.SD, sum), by = "id", .SDcols = cols]
#'
#' @importFrom stats aggregate
#' @export
aggregate.sentiment <- function(x, ctr, do.full = TRUE, ...) {
  stopifnot(is.logical(do.full))

  howDocs <- ctr$docs$howDocs
  howTime <- ctr$time$howTime
  howWithin <- ctr$within$howWithin
  weightingParamDocs <- ctr$docs$weightingParam
  weightingParamTime <- ctr$time$weightingParam
  by <- weightingParamTime$by

  cols <- colnames(x)
  if ("sentence_id" %in% cols) {
    x <- aggregate_sentences(x, how = howDocs, weightingParamDocs = weightingParamDocs)
    if (do.full == FALSE) return(x)
    # if (do.full == TRUE && !("date" %in% cols)) {
    #   warning("Aggregation only performed until document-level, since no 'date' column present.")
    #   return(x)
    # }
  }
  # if (!("date" %in% cols))
  #  stop("A document-level sentiment input should have a 'date' column for full aggregation.")

  aggDocs <- aggregate_docs(x, by = by, how = howDocs, weightingParamDocs = weightingParamDocs)
  aggDocs$ctr <- ctr
  sento_measures <- aggregate_time(aggDocs, how = howTime, weightingParamTime = weightingParamTime)

  sento_measures
}

aggregate_sentences <- function(sentiment, how, weightingParamDocs) {
  wc <- sentiment[, .(word_count)]
  do.ignoreZeros <- weightingParamDocs$do.ignoreZeros
  alphaExpDocs <- weightingParamDocs$alphaExpDocs

  weights <- weights_across(sentiment, how, do.ignoreZeros, alphaExpDocs, by = "id")

  if ("date" %in% colnames(sentiment)) { # inherits(sentiment, "sentiment")
    sw <- data.table::data.table(id = sentiment[["id"]], sentiment[, .(date)], wc, sentiment[, -1:-4] * weights)
    s <- sw[, lapply(.SD, sum, na.rm = TRUE), by = c("id", "date")] # assumes all id and date combinations are unique
    class(s) <- c("sentiment", class(s))
  } else {
    sw <- data.table::data.table(id = sentiment[["id"]], wc, sentiment[, -1:-3] * weights)
    s <- sw[, lapply(.SD, sum, na.rm = TRUE), by = "id"]
  }

  s
}

aggregate_docs <- function(sent, by, how = get_hows()$docs, weightingParamDocs) {

  names <- stringi::stri_split(colnames(sent)[4:ncol(sent)], regex = "--")
  lexNames <- unique(sapply(names, "[", 1))
  features <- unique(sapply(names, "[", 2))

  data.table::setorder(sent, date, na.last = FALSE)
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

  do.ignoreZeros <- weightingParamDocs$do.ignoreZeros
  if (do.ignoreZeros == TRUE) { # ignore documents with zero sentiment in aggregation
    nms <- names(sent)[-c(1:3)]
    sent[, nms] <-
      sent[, lapply(.SD, function(x) replace(x, which(x == 0), NA)), .SDcols = nms]
  }

  alphaExpDocs <- weightingParamDocs$alphaExpDocs
  weights <- weights_across(sent, how, do.ignoreZeros, alphaExpDocs, by = "date")
  s <- sent[, !"id"]
  attribWeights[["W"]] <- data.table::data.table(id = sent$id, date = sent$date, weights)
  sw <- data.table::data.table(date = s$date, s[, -c(1:2)] * weights)
  measures <- sw[, lapply(.SD, function(x) sum(x, na.rm = TRUE)), by = date]

  sento_measures <- list(measures = measures,
                         features = features,
                         lexicons = lexNames,
                         time = NA,
                         sentiment = sent, # zeros replaced by NAs if do.ignoreZeros = TRUE
                         stats = NA,
                         attribWeights = attribWeights)

  class(sento_measures) <- "sento_measures"

  return(sento_measures)
}

aggregate_time <- function(sento_measures, how = get_hows()$time, weightingParamTime) {
  check_class(sento_measures, "sento_measures")
  lag <- weightingParamTime$lag
  fill <- weightingParamTime$fill
  weights <- setup_time_weights(how, weightingParamTime) # construct all weights
  if (sum(duplicated(colnames(weights))) > 0) { # check for duplicated names
    duplics <- unique(colnames(weights)[duplicated(colnames(weights))])
    stop(paste0("Names of weighting schemes are not unique. Following names occur at least twice: ",
                paste0(duplics, collapse = ", "), "."))
  }

  # check if any duplicate names across dimensions
  namesAll <- c(sento_measures$features, sento_measures$lexicons, colnames(weights))
  dup <- duplicated(namesAll)
  if (any(dup)) {
    stop(paste0("Following names appear at least twice as a component of a dimension: ",
                paste0(unique(namesAll[dup]), collapse = ', '), ". ",
                "Make sure names are unique within and across lexicons, features and time weighting schemes."))
  }

  # apply rolling time window, if not too large, for every weights column and combine all new measures column-wise
  if (!(fill %in% "none")) sento_measures <- measures_fill(sento_measures, fill = fill)
  measures <- data.table::as.data.table(sento_measures)
  toRoll <- measures[, -1]
  m <- nrow(measures)
  if (lag > m)
    stop("Rolling time aggregation window (= ", lag, ") is too large for number of observations per measure (= ", m, ")")
  sento_measures$attribWeights[["B"]] <- copy(weights)
  for (i in 1:ncol(weights)) {
    name <- colnames(weights)[i]
    add <- RcppRoll::roll_sum(as.matrix(toRoll), n = lag, weights = as.vector(weights[, i]),
                              normalize = FALSE, align = "right", na.rm = TRUE)
    colnames(add) <- paste0(colnames(toRoll), "--", name)
    if (i == 1) measuresAggTime <- add
    else measuresAggTime <- cbind(measuresAggTime, add)
  }
  measuresAggTime <- data.table::as.data.table(measuresAggTime)
  if (lag > 1) date <- measures$date[-1:-(lag-1)]
  else date <- measures$date
  measuresAggTime[, "date" := date]
  data.table::setcolorder(measuresAggTime, c("date", colnames(measuresAggTime)[-ncol(measuresAggTime)]))

  sento_measures$measures <- measuresAggTime
  sento_measures$time <- colnames(weights)
  sento_measures$stats <- compute_stats(sento_measures)

  return(sento_measures)
}

#' Extract dates related to sentiment time series peaks
#'
#' @author Samuel Borms
#'
#' @description This function extracts the dates for which aggregated time series sentiment is most
#' extreme (lowest, highest or both in absolute terms). The extracted dates are unique, even when,
#' for example, all most extreme sentiment values (for different sentiment measures) occur on only
#' one date.
#'
#' @param sento_measures a \code{sento_measures} object created using \code{\link{sento_measures}}.
#' @param n a positive \code{numeric} value to indicate the number of dates associated to sentiment peaks to extract.
#' If \code{n < 1}, it is interpreted as a quantile (for example, 0.07 would mean the 7\% most extreme dates).
#' @param type a \code{character} value, either \code{"pos"}, \code{"neg"} or \code{"both"}, respectively to look
#' for the \code{n} dates related to the most positive, most negative or most extreme (in absolute terms) sentiment
#' occurrences.
#' @param do.average a \code{logical} to indicate whether peaks should be selected based on the average sentiment
#' value per date.
#'
#' @return A vector of type \code{"Date"} corresponding to the \code{n} extracted sentiment peak dates.
#'
#' @examples
#' set.seed(505)
#'
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' # construct a sento_measures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 500)
#' l <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "month", lag = 3)
#' sento_measures <- sento_measures(corpusSample, l, ctr)
#'
#' # extract the peaks
#' peaksAbs <- peakdates(sento_measures, n = 5)
#' peaksAbsQuantile <- peakdates(sento_measures, n = 0.50)
#' peaksPos <- peakdates(sento_measures, n = 5, type = "pos")
#' peaksNeg <- peakdates(sento_measures, n = 5, type = "neg")
#'
#' @export
peakdates <- function(sento_measures, n = 10, type = "both", do.average = FALSE) {
  check_class(sento_measures, "sento_measures")
  stopifnot(n > 0)
  stopifnot(type %in% c("both", "neg", "pos"))

  nMax <- nobs(sento_measures)
  if (n < 1) n <- n * nMax
  n <- floor(n)
  if (n >= nMax) stop("The 'n' argument asks for too many dates.")

  measures <- data.table::as.data.table(sento_measures)[, -1] # drop dates
  m <- nmeasures(sento_measures)
  if (do.average == TRUE) {
    measures <- rowMeans(measures, na.rm = TRUE)
    dates <- get_dates(sento_measures)
  } else dates <- rep(get_dates(sento_measures), m)
  if (type == "both") measures <- abs(measures)
  indx <- order(unlist(measures), decreasing = ifelse(type == "neg", FALSE, TRUE))[1:(m * n)]
  peakDates <- unique(dates[indx])[1:n]
  peakDates
}

weights_across <- function(s, how = "proportional", do.ignoreZeros = TRUE, alpha = 0.1, by = "date") {

  if ("id" %in% colnames(s) && !("id" %in% by)) s <- s[, !"id"]
  if ("sentence_id" %in% colnames(s) && !("sentence_id" %in% by)) {
    if ("date" %in% colnames(s)) s <- s[, !"sentence_id"][, !"date"]
    else s <- s[, !"sentence_id"]
  }

  if (how == "equal_weight") {
    if (do.ignoreZeros == TRUE) {
      docsIn <- s[, lapply(.SD, function(x) (x * 1) / x), by = eval(by)] # 1 if document score not equal to NA
      weights <- docsIn[, lapply(.SD, function(x) x / sum(x, na.rm = TRUE)), by = eval(by)][, -c(1:2)]
    } else {
      weights <- s[, w := 1 / .N, by = eval(by)][, "w"]
      weights <- weights[, colnames(s)[-c(1:2)] := weights][, -1] # drop w column
      s[, w := NULL]
    }
  } else if (how == "proportional") {
    # proportional w.r.t. words in document vs. total words in all documents per date
    if (do.ignoreZeros == TRUE) {
      docsIn <- s[, lapply(.SD, function(x) (x * word_count) / x), by = eval(by)]
      weights <- docsIn[, lapply(.SD, function(x) x / sum(x, na.rm = TRUE)), by = eval(by)][, -c(1:2)]
    } else {
      weights <- s[, w := word_count / sum(word_count, na.rm = TRUE), by = eval(by)][, "w"]
      weights <- weights[, colnames(s)[-c(1:2)] := weights][, -1]
    }
  } else if (how == "inverseProportional") {
    # inverse proportional w.r.t. words in document vs. total words in all documents per date
    if (do.ignoreZeros == TRUE) {
      docsIn <- s[, lapply(.SD, function(x) (x * (1 / word_count)) / x), by = eval(by)]
      weights <- docsIn[, lapply(.SD, function(x) x / sum(x, na.rm = TRUE)), by = eval(by)][, -c(1:2)]
    } else {
      weights <- s[, w := (1 / word_count) / sum(1 / word_count, na.rm = TRUE), by = eval(by)][, "w"]
      weights <- weights[, colnames(s)[-c(1:2)] := weights][, -1]
    }
  } else if (how == "exponential") {
    # exponential w.r.t. words in document vs. total words in all documents per date
    if (do.ignoreZeros == TRUE) {
      docsIn <- s[, lapply(.SD, function(x)
        x * (10 * alpha * (word_count / sum(word_count, na.rm = TRUE) - 1)) / x), by = eval(by)]
      weights <- docsIn[, lapply(.SD, function(x) x / sum(x, na.rm = TRUE)), by = eval(by)][, -c(1:2)]
    } else {
      weights <- s[, w := 10 * alpha * (word_count / sum(word_count, na.rm = TRUE) - 1) /
                     sum(10 * alpha * (word_count / sum(word_count, na.rm = TRUE) - 1), na.rm = TRUE),
                   by = eval(by)][, "w"]
      weights <- weights[, colnames(s)[-c(1:2)] := weights][, -1]
    }
  } else if (how == "inverseExponential") {
    # inverse exponential w.r.t. words in document vs. total words in all documents per date
    if (do.ignoreZeros == TRUE) {
      docsIn <- s[, lapply(.SD, function(x)
        x * (10 * alpha * (1 - word_count / sum(word_count, na.rm = TRUE))) / x), by = eval(by)]
      weights <- docsIn[, lapply(.SD, function(x) x / sum(x, na.rm = TRUE)), by = eval(by)][, -c(1:2)]
    } else {
      weights <- s[, w := (10 * alpha * (1 - word_count / sum(word_count, na.rm = TRUE))) /
                     sum(10 * alpha * (1 - word_count / sum(word_count, na.rm = TRUE)), na.rm = TRUE),
                   by = eval(by)][, "w"]
      weights <- weights[, colnames(s)[-c(1:2)] := weights][, -1]
    }
  } else stop("Weighting scheme not recognized.")

  weights
}

