
attributions_docs <- function(sentomeasures, s, sentDates, seqDates, W, cols, refDates, coeffs, tNames) {
  B <- sentomeasures$attribWeights[["B"]]
  nLags <- nrow(B)
  attribsDocs <- lapply(refDates, function(t) {
    datesIn <- seqDates[(which(seqDates == t) - nLags + 1):which(seqDates == t)] # dates between t and lag number
    docWeights <- W[date %in% datesIn, -1:-2]
    if (nrow(docWeights) == 0) return(NULL) # no documents on this date
    B$date <- datesIn
    sents <- s[date %in% datesIn, ]
    sentWeighted <- sents[, -1:-3] * docWeights # drop id, date and word_count columns
    n <- sents[, list(count = .N), by = list(date)] # number of individual occurrences of the dates
    sentFull <- lapply(tNames, function(b) {
      coeffsIn <- coeffs[stringi::stri_detect(cols, regex = paste0("\\b", b, "\\b"))] # coefficients for weighting scheme
      sel <- sapply(stringi::stri_split(names(coeffsIn), regex = "--"), function(n) paste0(n[1:2], collapse = "--"))
      timeWeights <- rep(B[B$date %in% n$date, b], n$count) # respective lag weights per document
      colAttr <- matrix(rep(coeffsIn, sum(n$count)), nrow = sum(n$count), byrow = TRUE) *
        timeWeights * sentWeighted[, sel, with = FALSE]
      return(colAttr)
    })
    attribs <- Reduce(`+`, lapply(sentFull, rowSums, na.rm = TRUE)) # sum document values over time weighting schemes
    out <- data.table(sents[, c("id")], date = sents$date, attrib = attribs)
    return(out)
  })
  names(attribsDocs) <- refDates
  return(attribsDocs)
}

attributions_lags <- function(s, sentDates, seqDates, W, cols, sentomeasures, measures, coeffs,
                              attribsDocs, tNames, do.normalize) {
  B <- sentomeasures$attribWeights$B
  nLags <- nrow(B)
  namesLags <- get_names_lags(nLags)
  attribsLag <- lapply(names(attribsDocs), function(d) {
    if (is.null(attribsDocs[[d]])) {
      doc <- data.table(date = "xxxx-yy-zz", attrib = 0) # throw-away template
    } else {
      doc <- attribsDocs[[d]][, list(attrib = sum(attrib)), by = date]
    }
    datesLags <- seqDates[(which(seqDates == as.Date(d)) - nLags + 1):which(seqDates == as.Date(d))]
    lagsMissing <- which(!(datesLags %in% doc[["date"]]))
    setnames(doc, "date", "lag")
    if (length(lagsMissing) == 0) {
      doc[, "lag" := namesLags]
      return(doc)
    }
    if (length(lagsMissing) != nLags) doc[, "lag" := namesLags[-lagsMissing]]
    datesMissing <- datesLags[lagsMissing]
    if (sentomeasures$fill == "latest") {
      attribFills <- lapply(seq_along(datesMissing), function(j) {
        diffs <- s$date - datesMissing[j]
        diffsNeg <- diffs[diffs < 0]
        dateLatest <- s$date[1:length(diffsNeg)][which.max(diffsNeg)]
        docWeights <- W[date %in% dateLatest, -1:-2]
        sents <- s[date %in% dateLatest, ]
        sentWeighted <- colSums(sents[, -1:-3] * docWeights) # recreate sentiment measures as before time smoothing
        n <- sents[, list(count = .N), by = list(date)]
        sentFull <- lapply(tNames, function(b) {
          coeffsIn <- coeffs[stringi::stri_detect(cols, regex = paste0("\\b", b, "\\b"))]
          sel <- sapply(stringi::stri_split(names(coeffsIn), regex = "--"), function(n) paste0(n[1:2], collapse = "--"))
          attr <- coeffsIn * B[lagsMissing[j], b] * sentWeighted[sel]
          return(attr)
        })
        attribFill <- sum(unlist(sentFull), na.rm = TRUE)
        return(attribFill)
      })
      doc <- rbind(doc, data.table(lag = namesLags[lagsMissing], attrib = attribFills))
      doc <- doc[order(match(lag, namesLags))][lag %in% namesLags]
    } else {
      doc <- rbind(doc, data.table(lag = namesLags[lagsMissing], attrib = 0))
      doc <- doc[order(match(lag, namesLags))][lag %in% namesLags]
    }
    return(doc)
  })
  names(attribsLag) <- names(attribsDocs)
  attribsLag <- rbindlist(attribsLag, idcol = "date")
  attribsLag[, attrib := unlist(attrib)]
  attribsLag <- dcast(attribsLag, date ~ lag, value.var = "attrib")
  attribsLag[, date := as.Date(date)]
  if (do.normalize) {
    attribsLag[, colnames(attribsLag)[-1] := attribsLag[, -1] / sqrt(rowSums(attribsLag[, -1]^2, na.rm = TRUE))][]
  }
  for (i in seq_along(attribsLag)[-1])
    set(attribsLag, i = which(is.na(attribsLag[[i]])), j = i, value = 0)
  return(attribsLag)
}

attributions_dims <- function(sentomeasures, measures, cols, refDates, loc, coeffs,
                              do.normalize, dimNames, missingNames, type) {
  attribsDim <- lapply(dimNames, function(x) {
    sel <- cols[stringi::stri_detect(cols, regex = paste0("\\b", x, "\\b"))]
    coeffsIn <- data.table(matrix(coeffs[sel], nrow = length(loc), ncol = length(coeffs[sel]), byrow = TRUE))
    attribs <- rowSums(coeffsIn * measures[loc, sel, with = FALSE, drop = FALSE], na.rm = TRUE)
    attr <- data.table(date = refDates, attrib = attribs)
    return(attr)
  })
  names(attribsDim) <- dimNames
  attribsDim <- dcast(rbindlist(attribsDim, idcol = "name"), date ~ name, value.var = "attrib")
  if (length(missingNames) > 0) attribsDim[, (missingNames) := 0]
  attribsDim <- attribsDim[, c("date", sentomeasures[[type]]), with = FALSE]
  if (do.normalize) {
    attribsDim[, colnames(attribsDim)[-1] := attribsDim[, -1] / sqrt(rowSums(attribsDim[, -1]^2, na.rm = TRUE))][]
  }
  for (i in seq_along(attribsDim)[-1]) # set NaNs to zero (e.g. due to zero norm division)
    set(attribsDim, i = which(is.na(attribsDim[[i]])), j = i, value = 0)
  return(attribsDim)
}

.attributions.sentomodel <- function(model, sentomeasures, do.lags = TRUE, do.normalize = FALSE,
                                     refDates = NULL, factor = NULL) {
  check_class(sentomeasures, "sentomeasures")
  stopifnot(is.logical(do.normalize))

  sentomodel <- model

  # get appropriate sentiment measures from sentomeasures input object
  discarded <- sentomodel$discarded
  measures <- get_measures(sentomeasures)[, c(TRUE, !discarded), with = FALSE]

  # set dates at which to do attribution
  sampleDates <- sentomodel$dates
  if (is.null(refDates)) {
    refDates <- measures[date >= sampleDates[1] & date <= sampleDates[2], ][["date"]] # take in-sample dates
  } else {
    refDates <- sort(as.Date(refDates))
    if (min(refDates) < sampleDates[1])
      stop("The earliest date in 'refDates' is earlier than the first estimation sample date.")
    if (!all(refDates %in% measures[["date"]]))
      stop("Not all 'refDates' are available in the textual sentiment time series.")
  }

  # retrieve remaining required information from input objects
  s <- sentomeasures$sentiment
  W <- sentomeasures$attribWeights[["W"]]
  loc <- which(measures$date %in% refDates)
  measures[, "date" := NULL]
  cols <- colnames(measures)
  colsSplit <- stringi::stri_split(cols, regex = "--")
  lNames <- unique(sapply(colsSplit, "[", 1))
  lDel <- sentomeasures$lexicons[which(!(sentomeasures$lexicons %in% lNames))]
  fNames <- unique(sapply(colsSplit, "[", 2))
  fDel <- sentomeasures$features[which(!(sentomeasures$features %in% fNames))]
  tNames <- unique(sapply(colsSplit, "[", 3))
  tDel <- sentomeasures$time[which(!(sentomeasures$time %in% tNames))]

  # ignore fully dropped components in sentiment data.table (only composed of lexicon--feature combinations)
  if (length(fDel) > 0) {
    regex <- paste0(paste0("\\b", fDel, "\\b"), collapse = "|")
    s <- s[, !stringi::stri_detect(colnames(s), regex = regex), with = FALSE]
    W <- W[, !stringi::stri_detect(colnames(W), regex = regex), with = FALSE]
  }
  if (length(lDel) > 0) {
    regex <- paste0(paste0("\\b", lDel, "\\b"), collapse = "|")
    s <- s[, !stringi::stri_detect(colnames(s), regex = regex), with = FALSE]
    W <- W[, !stringi::stri_detect(colnames(W), regex = regex), with = FALSE]
  }

  # set a sequence of all possible dates
  sentDates <- unique(s$date)
  if (!(sentomeasures$fill == "none"))
    seqDates <- seq(sentDates[1], sentDates[length(sentDates)], by = sentomeasures$by)
  else seqDates <- sentDates

  # extract sentiment coefficients
  if (is.null(factor))
    coeffs <- stats::coef(sentomodel$reg)[cols, ]
  else
    coeffs <- stats::coef(sentomodel$reg)[[factor]][cols, ]

  # calculate and assemble attributions
  attribsAll <- list(documents = NULL, lags = NULL, lexicons = NULL, features = NULL, time = NULL)
  attribsDocs <- attributions_docs(sentomeasures, s, sentDates, seqDates, W, cols, refDates, coeffs, tNames)
  attribsAll[["documents"]] <- attribsDocs
  if (do.lags == TRUE) {
    attribsAll[["lags"]] <- attributions_lags(s, sentDates, seqDates, W, cols, sentomeasures, measures, coeffs,
                                              attribsDocs, tNames, do.normalize)
  }
  attribsAll[["lexicons"]] <- attributions_dims(sentomeasures, measures, cols, refDates, loc, coeffs, do.normalize,
                                                lNames, lDel, "lexicons")
  attribsAll[["features"]] <- attributions_dims(sentomeasures, measures, cols, refDates, loc, coeffs, do.normalize,
                                                fNames, fDel, "features")
  attribsAll[["time"]] <- attributions_dims(sentomeasures, measures, cols, refDates, loc, coeffs, do.normalize,
                                            tNames, tDel, "time")

  class(attribsAll) <- c("attributions", class(attribsAll))

  return(attribsAll)
}

.attributions.sentomodeliter <- function(model, sentomeasures, do.lags = TRUE, do.normalize = FALSE,
                                         refDates = NULL, factor = NULL) {
  stopifnot(is.logical(do.normalize))

  sentomodeliter <- model

  if (is.null(refDates)) refDates <- as.Date(names(sentomodeliter$models))
  attribsFull <- lapply(1:length(refDates), function(i) {
    date <- refDates[i]
    model <- sentomodeliter$models[[i]]
    attribs <- attributions(model, sentomeasures, do.lags = do.lags, do.normalize = do.normalize,
                            refDates = date, factor = factor)
    return(attribs)
  })

  attribsAll <- list(documents = NULL, lags = NULL, lexicons = NULL, features = NULL, time = NULL)
  attribsAll[["documents"]] <- unlist(lapply(attribsFull, function(x) return(x[["documents"]])), recursive = FALSE)
  if (do.lags == TRUE) attribsAll[["lags"]] <- rbindlist(lapply(attribsFull, function(x) return(x[["lags"]])))
  attribsAll[["lexicons"]] <- rbindlist(lapply(attribsFull, function(x) return(x[["lexicons"]])))
  attribsAll[["features"]] <- rbindlist(lapply(attribsFull, function(x) return(x[["features"]])))
  attribsAll[["time"]] <- rbindlist(lapply(attribsFull, function(x) return(x[["time"]])))

  class(attribsAll) <- c("attributions", class(attribsAll))

  return(attribsAll)
}

#' @importFrom compiler cmpfun
#' @export
attributions.sentomodel <- compiler::cmpfun(.attributions.sentomodel)

#' @importFrom compiler cmpfun
#' @export
attributions.sentomodeliter <- compiler::cmpfun(.attributions.sentomodeliter)

#' Retrieve top-down model sentiment attributions
#'
#' @author Samuel Borms, Keven Bluteau
#'
#' @description Computes the attributions to predictions for a (given) number of dates at all possible sentiment dimensions,
#' based on the coefficients associated to each sentiment measure, as estimated in the provided model object.
#'
#' @details See \code{\link{sento_model}} for an elaborate modelling example including the calculation and plotting of
#' attributions. The attribution for logistic models is represented in terms of log odds. For binomial models, it is
#' calculated with respect to the last factor level or factor column. A \code{NULL} value for document-level attribution
#' on a given date means no documents are directly implicated in the associated prediction.
#'
#' @param model a \code{sentomodel} or \code{sentomodeliter} object created with \code{\link{sento_model}}.
#' @param sentomeasures the \code{sentomeasures} object, as created with \code{\link{sento_measures}}, used to estimate
#' the model from the first argument (make sure this is the case!).
#' @param do.lags a \code{logical}, \code{TRUE} also computes the attribution to each time lag. For large time lags,
#' this is time-consuming.
#' @param do.normalize a \code{logical}, \code{TRUE} divides each element of every attribution vector at a given date by its
#' L2-norm at that date, normalizing the values between -1 and 1. The document attributions are not normalized.
#' @param refDates the dates (as \code{"yyyy-mm-dd"}) at which attribution is to be performed. These should be between the latest
#' date available in the input \code{sentomeasures} object and the first estimation sample date (that is, \code{model$dates[1]}
#' if \code{model} is a \code{sentomodel} object). All dates should also be in \code{get_dates(sentomeasures)}. If
#' \code{NULL} (default), attribution is calculated for all in-sample dates. Ignored if \code{model} is a \code{sentomodeliter}
#' object, for which attribution is calculated for all out-of-sample prediction dates.
#' @param factor the factor level as a single \code{character} vector for which attribution has to be calculated in
#' case of (a) multinomial model(s). Ignored for linear and binomial models.
#'
#' @return A \code{list} of class \code{attributions}, with \code{"documents"}, \code{"lags"}, \code{"lexicons"},
#' \code{"features"} and \code{"time"} as dimensions for which aggregation is computed. The last four dimensions are
#' \code{data.table}s having a \code{"date"} column and the other columns the different components of the dimension, with
#' the attributions as values. Document-level attribution is further decomposed into a \code{data.table} per date, with
#' \code{"id"}, \code{"date"} and \code{"attrib"} columns. If \code{do.lags = FALSE}, the \code{"lags"} element is set
#' to \code{NULL}.
#'
#' @seealso \code{\link{sento_model}}
#'
#' @export
attributions <- function(model, sentomeasures, do.lags = TRUE, do.normalize = FALSE, refDates = NULL, factor = NULL) {
  UseMethod("attributions", model)
}

#' Plot prediction attributions at specified level
#'
#' @author Samuel Borms, Keven Bluteau
#'
#' @description Shows a plot of the attributions along the dimension provided, stacked per date.
#'
#' @details See \code{\link{sento_model}} for an elaborate modelling example including the calculation and plotting of
#' attributions. This function does not handle the plotting of the attribution of individual documents, since there are
#' often a lot of documents involved and they appear only once at one date (even though a document may contribute to
#' predictions at several dates, depending on the number of lags in the time aggregation).
#'
#' @param x an \code{attributions} object created with \code{\link{attributions}}.
#' @param group a value from \code{c("lags", "lexicons", "features", "time")}.
#' @param ... not used.
#'
#' @return Returns a simple \code{\link{ggplot}} object, which can be added onto (or to alter its default elements) by using
#' the \code{+} operator. By default, a legend is positioned at the top if the number of components of the
#' dimension is at maximum twelve.
#'
#' @import ggplot2
#' @export
plot.attributions <- function(x, group = "features", ...) {
  if (!(group %in% c("lags", "lexicons", "features", "time")))
    stop("The 'group' argument should be either 'lags', 'lexicons', 'features' or 'time'.")
  attributions <- x
  attributions <- attributions[[group]]
  if (group == "lags" && is.null(attributions))
    stop("No 'lags' attribution is calculated. Set the 'do.lags' argument in the attributions() function to TRUE.")
  attributionsMelt <- melt(attributions, id.vars = "date", variable.factor = FALSE)
  attributionsMelt <- attributionsMelt[order(rank(as.character(variable)))]
  legendPos <- ifelse(length(unique(attributionsMelt[["variable"]])) <= 12, "top", "none")
  p <- ggplot(data = attributionsMelt, aes(x = date, y = value, group = variable, color = variable)) +
    geom_area(aes(fill = variable), alpha = 1) +
    geom_hline(yintercept = 0, size = 0.50, linetype = "dotted") +
    scale_fill_grey(start = 0, end = 1) +
    scale_x_date(name = "Date", date_labels = "%m-%Y") +
    scale_y_continuous(name = "Attribution") +
    theme_bw() +
    plot_theme(legendPos)
  p
}

