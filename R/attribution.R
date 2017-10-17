
.retrieve_attributions.sentomodel <- function(model, sentomeasures, ...) {

  ### TODO: application to logistic models (exponentiation? + selection of factor level coefficients if multinomial)
  ### TODO: scaling
  ### TODO: add attribution_lags() function (handle when fill is "latest" + aggregation across documents per lag needs to be known)
  ### TODO: make cleaner (helper functions for repeting parts)
  ### TODO: document usage of ... in retrieve_attributions() [date(s)] + add example

  ### CHECK: identify sentomeasures with sentomodel object(s) + first date >= first model estimation date & refDates %in% measures$date

  sentomodel <- model

  if (FALSE)
    stop("The provided sentomeasures object does not coincide with the sentiment measures used for the sentomodel estimation.")

  dots <- list(...)
  refDates <- as.Date(dots[[1]])
  loc <- which(sentomeasures$measures$date %in% refDates)
  sent <- sentomeasures$sentiment
  sentDates <- unique(sent$date)
  W <- sentomeasures$attribWeights[["W"]]
  B <- sentomeasures$attribWeights[["B"]]

  measures <- sentomeasures$measures[, c(FALSE, !sentomodel$discarded), with = FALSE]
  cols <- colnames(measures)
  colsSplit <- stringi::stri_split(cols, regex = "--")
  lexNames <- unique(sapply(colsSplit, "[", 1))
  featNames <- unique(sapply(colsSplit, "[", 2))
  timeNames <- unique(sapply(colsSplit, "[", 3))

  featuresDropped <- sentomeasures$features[which(!(sentomeasures$features %in% featNames))]
  lexiconsDropped <- sentomeasures$lexicons[which(!(sentomeasures$lexicons %in% lexNames))]
  if (length(featuresDropped) > 0)
    sent <- sent[, !stringi::stri_detect(colnames(sent),
                                         regex = paste0(paste0("\\b", featuresDropped, "\\b"), collapse = "|")), with = FALSE]
  if (length(lexiconsDropped) > 0)
    sent <- sent[, !stringi::stri_detect(colnames(sent),
                                         regex = paste0(paste0("\\b", lexiconsDropped, "\\b"), collapse = "|")), with = FALSE]

  coeffs <- stats::coef(sentomodel$reg)[cols, ] # only keep sentiment coefficients

  attribution_docs <- function() {
    if (!(sentomeasures$fill == "none"))
      dates <- seq(sentDates[1], sentDates[length(sentDates)], by = sentomeasures$by)
    else dates <- sentDates
    attribsDocs <- lapply(refDates, function(t) {
      datesIn <- dates[(which(dates %in% t) - nrow(B) + 1):which(dates == t)] # dates between t and lag number
      docWeights <- W[date %in% datesIn, "w"]
      B$date <- datesIn
      if (dim(docWeights)[1] == 0) return("No documents on this date.")
      sents <- sent[date %in% datesIn, ]
      sentWeighted <- docWeights$w * sents[, -1:-3]
      n <- sents[, list(count = .N), by = list(date)] # number of individual occurrences of the dates
      sentFull <- lapply(timeNames, function(b) {
        c <- coeffs[stringi::stri_detect(cols, regex = paste0("\\b", b))] # get coefficients for weighting scheme
        timeWeights <- rep(B[B$date %in% n$date, b], n$count) # respective lag weights per document
        colAttr <- matrix(rep(c, sum(n$count)), nrow = sum(n$count), byrow = TRUE) * timeWeights * sentWeighted
        return(colAttr)
      })
      attribs <- Reduce(`+`, lapply(sentFull, rowSums)) # sum document values over time weighting schemes)
      out <- data.table(sents[, c("id")], date = sents$date, attrib = attribs)
      return(out)
    })
    names(attribsDocs) <- refDates
    return(attribsDocs)
  }

  attribution_lexicons <- function() {
    attribsLexicons <- lapply(lexNames, function(x) {
      sel <- cols[stringi::stri_detect(cols, regex = paste0("\\b", x, "\\b"))]
      coeffsIn <- data.table(matrix(coeffs[sel], nrow = length(loc), ncol = length(coeffs[sel]), byrow = TRUE))
      attribs <- rowSums(coeffsIn * measures[loc, sel, with = FALSE, drop = FALSE])
      attr <- data.table(date = refDates, attrib = attribs)
      return(attr)
    })
    names(attribsLexicons) <- lexNames
    attribsLexicons <- dcast(rbindlist(attribsLexicons, idcol = "name"), date ~ name, value.var = "attrib")
    missing <- sentomeasures$lexicons[!(sentomeasures$lexicons %in% colnames(attribsLexicons))]
    if (length(missing) > 0) {
      attribsLexicons[, (missing) := 0]
    }
    attribsLexicons <- attribsLexicons[, c("date", sentomeasures$lexicons), with = FALSE]
    return(attribsLexicons)
  }

  attribution_features <- function() {
    attribsFeatures <- lapply(featNames, function(x) {
      sel <- cols[stringi::stri_detect(cols, regex = paste0("\\b", x, "\\b"))]
      coeffsIn <- data.table(matrix(coeffs[sel], nrow = length(loc), ncol = length(coeffs[sel]), byrow = TRUE))
      attribs <- rowSums(coeffsIn * measures[loc, sel, with = FALSE, drop = FALSE])
      attr <- data.table(date = refDates, attrib = attribs)
      return(attr)
    })
    names(attribsFeatures) <- featNames
    attribsFeatures <- dcast(rbindlist(attribsFeatures, idcol = "name"), date ~ name, value.var = "attrib")
    missing <- sentomeasures$features[!(sentomeasures$features %in% colnames(attribsFeatures))]
    if (length(missing) > 0) {
      attribsFeatures[, (missing) := 0]
    }
    attribsFeatures <- attribsFeatures[, c("date", sentomeasures$features), with = FALSE]
    return(attribsFeatures)
  }

  attribution_time <- function() {
    attribsTime <- lapply(timeNames, function(x) {
      sel <- cols[stringi::stri_detect(cols, regex = paste0("\\b", x, "\\b"))]
      coeffsIn <- data.table(matrix(coeffs[sel], nrow = length(loc), ncol = length(coeffs[sel]), byrow = TRUE))
      attribs <- rowSums(coeffsIn * measures[loc, sel, with = FALSE, drop = FALSE])
      attr <- data.table(date = refDates, attrib = attribs)
      return(attr)
    })
    names(attribsTime) <- timeNames
    attribsTime <- dcast(rbindlist(attribsTime, idcol = "name"), date ~ name, value.var = "attrib")
    missing <- sentomeasures$time[!(sentomeasures$time %in% colnames(attribsTime))]
    if (length(missing) > 0) {
      attribsTime[, (missing) := 0]
    }
    attribsTime <- attribsTime[, c("date", sentomeasures$time), with = FALSE]
    return(attribsTime)
  }

  attribsAll <- vector(mode = "list", length = 4)
  names(attribsAll) <- c("documents", "lexicons", "features", "time")
  attribsAll[["documents"]] <- attribution_docs()
  attribsAll[["lexicons"]] <- attribution_lexicons()
  attribsAll[["features"]] <- attribution_features()
  attribsAll[["time"]] <- attribution_time()

  ### unit test material

  l <- rowSums(attribsAll$lexicons[, -1])
  f <- rowSums(attribsAll$features[, -1])
  t <- rowSums(attribsAll$time[, -1])
  d <- as.vector(sapply(attribsAll$documents, function(x) return(sum(x$attrib))))
  print(all.equal(t, l, f, d)) # compares only first elements, beware!

  return(attribsAll)
}

.retrieve_attributions.sentomodeliter <- function(model, sentomeasures, ...) {

  sentomodeliter <- model

  if (FALSE)
    stop("The provided sentomeasures object does not coincide with the sentiment measures used for the sentomodel estimation.")

  refDates <- as.Date(names(sentomodeliter$models))
  # refDates <- modelDates %m+% months(1)

  attribsFull <- lapply(1:length(refDates), function(d) {
    date <- refDates[d]
    model <- sentomodeliter$models[[d]]
    attribs <- retrieve_attributions(model, sentomeasures, refDates = date)
    return(attribs)
  })

  attribsAll <- vector(mode = "list", length = 4)
  names(attribsAll) <- c("documents", "lexicons", "features", "time")
  attribsAll[["documents"]] <- unlist(lapply(attribsFull, function(x) return(x[["documents"]])), recursive = FALSE)
  attribsAll[["lexicons"]] <- rbindlist(lapply(attribsFull, function(x) return(x[["lexicons"]])))
  attribsAll[["features"]] <- rbindlist(lapply(attribsFull, function(x) return(x[["features"]])))
  attribsAll[["time"]] <- rbindlist(lapply(attribsFull, function(x) return(x[["time"]])))

  return(attribsAll)
}

#' @export
retrieve_attributions.sentomodel <- compiler::cmpfun(.retrieve_attributions.sentomodel)

#' @export
retrieve_attributions.sentomodeliter <- compiler::cmpfun(.retrieve_attributions.sentomodeliter)

#' Retrieve top-down sentiment attributions given forecasting model object
#'
#' @author Samuel Borms, Keven Bluteau
#'
#' @description Computes the attributions to forecasts for a (given) number of dates at all possible sentiment dimensions, based on
#' the coefficients associated to each sentiment measure, as estimated in the provided model object.
#'
#' @details See \code{\link{sento_model}} for an elaborate modelling example including the calculation and plotting of attributions.
#'
#' @param model a \code{sentomodel} or \code{sentomodeliter} object.
#' @param sentomeasures the \code{sentomeasures} object used to estimate the \code{sentomodel} object argument.
#' @param ... the dates at which attribution is to be performed. The dates should be between the latest date avaiable in the
#' input \code{sentomeasures} object and the first date of the sample used to estimate the model in the \code{model} argument.
#' If not provided, attribution will be calculated for all in-sample dates. Not used if \code{model} is a \code{sentomodeliter}
#' object, for which attribution for all out-of-sample forecasting dates is calculated.
#'
#' @return A list with all possible dimensions for which aggregation is computed, being \code{"documents"}, \code{"lexicons"},
#' \code{"features"}, \code{"time"} and \code{"lags"}. The last four dimensions are \code{data.table}s having a \code{"date"}
#' column and the other columns the different names of the dimension, with the attributions as values. For document-level attribution,
#' the list is further decomposed into a \code{data.table} per date, with \code{"id"}, \code{date} and \code{attrib} columns.
#'
#' @seealso \code{\link{sento_model}}
#'
#' @export
retrieve_attributions <- function(model, sentomeasures, ...) {
  UseMethod("retrieve_attributions", model)
}

#' Plot forecasting attribution at specified level
#'
#' @author Samuel Borms
#'
#' @description Shows a plot of the attributions along the dimension provided.
#'
#' @details See \code{\link{sento_model}} for an elaborate modelling example including the calculation and plotting of attributions.
#' This function does not handle the plotting of the attribution of individual documents, since there are often a lot of documents
#' involved and de facto they appear only once at one date (even though a document may contribute to forecasts at several dates,
#' depending on the number of lags in the time aggregation.)
#'
#' @param attributions an output from a \code{retrieve_attributions()} call.
#' @param group a value from \code{c("lexicons", "features", "time", "lags")}.
#'
#' @return Returns a simple \pkg{ggplot2} plot, which can be added onto (or to alter its default elements) by using the
#' \code{+} operator (see examples). By default, a legend is positioned at the top if the number of dimensions (thus, individual
#' plots) is at maximum twelve.
#'
#' @import ggplot2
#' @export
plot_attributions <- function(attributions, group = "features") {

  if (!(group %in% c("lexicons", "features", "time", "lags")))
    stop("The 'group' argument should be either 'lexicons', 'features', 'time' or 'lags'.")
  # melt attributions for plotting
  attributions <- attributions[[group]]
  attributionsMelt <- melt(attributions, id.vars = "date")
  legendPos <- ifelse(length(unique(attributionsMelt[["variable"]])) <= 12, "top", "none")
  p <- ggplot(data = attributionsMelt, aes(x = date, y = value, color = variable)) +
    geom_line() +
    geom_hline(yintercept = 0, size = 0.50, linetype = "dotted") +
    scale_x_date(name = "Date", date_labels = "%m-%Y") +
    scale_y_continuous(name = "Attribution") +
    ggthemes::theme_tufte() +
    theme(legend.title = element_blank(), legend.position = legendPos, text = element_text(size = 11))

  return(p)
}

