
.retrieve_attributions.sentomodel <- function(model, sentomeasures, do.normalize = FALSE, refDates = NULL) {

  ### TODO: application to logistic models (exponentiation? + selection of factor level coefficients if multinomial)
  ### TODO: add attribution_lags() function (handle fill is "latest" + aggregation across documents per lag needs to be known)

  sentomodel <- model

  # check if sentiment measures sample used for sentomodel estimation coincides with measures in input sentomeasures
  xSent <- sentomodel$x[, 1:sum(!sentomodel$discarded)] # get sentiment variables
  measures <- sentomeasures$measures[, c(TRUE, !sentomodel$discarded), with = FALSE]
  sampleDates <- sentomodel$dates
  measuresSample <- measures[date >= sampleDates[1] & date <= sampleDates[2], ]
  if (!all(xSent == measuresSample[, -1]))
    stop("The input sentomeasures object does not coincide with the sentiment measures used in the sentomodel estimation.")
  if (is.null(refDates)) {
    refDates <- measuresSample$date # take in-sample dates
  } else {
    refDates <- sort(as.Date(refDates))
    if (min(refDates) < sampleDates[1])
      stop("The earliest date for which attribution is to be calculated is earlier than the first estimation sample date.")
    if (!all(refDates %in% measures$date))
      stop("Not all dates for which attribution is to be calculated are available in the textual sentiment time series.")
  }

  # retrieve remaining necessary information from input objects
  sent <- sentomeasures$sentiment
  sentDates <- unique(sent$date)
  W <- sentomeasures$attribWeights[["W"]]
  B <- sentomeasures$attribWeights[["B"]]
  loc <- which(measures$date %in% refDates)
  measures[, "date" := NULL]
  cols <- colnames(measures)
  colsSplit <- stringi::stri_split(cols, regex = "--")
  lexNames <- unique(sapply(colsSplit, "[", 1))
  featNames <- unique(sapply(colsSplit, "[", 2))
  timeNames <- unique(sapply(colsSplit, "[", 3))
  featDropped <- sentomeasures$features[which(!(sentomeasures$features %in% featNames))]
  lexDropped <- sentomeasures$lexicons[which(!(sentomeasures$lexicons %in% lexNames))]
  timeDropped <- sentomeasures$time[which(!(sentomeasures$time %in% timeNames))]

  # ignore dropped components in sentiment data.table (only composed of lexicon--feature combinations)
  if (length(featDropped) > 0)
    sent <- sent[, !stringi::stri_detect(colnames(sent),
                                         regex = paste0(paste0("\\b", featDropped, "\\b"), collapse = "|")), with = FALSE]
  if (length(lexDropped) > 0)
    sent <- sent[, !stringi::stri_detect(colnames(sent),
                                         regex = paste0(paste0("\\b", lexDropped, "\\b"), collapse = "|")), with = FALSE]
  coeffs <- stats::coef(sentomodel$reg)[cols, ] # only keep sentiment coefficients

  # create attribution calculation functions (makes use of variables defined in this environment)
  attribution_docs <- function() {
    if (!(sentomeasures$fill == "none"))
      dates <- seq(sentDates[1], sentDates[length(sentDates)], by = sentomeasures$by)
    else dates <- sentDates
    attribsDocs <- lapply(refDates, function(t) {
      datesIn <- dates[(which(dates %in% t) - nrow(B) + 1):which(dates == t)] # dates between t and lag number
      docWeights <- W[date %in% datesIn, "w"]
      if (dim(docWeights)[1] == 0) return("No documents on this date.")
      B$date <- datesIn
      sents <- sent[date %in% datesIn, ]
      sentWeighted <- docWeights$w * sents[, -1:-3] # drop id, date and word_count columns
      n <- sents[, list(count = .N), by = list(date)] # number of individual occurrences of the dates
      sentFull <- lapply(timeNames, function(b) {
        coeffsIn <- coeffs[stringi::stri_detect(cols, regex = paste0("\\b", b))] # get coefficients for weighting scheme
        timeWeights <- rep(B[B$date %in% n$date, b], n$count) # respective lag weights per document
        colAttr <- matrix(rep(coeffsIn, sum(n$count)), nrow = sum(n$count), byrow = TRUE) * timeWeights * sentWeighted
        return(colAttr)
      })
      attribs <- Reduce(`+`, lapply(sentFull, rowSums)) # sum document values over time weighting schemes
      out <- data.table(sents[, c("id")], date = sents$date, attrib = attribs)
      return(out)
    })
    names(attribsDocs) <- refDates
    return(attribsDocs)
  }
  attribution_dim <- function(dimNames, missingNames, type) {
    attribsDim <- lapply(dimNames, function(x) {
      sel <- cols[stringi::stri_detect(cols, regex = paste0("\\b", x, "\\b"))]
      coeffsIn <- data.table(matrix(coeffs[sel], nrow = length(loc), ncol = length(coeffs[sel]), byrow = TRUE))
      attribs <- rowSums(coeffsIn * measures[loc, sel, with = FALSE, drop = FALSE])
      attr <- data.table(date = refDates, attrib = attribs)
      return(attr)
    })
    names(attribsDim) <- dimNames
    attribsDim <- dcast(rbindlist(attribsDim, idcol = "name"), date ~ name, value.var = "attrib")
    if (length(missingNames) > 0) attribsDim[, (missingNames) := 0]
    attribsDim <- attribsDim[, c("date", sentomeasures[[type]]), with = FALSE]
    if (do.normalize) {
      scaled <- attribsDim[, -1] / sqrt(rowSums(attribsDim[, -1]^2))
      attribsDim[, colnames(attribsDim)[-1] := ifelse(is.na(scaled), 0, scaled)][]
    }
    return(attribsDim)
  }

  # calculate and assemble attributions
  attribsAll <- vector(mode = "list", length = 4)
  names(attribsAll) <- c("documents", "lexicons", "features", "time")
  attribsAll[["documents"]] <- attribution_docs()
  attribsAll[["lexicons"]] <- attribution_dim(lexNames, lexDropped, "lexicons")
  attribsAll[["features"]] <- attribution_dim(featNames, featDropped, "features")
  attribsAll[["time"]] <- attribution_dim(timeNames, timeDropped, "time")

  return(attribsAll)
}

.retrieve_attributions.sentomodeliter <- function(model, sentomeasures, do.normalize = FALSE, refDates = NULL) {

  sentomodeliter <- model

  if (FALSE)
    stop("The provided sentomeasures object does not coincide with the sentiment measures used for the sentomodel estimation.")

  refDates <- as.Date(names(sentomodeliter$models))
  attribsFull <- lapply(1:length(refDates), function(d) {
    date <- refDates[d]
    model <- sentomodeliter$models[[d]]
    attribs <- retrieve_attributions(model, sentomeasures, do.normalize = do.normalize, refDates = date)
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

#' Retrieve top-down sentiment attributions given model object
#'
#' @author Samuel Borms, Keven Bluteau
#'
#' @description Computes the attributions to predictions for a (given) number of dates at all possible sentiment dimensions,
#' based on the coefficients associated to each sentiment measure, as estimated in the provided model object.
#'
#' @details See \code{\link{sento_model}} for an elaborate modelling example including the calculation and plotting of
#' attributions.
#'
#' @param model a \code{sentomodel} or \code{sentomodeliter} object.
#' @param sentomeasures the \code{sentomeasures} object used to estimate the \code{sentomodel} object argument.
#' @param do.normalize a \code{logical}, \code{TRUE} divides each element of every attribution vector at a given date by its
#' L2-norm at that date. The document attributions are not normalized.
#' @param refDates the dates at which attribution is to be performed. These should be between the latest date available in the
#' input \code{sentomeasures} object and the first date of the sample used to estimate the model, i.e. \code{model$dates[1]}
#' if \code{model} is a \code{sentomodel} object. All dates should also be present in \code{sentomeasures$measures$date}.
#' If \code{NULL} (default), attribution is calculated for all in-sample dates. Ignored if \code{model} is a
#' \code{sentomodeliter} object, for which attribution is calculated for all out-of-sample prediction dates.
#'
#' @return A list with all possible dimensions for which aggregation is computed, being \code{"documents"}, \code{"lexicons"},
#' \code{"features"}, \code{"time"} and \code{"lags"}. The last four dimensions are \code{data.table}s having a \code{"date"}
#' column and the other columns the different names of the dimension, with the attributions as values. For document-level
#' attribution, the list is further decomposed into a \code{data.table} per date, with \code{"id"}, \code{date} and
#' \code{attrib} columns.
#'
#' @seealso \code{\link{sento_model}}
#'
#' @export
retrieve_attributions <- function(model, sentomeasures, do.normalize, refDates) {
  UseMethod("retrieve_attributions", model)
}

#' Plot prediction attribution at specified level
#'
#' @author Samuel Borms
#'
#' @description Shows a plot of the attributions along the dimension provided.
#'
#' @details See \code{\link{sento_model}} for an elaborate modelling example including the calculation and plotting of
#' attributions. This function does not handle the plotting of the attribution of individual documents, since there are often a
#' lot of documents involved and de facto they appear only once at one date (even though a document may contribute to
#' predictions at several dates, depending on the number of lags in the time aggregation).
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

