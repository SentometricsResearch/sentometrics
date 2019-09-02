
#' Plot sentiment measures
#'
#' @author Samuel Borms
#'
#' @method plot sento_measures
#'
#' @description Plotting method that shows all sentiment measures from the provided \code{sento_measures}
#' object in one plot, or the average along one of the lexicons, features and time weighting dimensions. We suggest to
#' make use of a \code{measures_xyz} function when you want to plot only a subset of the sentiment measures.
#'
#' @param x a \code{sento_measures} object created using \code{\link{sento_measures}}.
#' @param group a value from \code{c("lexicons", "features", "time", "all")}. The first three choices display the average of
#' all measures from the same group, in a different color. The choice \code{"all"} displays every single sentiment measure
#' in a separate color, but this may look visually overwhelming very fast, and can be quite slow.
#' @param ... not used.
#'
#' @return Returns a simple \code{\link{ggplot}} object, which can be added onto (or to alter its default elements) by using
#' the \code{+} operator (see example). By default, a legend is positioned at the top if there are at maximum twelve line
#' graphs plotted and \code{group} is different from \code{"all"}.
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' # construct a sento_measures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 500)
#' l <- sento_lexicons(list_lexicons[c("LM_en")], list_valence_shifters[["en"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sento_measures <- sento_measures(corpusSample, l, ctr)
#'
#' # plot sentiment measures
#' plot(sento_measures, group = "features")
#'
#' \dontrun{
#' # adjust appearance of plot
#' library("ggplot2")
#' p <- plot(sento_measures)
#' p <- p +
#'   scale_x_date(name = "month-year") +
#'   scale_y_continuous(name = "newName")
#' p}
#'
#' @import ggplot2
#' @export
plot.sento_measures <- function(x, group = "all", ...) {
  if (!(group %in% c("lexicons", "features", "time", "all")))
    stop("The 'group' argument should be either 'lexicons', 'features', 'time' or 'all'.")
  sento_measures <- x
  measures <- as.data.table(sento_measures)
  if (group == "all") {
    measuresMelt <- melt(measures, id.vars = "date", variable.factor = FALSE)
    legendPos <- "none"
  } else {
    measuresMelt <- measures_to_long(measures)[, c("date", group, "value"), with = FALSE]
    measuresMelt <- measuresMelt[, list(value = mean(value)), by = list(date, variable = eval(parse(text = group)))]
    legendPos <- ifelse(length(unique(measuresMelt[["variable"]])) <= 12, "top", "none")
  }
  measuresMelt <- measuresMelt[order(rank(as.character(variable)))]
  p <- ggplot(data = measuresMelt, aes(x = date, y = value, color = variable)) +
    geom_line() +
    geom_hline(yintercept = 0, size = 0.50, linetype = "dotted") +
    scale_x_date(name = "Date", date_labels = "%m-%Y") +
    scale_y_continuous(name = "Sentiment") +
    theme_bw() +
    plot_theme(legendPos)
  p
}

#' Differencing of sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Differences the sentiment measures from a \code{sento_measures} object.
#'
#' @param x a \code{sento_measures} object created using \code{\link{sento_measures}}.
#' @param lag a \code{numeric}, see documentation for the generic \code{\link{diff}}.
#' @param differences a \code{numeric}, see documentation for the generic \code{\link{diff}}.
#' @param ... not used.
#'
#' @return A modified \code{sento_measures} object, with the measures replaced by the differenced measures as well as updated
#' statistics.
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
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sento_measures <- sento_measures(corpusSample, l, ctr)
#'
#' # first-order difference sentiment measures with a lag of two
#' diffed <- diff(sento_measures, lag = 2, differences = 1)
#'
#' @export
diff.sento_measures <- function(x, lag = 1, differences = 1, ...) {
  sento_measures <- x
  dates <- get_dates(sento_measures)[-1:-(lag * differences)]
  measures <- as.data.table(sento_measures)[, -1] # drop dates
  measuresDiff <- diff(as.matrix(measures), lag = lag, differences = differences)
  sento_measures$measures <- data.table(date = dates, measuresDiff)
  sento_measures$stats <- compute_stats(sento_measures)
  return(sento_measures)
}

#' @export
nmeasures.sento_measures <- function(sento_measures) {
  NCOL(sento_measures[["measures"]]) - 1 # omit date column
}

#' Get number of sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Returns the number of sentiment measures.
#'
#' @param sento_measures a \code{sento_measures} object created using \code{\link{sento_measures}}.
#'
#' @return The number of sentiment measures in the input \code{sento_measures} object.
#'
#' @export
nmeasures <- function(sento_measures) {
  UseMethod("nmeasures", sento_measures)
}

#' Get number of observations in the sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Returns the number of data points available in the sentiment measures.
#'
#' @param object a \code{sento_measures} object created using \code{\link{sento_measures}}.
#' @param ... not used.
#'
#' @return The number of rows (observations/data points) in \code{sento_measures[["measures"]]}.
#'
#' @keywords internal
#'
#' @importFrom stats nobs
#' @export
nobs.sento_measures <- function(object, ...) {
  NROW(object[["measures"]])
}

#' Scaling and centering of sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Scales and centers the sentiment measures from a \code{sento_measures} object, column-per-column. By default,
#' the measures are normalized. \code{NA}s are removed first.
#'
#' @details If one of the arguments \code{center} or \code{scale} is a \code{matrix}, this operation will be applied first,
#' and eventual other centering or scaling is computed on that data.
#'
#' @param x a \code{sento_measures} object created using \code{\link{sento_measures}}.
#' @param center a \code{logical} or a \code{numeric} vector, see documentation for the generic \code{\link{scale}}.
#' Alternatively, one can provide a \code{matrix} of dimensions \code{nobs(sento_measures)} times \code{1} or
#' \code{nmeasures(sento_measures)} with values to add to each individual observation.
#' @param scale a \code{logical} or a \code{numeric} vector, see documentation for the generic \code{\link{scale}}.
#' Alternatively, one can provide a \code{matrix} of dimensions \code{nobs(sento_measures)} times \code{1} or
#' \code{nmeasures(sento_measures)} with values to divide each individual observation by.
#'
#' @return A modified \code{sento_measures} object, with the measures replaced by the scaled measures as well as updated
#' statistics.
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' set.seed(505)
#'
#' # construct a sento_measures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 500)
#' l <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sento_measures <- sento_measures(corpusSample, l, ctr)
#'
#' # scale sentiment measures to zero mean and unit standard deviation
#' sc1 <- scale(sento_measures)
#'
#' n <- nobs(sento_measures)
#' m <- nmeasures(sento_measures)
#'
#' # add a matrix
#' sc2 <- scale(sento_measures, center = matrix(runif(n * m), n, m), scale = FALSE)
#'
#' # divide every row observation based on a one-column matrix, then center
#' sc3 <- scale(sento_measures, center = TRUE, scale = matrix(runif(n)))
#'
#' @export
scale.sento_measures <- function(x, center = TRUE, scale = TRUE) {
  sento_measures <- x
  dates <- get_dates(sento_measures)
  measures <- as.data.table(sento_measures)[, -1] # drop dates
  if (is.matrix(center)) {
    if (nrow(center) != nobs(sento_measures) || !(ncol(center) %in% c(1, nmeasures(sento_measures))))
      stop("The matrix dimensions of the 'center' argument are not correct.")
    measures <- measures + center
    center <- FALSE
  }
  if (is.matrix(scale)) {
    if (nrow(scale) != nobs(sento_measures) || !(ncol(scale) %in% c(1, nmeasures(sento_measures))))
      stop("The matrix dimensions of the 'scale' argument are not correct.")
    measures <- measures / scale
    scale <- FALSE
  }

  measuresNorm <- scale(measures, center = center, scale = scale)
  sento_measures$measures <- data.table(date = dates, measuresNorm)
  sento_measures$stats <- compute_stats(sento_measures)
  return(sento_measures)
}

#' @export
summary.sento_measures <- function(object, ...) {
  sento_measures <- object
  freq <- c("daily", "weekly", "monthly", "yearly")[c("day", "week", "month", "year") %in% sento_measures$ctr$time$weightingParam$by]
  cat("This sento_measures object contains ", nmeasures(sento_measures), " textual sentiment time series with ",
      nobs(sento_measures), " observations each ", "(", freq, ").", "\n", sep = "")
  cat("\n")
  cat("Following features are present:", sento_measures$features, "\n")
  cat("Following lexicons are used to calculate sentiment:", sento_measures$lexicons, "\n")
  cat("Following scheme is applied for aggregation within documents:", sento_measures$within$howWithin, "\n")
  cat("Following scheme is applied for aggregation across documents:", sento_measures$docs$howDocs, "\n")
  cat("Following schemes are applied for aggregation across time:", sento_measures$time, "\n")
  cat("\n")
  cat("Aggregate average statistics:", "\n")
  print(round(rowMeans(sento_measures$stats), 5))
  cat()
}

#' @export
print.sento_measures <- function(x, ...) {
  sento_measures <- x
  cat("A sento_measures object (", nmeasures(sento_measures),
      " textual sentiment time series, ", nobs(sento_measures),
      " observations).", "\n", sep = "")
}

#' Get the dates of the sentiment measures/time series
#'
#' @author Samuel Borms
#'
#' @description Returns the dates of the sentiment time series.
#'
#' @param sento_measures a \code{sento_measures} object created using \code{\link{sento_measures}}.
#'
#' @return The \code{"date"} column in \code{sento_measures[["measures"]]} as a \code{character} vector.
#'
#' @export
get_dates <- function(sento_measures) {
  check_class(sento_measures, "sento_measures")
  sento_measures$measures[, date]
}

#' Get the dimensions of the sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Returns the components across all three dimensions of the sentiment measures.
#'
#' @param sento_measures a \code{sento_measures} object created using \code{\link{sento_measures}}.
#'
#' @return The \code{"features"}, \code{"lexicons"} and \code{"time"} elements in \code{sento_measures}.
#'
#' @export
get_dimensions <- function(sento_measures) {
  check_class(sento_measures, "sento_measures")
  sento_measures[c("features", "lexicons", "time")]
}

#' Get the sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Extracts the sentiment measures \code{data.table} in either wide (by default)
#' or long format.
#'
#' @param sento_measures a \code{sento_measures} object created using \code{\link{sento_measures}}.
#' @param keep.rownames see \code{\link{as.data.table}}.
#' @param format a single \code{character} vector, one of \code{c("wide", "long")}.
#' @param ... not used.
#'
#' @return The panel of sentiment measures under \code{sento_measures[["measures"]]},
#' in wide or long format.
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' sm <- sento_measures(sento_corpus(corpusdf = usnews[1:200, ]),
#'                      sento_lexicons(list_lexicons["LM_en"]),
#'                      ctr_agg(lag = 3))
#'
#' as.data.table(sm)
#' as.data.table(sm, "long")
#'
#' @export
as.data.table.sento_measures <- function(sento_measures, keep.rownames = FALSE, format = "wide", ...) {
  check_class(sento_measures, "sento_measures")
  if (format == "wide")
    sento_measures[["measures"]]
  else if (format == "long")
    measures_to_long(sento_measures[["measures"]])
  else
    stop("The 'format' argument should be 'wide' or 'long'.")
}

#' Subset sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Subsets rows of the sentiment measures based on its columns.
#'
#' @param x a \code{sento_measures} object created using \code{\link{sento_measures}}.
#' @param subset a logical (non-\code{character}) expression indicating the rows to keep. If a
#' \code{numeric} input is given, it is used for row index subsetting.
#' @param select a \code{character} vector of the lexicon, feature and time weighting scheme names, to indicate which
#' measures need to be selected, or as a \code{list} of \code{character} vectors, possibly with separately specified
#' combinations (consisting of one unique lexicon, one unique feature, and one unique time weighting scheme at maximum).
#' @param delete see the \code{select} argument.
#' @param ... not used.
#'
#' @return A modified \code{sento_measures} object, with only the remaining rows and sentiment measures,
#' including updated information and statistics, but the original sentiment scores \code{data.table} untouched.
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' # construct a sento_measures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 500)
#' l <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sm <- sento_measures(corpusSample, l, ctr)
#'
#' # different subsets
#' sub1 <- subset(sm, HENRY_en--economy--equal_weight >= 0.01)
#' sub2 <- subset(sm, date %in% get_dates(sm)[3:12])
#' sub3 <- subset(sm, 3:12)
#' sub4 <- subset(sm, 1:100) # warning
#'
#' # different selections
#' sel1 <- subset(sm, select = "equal_weight")
#' sel2 <- subset(sm, select = c("equal_weight", "linear"))
#' sel3 <- subset(sm, select = c("linear", "LM_en"))
#' sel4 <- subset(sm, select = list(c("linear", "wsj"), c("linear", "economy")))
#'
#' # different deletions
#' del1 <- subset(sm, delete = "equal_weight")
#' del2 <- subset(sm, delete = c("linear", "LM_en"))
#' del3 <- subset(sm, delete = list(c("linear", "wsj"), c("linear", "economy")))
#' del4 <- subset(sm, delete = c("equal_weight", "linear")) # warning
#'
#' @export
subset.sento_measures <- function(x, subset = NULL, select = NULL, delete = NULL, ...) {
  check_class(x, "sento_measures")

  # subset
  isNumericSubset <- tryCatch(is.numeric(subset), error = function(e) FALSE)
  if (isNumericSubset) {
    if (max(subset) > nobs(x)) {
      warning("At least one row index is greater than nobs(x). Input sento_measures object is returned.")
      return(x)
    }
    measuresNew <- as.data.table(x)[subset, ]
    if (nrow(measuresNew) == 0) {
      warning("No rows retained. Input sento_measures object is returned.")
      return(x)
    } else {
      x <- update_info(x, measuresNew) # subset update
    }
  } else {
    sub <- as.character(substitute(list(subset))[-1L])
    if (length(sub) > 0 && sub != "NULL") {
      sub <- stringi::stri_replace_all(sub, "", regex = " ")
      sub <- stringi::stri_replace_all(sub, "____", regex = "--")
      measures <- as.data.table(x)
      colnames(measures) <- stringi::stri_replace_all(colnames(measures), "____", regex = "--") # -- is problematic here
      measuresNew <- tryCatch(measures[eval(parse(text = sub), parent.frame(2))], error = function(e) NULL)
      if (is.null(measuresNew)) stop("The 'subset' argument must evaluate to logical.")
      colnames(measuresNew) <- stringi::stri_replace_all(colnames(measuresNew), "--", regex = "____")
      if (dim(measuresNew)[1] == 0) {
        warning("No rows selected in subset. Input sento_measures object is returned.")
        return(x)
      }
      x <- update_info(x, measuresNew) # subset update
    }
  }

  # select
  if (!is.null(select)) {
    allOpts <- unlist(get_dimensions(x))
    valid <- unlist(select) %in% allOpts
    if (any(!valid)) {
      stop(paste0("Following components make up none of the sentiment measures: ",
                  paste0(unique(unlist(select)[!valid]), collapse = ', '), "."))
    }

    measures <- as.data.table(x)
    namesList <- stringi::stri_split(colnames(measures), regex = "--")
    if (is.list(select)) {
      ind <- rep(FALSE, length(namesList))
      for (com in select) {
        inds <- sapply(namesList, function(x) return(all(com %in% x)))
        ind[inds == TRUE] <- TRUE
      }
    } else ind <- sapply(namesList, function(x) return(any(select %in% x)))
    if (!any(ind[-1])) {
      warning("No appropriate combination for selection found. Input sento_measures object is returned.")
      return(x)
    }
    measuresNew <- measures[, c(TRUE, ind[-1]), with = FALSE]
    x <- update_info(x, measuresNew) # select update
  }

  # delete
  if (!is.null(delete)) {
    allOpts <- unlist(get_dimensions(x))
    valid <- unlist(delete) %in% allOpts
    if (any(!valid)) {
      stop(paste0("Following components make up none of the sentiment measures: ",
                  paste0(unique(unlist(delete)[!valid]), collapse = ', '), "."))
    }

    measures <- as.data.table(x)
    namesList <- stringi::stri_split(colnames(measures), regex = "--")
    if (is.list(delete)) {
      ind <- rep(FALSE, length(namesList))
      for (com in delete) {
        inds <- sapply(namesList, function(x) return(all(com %in% x)))
        ind[inds == TRUE] <- TRUE
      }
    } else ind <- sapply(namesList, function(x) return(any(delete %in% x)))
    if (all(ind[-1]) || all(!ind[-1])) {
      warning("No appropriate combination found or all measures selected for deletion. Input sento_measures object is returned.")
      return(x)
    }
    measuresNew <- measures[, c(TRUE, !ind[-1]), with = FALSE]
    x <- update_info(x, measuresNew) # delete update
  }

  return(x)
}

