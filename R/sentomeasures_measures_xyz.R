
#' Add and fill missing dates to sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Adds missing dates between earliest and latest date of a \code{sentomeasures} object or two more extreme
#' boundary dates, such that the time series are continuous date-wise. Fills in any missing date with either 0 or the
#' most recent non-missing value.
#'
#' @details The \code{dateBefore} and \code{dateAfter} dates are converted according to the \code{sentomeasures[["by"]]}
#' frequency.
#'
#' @param sentomeasures a \code{sentomeasures} object created using \code{\link{sento_measures}}.
#' @param fill an element of \code{c("zero", "latest")}; the first assumes missing dates represent zero sentiment,
#' the second assumes missing dates represent constant sentiment.
#' @param dateBefore a date as \code{"yyyy-mm-dd"}, to stretch the sentiment time series from up to the first date. Should
#' be earlier than \code{get_dates(sentomeasures)[1]} to take effect. The values for these dates are set to those at
#' \code{get_dates(sentomeasures)[1]}. If \code{NULL}, then ignored.
#' @param dateAfter a date as \code{"yyyy-mm-dd"}, to stretch the sentiment time series up to this date. Should be
#' later than \code{tail(get_dates(sentomeasures), 1)} to take effect. If \code{NULL}, then ignored.
#'
#' @return A modified \code{sentomeasures} object.
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' # construct a sentomeasures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 500)
#' l <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "day", lag = 7, fill = "none")
#' sentomeasures <- sento_measures(corpusSample, l, ctr)
#'
#' # fill measures
#' f1 <- measures_fill(sentomeasures)
#' f2 <- measures_fill(sentomeasures, fill = "latest")
#' f3 <- measures_fill(sentomeasures, fill = "zero",
#'                     dateBefore = get_dates(sentomeasures)[1] - 10,
#'                     dateAfter = tail(get_dates(sentomeasures), 1) + 15)
#'
#' @export
measures_fill <- function(sentomeasures, fill = "zero", dateBefore = NULL, dateAfter = NULL) {
  check_class(sentomeasures, "sentomeasures")

  by <- sentomeasures$ctr$time$weightingParam$by
  dates <- get_dates(sentomeasures)

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
  dt <- data.table(date = ts)

  # join and fill as provided into new measures
  measures <- get_measures(sentomeasures)
  measuresFill <- merge(dt, measures, by = "date", all = TRUE) # fills with NA
  if (fill == "zero") {
    measuresFill[is.na(measuresFill)] <- 0
  } else if (fill == "latest") {
    if (!is.null(dateBefore)) measuresFill[1, 2:ncol(measures)] <- measures[1, -1]
    measuresFill <- cbind(dt, as.data.table(fill_NAs(as.matrix(measuresFill[, -1]))))
  } else stop("Input variable 'fill' should be either 'zero' or 'latest'.")
  measuresFill <- data.table(date = ts, measuresFill[, lapply(.SD, as.numeric), .SDcols = colnames(measures)[-1]])

  sentomeasures$measures <- measuresFill
  sentomeasures$stats <- compute_stats(sentomeasures) # will be overwritten at end of aggregate_time() call

  return(sentomeasures)
}

#' Select sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Selects all sentiment measures which include either all of the given selection components combined,
#' or those who's name consist of at least one of the selection components.
#'
#' @param sentomeasures a \code{sentomeasures} object created using \code{\link{sento_measures}}.
#' @param toSelect a \code{character} vector of the lexicon, feature and time weighting scheme names, to indicate which
#' measures need to be selected, or as a \code{list} of \code{character} vectors, possibly with separately specified
#' combinations (consisting of one unique lexicon, one unique feature, and one unique time weighting scheme at maximum).
#'
#' @return A modified \code{sentomeasures} object, with only the sentiment measures required, including updated information
#' and statistics, but the original sentiment scores \code{data.table} untouched.
#'
#' @seealso \code{\link{measures_delete}}
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' # construct a sentomeasures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 500)
#' l <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpusSample, l, ctr)
#'
#' # different selections
#' sel1 <- measures_select(sentomeasures, c("equal_weight"))
#' sel2 <- measures_select(sentomeasures, c("equal_weight", "linear"))
#' sel3 <- measures_select(sentomeasures, c("linear", "LM_en"))
#' sel4 <- measures_select(sentomeasures, list(c("linear", "wsj"), c("linear", "economy")))
#'
#' @export
measures_select <- function(sentomeasures, toSelect) {
  check_class(sentomeasures, "sentomeasures")

  allOpts <- unlist(get_dimensions(sentomeasures))
  valid <- unlist(toSelect) %in% allOpts
  if (any(!valid)) {
    stop(paste0("Following components make up none of the sentiment measures: ",
                paste0(unique(unlist(toSelect)[!valid]), collapse = ', '), "."))
  }

  measures <- get_measures(sentomeasures)
  namesList <- stringi::stri_split(colnames(measures), regex = "--")
  if (is.list(toSelect)) {
    ind <- rep(FALSE, length(namesList))
    for (com in toSelect) {
      inds <- sapply(namesList, function(x) return(all(com %in% x)))
      ind[inds == TRUE] <- TRUE
    }
  } else ind <- sapply(namesList, function(x) return(any(toSelect %in% x)))
  if (!any(ind[-1])) {
    warning("No appropriate combination found. Input sentomeasures object is returned.")
    return(sentomeasures)
  }
  measuresNew <- measures[, c(TRUE, ind[-1]), with = FALSE]

  sentomeasures <- update_info(sentomeasures, measuresNew) # update information in sentomeasures object

  return(sentomeasures)
}

#' Subset sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Subsets rows of the sentiment measures based on its columns.
#'
#' @param sentomeasures a \code{sentomeasures} object created using \code{\link{sento_measures}}.
#' @param subset a logical expression indicating the rows to keep.
#'
#' @return A modified \code{sentomeasures} object, with only the kept rows, including updated information
#' and statistics, but the original sentiment scores \code{data.table} untouched.
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' # construct a sentomeasures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 500)
#' l <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpusSample, l, ctr)
#'
#' # different subsets
#' sub1 <- measures_subset(sentomeasures, HENRY_en--economy--equal_weight >= 0.01)
#' sub2 <- measures_subset(sentomeasures,
#'    date %in% seq(as.Date("2000-01-01"), as.Date("2013-12-01"), by = "month"))
#'
#' @export
measures_subset <- function(sentomeasures, subset) {
  check_class(sentomeasures, "sentomeasures")

  sub <- as.character(substitute(list(subset))[-1L])
  if (length(sub) > 0) {
    sub <- stringi::stri_replace_all(sub, "", regex = " ")
    sub <- stringi::stri_replace_all(sub, "____", regex = "--")
    measures <- get_measures(sentomeasures)
    colnames(measures) <- stringi::stri_replace_all(colnames(measures), "____", regex = "--") # -- is problematic here
    measures <- tryCatch(measures[eval(parse(text = sub), parent.frame())], error = function(e) return(NULL))
    if (is.null(measures)) stop("The 'subset' argument must evaluate to logical.")
    colnames(measures) <- stringi::stri_replace_all(colnames(measures), "--", regex = "____")
  }

  if (dim(measures)[1] == 0) {
    warning("No rows selected in subset. Input sentomeasures object is returned.")
    return(sentomeasures)
  } else {
    sentomeasures <- update_info(sentomeasures, measures) # update information in sentomeasures object
  }

  return(sentomeasures)
}

check_merge_dimensions <- function(sentomeasures, features = NULL, lexicons = NULL, time = NULL) {
  check_class(sentomeasures, "sentomeasures")

  # check if columns to merge exist (missings), and if merges have at least two columns to combine and are unique (tooFew)
  missings <- tooFew <- NULL
  if (!is.null(features)) {
    missings <- c(missings, unlist(features)[!(unlist(features) %in% sentomeasures$features)])
    for (i in seq_along(features)) {
      if (length(features[[i]]) <= 1 | length(unique(features[[i]])) != length(features[[i]]))
        tooFew <- c(tooFew, names(features)[i])
    }
  }
  if (!is.null(lexicons)) {
    missings <- c(missings, unlist(lexicons)[!(unlist(lexicons) %in% sentomeasures$lexicons)])
    for (i in seq_along(lexicons)) {
      if (length(lexicons[[i]]) <= 1 | length(unique(lexicons[[i]])) != length(lexicons[[i]]))
        tooFew <- c(tooFew, names(lexicons)[i])
    }
  }
  if (!is.null(time)) {
    missings <- c(missings, unlist(time)[!(unlist(time) %in% sentomeasures$time)])
    for (i in seq_along(time)) {
      if (length(time[[i]]) <= 1 | length(unique(time[[i]])) != length(time[[i]]))
        tooFew <- c(tooFew, names(time)[i])
    }
  }

  # assemble warning messages if any
  msg1 <- msg2 <- NULL
  if (length(missings) > 0) {
    msg1 <- paste0("Following columns to merge are not found: ",
                   paste0(missings, collapse = ", "), ".")
  }
  if (length(tooFew) > 0) {
    msg2 <- paste0("Following merges have less than two or not all unique columns: ",
                   paste0(tooFew, collapse = ", "), ".")
  }
  if (length(msg1) > 0 | length((msg2) > 0)) stop <- TRUE else stop <- FALSE

  return(list(stop = stop, msg1 = msg1, msg2 = msg2))
}

#' Merge sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Merges sentiment measures by combining across provided lexicons, features, and time weighting schemes
#' dimensions. The combination occurs by taking the mean of the relevant measures.
#'
#' @param sentomeasures a \code{sentomeasures} object created using \code{\link{sento_measures}}. This is necessary to check
#' whether the other input arguments make sense.
#' @param lexicons a \code{list} with unique lexicons to merge at given name, e.g., \cr
#' \code{list(lex12 = c("lex1", "lex2"))}. See \code{sentomeasures$lexicons} for the exact names to use. Use \code{NULL}
#' (default) to apply no merging across this dimension.
#' @param features a \code{list} with unique features to merge at given name, e.g., \cr
#' \code{list(feat12 = c("feat1", "feat2"))}. See \code{sentomeasures$features} for the exact names to use. Use \code{NULL}
#' (default) to apply no merging across this dimension.
#' @param time a \code{list} with unique time weighting schemes to merge at given name, e.g., \cr
#' \code{list(tw12 = c("tw1", "tw2"))}. See \code{sentomeasures$time} for the exact names to use. Use \code{NULL} (default)
#' to apply no merging across this dimension.
#' @param do.keep a \code{logical} indicating if the original sentiment measures should be kept (i.e., the merged
#' sentiment measures will be added to the current sentiment measures as additional indices if \code{do.keep = TRUE}).
#'
#' @return A modified \code{sentomeasures} object, with only the sentiment measures required, including updated information
#' and statistics, but the original sentiment scores \code{data.table} untouched.
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' # construct a sentomeasures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 500)
#' l <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpusSample, l, ctr)
#'
#' # merging across specified components
#' sentomeasuresMerged <- measures_merge(sentomeasures,
#'                                       time = list(W = c("equal_weight", "linear")),
#'                                       features = list(journals = c("wsj", "wapo")),
#'                                       do.keep = TRUE)
#'
#' # merging in full
#' dims <- get_dimensions(sentomeasures)
#' sentomeasuresFull <- measures_merge(sentomeasures,
#'                                     lexicons = list(L = dims[["lexicons"]]),
#'                                     time = list(T = dims[["time"]]),
#'                                     features = list(F = dims[["features"]]))
#'
#' \dontrun{
#' # this merging will not work, but produces an informative error message
#' measures_merge(sentomeasures,
#'                time = list(W = c("equal_weight", "almon1")),
#'                lexicons = list(LEX = c("LM_en")),
#'                features = list(journals = c("notInHere", "wapo")))}
#' @export
measures_merge <- function(sentomeasures, features = NULL, lexicons = NULL, time = NULL, do.keep = FALSE) {

  stopifnot(is.null(features) || is.list(features))
  stopifnot(is.null(lexicons) || is.list(lexicons))
  stopifnot(is.null(time) || is.list(time))

  check <- check_merge_dimensions(sentomeasures, features = features, lexicons = lexicons, time = time) # check inputs
  if (check$stop == TRUE)
    stop(paste0(c("Wrong inputs.", check$msg1, check$msg2), collapse = " "))

  measures <- get_measures(sentomeasures)
  toMerge <- list(lexicons = lexicons, features = features, time = time)

  if (do.keep == TRUE) {
    measuresOld <- measures
    namesOld <- colnames(measures)
  }
  # loop over lexicons, features and time lists
  for (across in toMerge) {
    # loop over set of aggregation levels to merge (combine) into given name (e.g., lex12 = c("lex1", "lex2"))
    for (i in seq_along(across)) {
      name <- names(across)[i] # e.g. "lex12"
      cols <- across[[i]] # e.g. c("lex1", "lex2")
      # find all sentiment columns aggregated at one of the 'cols' aggregation levels and stack them into ls
      ls <- sels <- as.list(1:length(cols))
      names(ls) <- names(sels) <- cols
      for (elem in cols) {
        sel <- colnames(measures)[stringi::stri_detect(colnames(measures), regex = paste0("\\b", elem, "\\b"))] # exact match
        selMeas <- measures[, sel, with = FALSE, drop = FALSE]
        nms <- stringi::stri_split(colnames(selMeas), regex = "--")
        loc <- which(stringi::stri_detect(nms[[1]], regex = elem))[1]
        nmsNew <- sapply(nms, function(x) {
          x[loc] <- name
          paste0(x, collapse = "--")
        })
        colnames(selMeas) <- nmsNew
        ls[[elem]] <- selMeas
        sels[[elem]] <- sel
      }
      common <- Reduce(intersect, lapply(ls, colnames))
      ls <- lapply(1:length(ls), function(k) {
        m <- ls[[k]]
        ind <- which(colnames(m) %in% common)
        measures <<- measures[, !sels[[k]][ind], with = FALSE, drop = FALSE] # drop columns to merge
        m[, ind, with = FALSE, drop = FALSE]
      })
      # take element-wise average for every row/column combination across columns to merge
      if (ncol(ls[[1]]) >= 2) { # ncol across elements of ls is the same
        all <- array(NA, dim = c(nrow(ls[[1]]), ncol(ls[[2]]), length(ls)))
        for (k in 1:length(ls)) all[, , k] <- as.matrix(ls[[k]])
        merged <- apply(all, c(1, 2), mean, na.rm = TRUE)
        colnames(merged) <- colnames(ls[[length(ls)]])
      } else {
        merged <- as.matrix(rowMeans(do.call(cbind, ls)))
        colnames(merged) <- colnames(ls[[length(ls)]])
      }
      measures <- cbind(measures, merged) # add back merged columns
    }
  }
  # add old unmerged measures to merged measures (if do.keep is TRUE)
  if (do.keep == TRUE) measures <- cbind(measures, measuresOld[, !(namesOld %in% colnames(measures)), with = FALSE])

  sentomeasures <- update_info(sentomeasures, measures,
                               merges = toMerge) # update information in sentomeasures object

  return(sentomeasures)
}

#' Delete sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Deletes all sentiment measures which include either all of the given deletion components combined,
#' or those who's name consist of at least one of the deletion components.
#'
#' @param sentomeasures a \code{sentomeasures} object created using \code{\link{sento_measures}}.
#' @param toDelete a \code{character} vector of the lexicon, feature and time weighting scheme names, to indicate which
#' measures need to be deleted, or as a \code{list} of \code{character} vectors, possibly with separately specified
#' combinations (consisting of one unique lexicon, one unique feature, and one unique time weighting scheme at maximum).
#'
#' @return A modified \code{sentomeasures} object, with the required sentiment measures deleted, including updated information
#' and statistics, but the original sentiment scores \code{data.table} untouched.
#'
#' @seealso \code{\link{measures_select}}
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' # construct a sentomeasures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 500)
#' l <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpusSample, l, ctr)
#'
#' # different deletions
#' del1 <- measures_delete(sentomeasures, c("equal_weight"))
#' del2 <- measures_delete(sentomeasures, c("linear", "LM_en"))
#' del3 <- measures_delete(sentomeasures, list(c("linear", "wsj"), c("linear", "economy")))
#' del4 <- measures_delete(sentomeasures, c("equal_weight", "linear")) # warning
#'
#' @export
measures_delete <- function(sentomeasures, toDelete) {
  check_class(sentomeasures, "sentomeasures")

  allOpts <- unlist(get_dimensions(sentomeasures))
  valid <- unlist(toDelete) %in% allOpts
  if (any(!valid)) {
    stop(paste0("Following components make up none of the sentiment measures: ",
                paste0(unique(unlist(toDelete)[!valid]), collapse = ', '), "."))
  }

  measures <- get_measures(sentomeasures)
  namesList <- stringi::stri_split(colnames(measures), regex = "--")
  if (is.list(toDelete)) {
    ind <- rep(FALSE, length(namesList))
    for (com in toDelete) {
      inds <- sapply(namesList, function(x) return(all(com %in% x)))
      ind[inds == TRUE] <- TRUE
    }
  } else ind <- sapply(namesList, function(x) return(any(toDelete %in% x)))
  if (all(ind[-1]) || all(!ind[-1])) {
    warning("No appropriate combination found or all measures selected for deletion. Input sentomeasures object is returned.")
    return(sentomeasures)
  }
  measuresNew <- measures[, c(TRUE, !ind[-1]), with = FALSE]

  sentomeasures <- update_info(sentomeasures, measuresNew) # update information in sentomeasures object

  return(sentomeasures)
}

#' Merge sentiment measures into multiple weighted global sentiment indices
#'
#' @author Samuel Borms
#'
#' @description Merges all sentiment measures into a weighted global textual sentiment measure for each of the
#' \code{lexicons}, \code{features}, and \code{time} dimensions.
#'
#' @details In contrast to other \code{measures_xyz} functions, this particular function returns no new \code{sentomeasures}
#' object. The measures are constructed from weights that indicate the importance (and sign) along each component from the
#' \code{lexicons}, \code{features}, and \code{time} dimensions. There is no restriction in terms of allowed weights. For
#' example, the global index based on the supplied lexicon weights (\code{"globLex"}) is obtained first by multiplying
#' every sentiment measure with its corresponding weight (meaning, the weight given to the lexicon the sentiment is
#' computed with), then by taking the average per date.
#'
#' @param sentomeasures a \code{sentomeasures} object created using \code{\link{sento_measures}}.
#' @param lexicons a \code{numeric} vector of weights, of size \code{length(sentomeasures$lexicons)}, in the same order.
#' By default set to 1, which means equally weighted.
#' @param features a \code{numeric} vector of weights, of size \code{length(sentomeasures$features)}, in the same order.
#' By default set to 1, which means equally weighted.
#' @param time a \code{numeric} vector of weights, of size \code{length(sentomeasures$time)}, in the same order. By default
#' set to 1, which means equally weighted.
#'
#' @return A \code{data.table} with the different types of weighted global sentiment measures, named \code{"globLex"},
#' \code{"globFeat"}, \code{"globTime"} and \code{"global"}, with \code{"date"} as the first column. The last measure is an average
#' of the the three other measures.
#'
#' @seealso \code{\link{sento_model}}
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' # construct a sentomeasures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 500)
#' l <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpusSample, l, ctr)
#'
#' # merge into one global sentiment measure, with specified weighting for lexicons and features
#' global <- measures_global(sentomeasures,
#'                           lexicons = c(0.40, 0.60),
#'                           features = c(0.10, -0.20, 0.30, -1),
#'                           time = 1)
#'
#' @export
measures_global <- function(sentomeasures, lexicons = 1, features = 1, time = 1) {
  check_class(sentomeasures, "sentomeasures")

  dims <- get_dimensions(sentomeasures)
  n <- sapply(dims, length)
  weightsInp <- list(features, lexicons, time)
  weights <- sapply(1:3, function(i) {
    if (length(weightsInp[[i]]) == 1)
      w <- as.list(rep(1/n[i], n[i])) # modify weights if equal to default value of 1
    else {
      w <- as.list(weightsInp[[i]])
      if (length(w) != n[i])
        stop("All weights must be equal in length to the respective number of components.")
    }
    names(w) <- dims[[i]] # named weight lists
    return(w)
  })

  measuresLong <- get_measures(sentomeasures, format = "long")
  measuresLong[, "wFeat" := unlist(weights[[1]][measuresLong[["features"]]])] # weights features
  measuresLong[, "wLex" := unlist(weights[[2]][measuresLong[["lexicons"]]])] # weights lexicon
  measuresLong[, "wTime" :=- unlist(weights[[3]][measuresLong[["time"]]])] # weights time
  globs <- measuresLong[, list(globLex = mean(value * wLex),
                               globFeat = mean(value * wFeat),
                               globTime = mean(value * wTime)), by = date]
  globs[["global"]] <- rowMeans(globs[, -1])

  return(globs)
}

#' Update sentiment measures
#'
#' @author Jeroen Van Pelt, Samuel Borms, Andres Algaba
#'
#' @description Updates a sentomeasures object based on a new corpus provided. Sentiment for the unseen corpus
#' texts calculated and aggregated applying the control variables from the input sentomeasures object.
#'
#' @param sentocorpus a \code{sentocorpus} object created with \code{\link{sento_corpus}}.
#' @param sentomeasures \code{sentomeasures} object created with \code{\link{sento_measures}}
#' @param lexicons a \code{sentolexicons} object created with \code{\link{sento_lexicons}}.
#
#' @return An updated \code{sentomeasures} object.
#'
#' @seealso \code{\link{sento_measures}}, \code{\link{compute_sentiment}}
#'
#' @examples
#' data("usnews", package = "sentometrics")
#'
#' corpus1 <- sento_corpus(usnews[1:500,])
#' ctr <- ctr_agg(howTime = "linear", by = "year", lag = 3)
#' l <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")],
#'                     list_valence_shifters[["en"]])
#' sentomeasures <- sento_measures(corpus1, l, ctr)
#' corpus2 <- sento_corpus(usnews[400:2000,])
#' sentomeasuresNew <- measures_update(sentomeasures, corpus2, l)
#'
#' @export
measures_update <- function(sentomeasures, sentocorpus, lexicons) {
  check_class(sentomeasures, "sentomeasures")
  check_class(sentocorpus, "sentocorpus")

  if (!setequal(get_dimensions(sentomeasures)$lexicons, names(lexicons)[names(lexicons) != "valence"])) {
    stop(paste0("Provided lexicon names are not the same as lexicons used in input sentomeasures object."))
  }

  ctr <- sentomeasures$ctr
  sentiment <- sentomeasures$sentiment
  partialCorpus <- quanteda::corpus_subset(sentocorpus, !quanteda::docnames(sentocorpus) %in% sentiment$id)
  if (length(quanteda::texts(partialCorpus)) > 0) {
    partialSentiment <- compute_sentiment(partialCorpus, lexicons, how = ctr$within$howWithin, nCore = ctr$nCore)
    sentiment <- sentiment_bind(sentiment, partialSentiment)
  }

  sentomeasuresUpdated <- aggregate(sentiment, ctr)
  sentomeasuresUpdated
}

