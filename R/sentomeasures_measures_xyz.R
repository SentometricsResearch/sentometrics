
#' Add and fill missing dates
#'
#' @author Samuel Borms
#'
#' @description Adds missing dates between earliest and latest date of a \code{sentomeasures} object, such that the time
#' series are continuous date-wise. Fills in these dates with either 0, the respective latest non-missing value or \code{NA}.
#'
#' @param sentomeasures a \code{sentomeasures} object created using \code{\link{sento_measures}}.
#' @param fill an element of \code{c("zero", "latest", NA)}; the first and last assume missing dates represent zero sentiment,
#' the second assumes missing dates represent constant sentiment.
#' @param dateBefore a date as \code{"yyyy-mm-dd"}, to stretch the sentiment time series from up to the first date. Should
#' be earlier than \code{get_dates(sentomeasures)[1]}, according to the \code{sentomeasures[["by"]]} frequency. If
#' \code{NULL}, then ignored. The values for these dates are set to the values at \code{get_dates(sentomeasures)[1]}.
#' @param dateAfter a date as \code{"yyyy-mm-dd"}, to stretch the sentiment time series up to this date. Should be
#' later than \code{tail(get_dates(sentomeasures), 1)}, according to the \code{sentomeasures[["by"]]} frequency. If
#' \code{NULL}, then ignored.
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
#' l <- setup_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "day", lag = 7, fill = "none")
#' sentomeasures <- sento_measures(corpusSample, l, ctr)
#'
#' # fill measures
#' f1 <- measures_fill(sentomeasures)
#' f2 <- measures_fill(sentomeasures, fill = "latest")
#' f3 <- measures_fill(sentomeasures, fill = NA)
#' f4 <- measures_fill(sentomeasures, fill = "zero",
#'                     dateBefore = get_dates(sentomeasures)[1] - 10,
#'                     dateAfter = tail(get_dates(sentomeasures), 1) + 15)
#'
#' @export
measures_fill <- function(sentomeasures, fill = "zero", dateBefore = NULL, dateAfter = NULL) {
  check_class(sentomeasures, "sentomeasures")

  by <- sentomeasures$by
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

  ts <- seq(start, end, by = by) # continuous date series
  dt <- data.table(date = ts)

  # join and fill as provided into new measures
  measures <- get_measures(sentomeasures)
  measuresFill <- merge(dt, measures, by = "date", all = TRUE) # fills with NA
  if (is.na(fill)) {
    sentomeasures$measures <- measuresFill
    return(sentomeasures)
  } else if (fill == "zero") {
    measuresFill[is.na(measuresFill)] <- 0
  } else if (fill == "latest") {
    if (!is.null(dateBefore)) measuresFill[1, 2:ncol(measures)] <- measures[1, -1]
    measuresFill <- zoo::na.locf(measuresFill)
  } else stop("Input variable 'fill' should be either 'zero', 'latest' or NA.")
  measuresFill <- data.table(date = ts, measuresFill[, lapply(.SD, as.numeric), .SDcols = colnames(measures)[-1]])

  sentomeasures$measures <- measuresFill
  sentomeasures$stats <- compute_stats(sentomeasures) # will be overwritten at end of agg_time() call

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
#' measures need to be selected. One can also supply a \code{list} of such \code{character} vectors, in which case
#' \code{do.combine = TRUE} is set automatically, such that the separately specified combinations are selected.
#' @param do.combine a \code{logical} indicating if only measures for which all (\code{do.combine = TRUE}) or at least one
#' (\code{do.combine = FALSE}) from \code{toSelect} should occur in each sentiment measure's name in the selection. If
#' \code{do.combine = TRUE}, the \code{toSelect} argument can only consist of one lexicon, one feature, and one time weighting
#' scheme at maximum.
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
#' l <- setup_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpusSample, l, ctr)
#'
#' # different selections
#' sel1 <- measures_select(sentomeasures, c("equal_weight"))
#' sel2 <- measures_select(sentomeasures, c("equal_weight", "linear"), do.combine = FALSE)
#' sel3 <- measures_select(sentomeasures, c("linear", "LM_en"))
#' sel4 <- measures_select(sentomeasures, list(c("linear", "wsj"), c("linear", "economy")))
#'
#' @export
measures_select <- function(sentomeasures, toSelect, do.combine = TRUE) {
  check_class(sentomeasures, "sentomeasures")

  allOpts <- unlist(get_dimensions(sentomeasures))
  valid <- unlist(toSelect) %in% allOpts
  if (any(!valid)) {
    stop("Following components make up none of the sentiment measures: ", paste0(unique(toSelect[!valid]), collapse = ', '))
  }

  measures <- get_measures(sentomeasures)
  namesList <- stringi::stri_split(colnames(measures), regex = "--")
  if (is.list(toSelect)) do.combine <- TRUE
  if (do.combine == TRUE) fun <- all
  else fun <- any
  if (is.list(toSelect)) {
    ind <- rep(FALSE, length(namesList))
    for (com in toSelect) {
      inds <- sapply(namesList, function(x) return(fun(com %in% x)))
      ind[inds == TRUE] <- TRUE
    }
  } else ind <- sapply(namesList, function(x) return(fun(toSelect %in% x)))
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
#' l <- setup_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
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

check_merge_dimensions <- function(sentomeasures, features = NA, lexicons = NA, time = NA) {
  check_class(sentomeasures, "sentomeasures")

  # check if columns to merge exist (missings), and if merges have at least two columns to combine and are unique (tooFew)
  missings <- tooFew <- NULL
  if (all(!is.na(features))) {
    missings <- c(missings, unlist(features)[!(unlist(features) %in% sentomeasures$features)])
    for (i in seq_along(features)) {
      if (length(features[[i]]) <= 1 | length(unique(features[[i]])) != length(features[[i]]))
        tooFew <- c(tooFew, names(features)[i])
    }
  }
  if (all(!is.na(lexicons))) {
    missings <- c(missings, unlist(lexicons)[!(unlist(lexicons) %in% sentomeasures$lexicons)])
    for (i in seq_along(lexicons)) {
      if (length(lexicons[[i]]) <= 1 | length(unique(lexicons[[i]])) != length(lexicons[[i]]))
        tooFew <- c(tooFew, names(lexicons)[i])
    }
  }
  if (all(!is.na(time))) {
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
#' \code{list(lex12 = c("lex1", "lex2"))}. See \code{sentomeasures$lexicons} for the exact names to use. Use \code{NA}
#' (default) to apply no merging across this dimension.
#' @param features a \code{list} with unique features to merge at given name, e.g., \cr
#' \code{list(feat12 = c("feat1", "feat2"))}. See \code{sentomeasures$features} for the exact names to use. Use \code{NA}
#' (default) to apply no merging across this dimension.
#' @param time a \code{list} with unique time weighting schemes to merge at given name, e.g., \cr
#' \code{list(tw12 = c("tw1", "tw2"))}. See \code{sentomeasures$time} for the exact names to use. Use \code{NA} (default) to
#' apply no merging across this dimension.
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
#' l <- setup_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpusSample, l, ctr)
#'
#' # perform the merging
#' sentomeasuresMerged <- measures_merge(sentomeasures,
#'                                       time = list(W = c("equal_weight", "linear")),
#'                                       features = list(journals = c("wsj", "wapo")),
#'                                       do.keep = TRUE)
#'
#' \dontrun{
#' # this merging won't work, but produces an informative error message
#' measures_merge(sentomeasures,
#'                time = list(W = c("equal_weight", "almon1")),
#'                lexicons = list(LEX = c("LM_en")),
#'                features = list(journals = c("notInHere", "wapo")))}
#' @export
measures_merge <- function(sentomeasures, features = NA, lexicons = NA, time = NA, do.keep = FALSE) {

  check <- check_merge_dimensions(sentomeasures, features = features, lexicons = lexicons, time = time) # check inputs
  if (check$stop == TRUE)
    stop(paste0(c("Wrong inputs.", check$msg1, check$msg2), collapse = " "))

  measures <- get_measures(sentomeasures)
  toMerge <- list(lexicons = lexicons, features = features, time = time)

  if (do.keep == TRUE) {
    measuresOld <- measures
    namesOld <- colnames(measures)
  }
  # loop over lex(icon), feat(ure) and time lists
  for (across in toMerge[!is.na(toMerge)]) {
    # loop over set of aggregation levels to merge (combine) into given name (e.g., lex12 = c("lex1", "lex2"))
    for (i in seq_along(across)) {
      name <- names(across)[i] # e.g. "lex12"
      cols <- across[[i]] # e.g. c("lex1", "lex2")
      # find all sentiment columns aggregated at one of the 'cols' aggregation levels and stack them into ls
      ls <- list()
      for (elem in cols) {
        sel <- colnames(measures)[stringi::stri_detect(colnames(measures), regex = paste0("\\b", elem, "\\b"))] # exact match
        ls[[elem]] <- measures[, sel, with = FALSE, drop = FALSE]
        measures <- measures[, !sel, with = FALSE, drop = FALSE]
      }
      # take element-wise average for every row/column combination across columns to merge
      if (ncol(ls[[1]] >= 2)) { # ncol across elements of ls is the same
        all <- abind::abind(ls, along = 3)
        merged <- apply(all, c(1, 2), mean, na.rm = TRUE)
      } else merged <- rowSums(abind::abind(ls, along = 2))
      # insert new name at name location of aggregation level (e.g. "lex1--top1" + "lex2--top1" = "lex12--top1")
      nms <- stringi::stri_split(colnames(merged), regex = "--") # list
      loc <- which(stringi::stri_detect(nms[[1]], regex = elem))[1]
      nmsNew <- lapply(nms, function(x) {
        x[loc] <- name
        return(paste0(x, collapse = "--"))
      })
      colnames(merged) <- unlist(nmsNew)
      measures <- cbind(measures, merged) # add back merged columns for further merging if needed
    }
  }
  # add old unmerged measures to merged measures (if do.keep is TRUE)
  if (do.keep == TRUE) measures <- cbind(measures, measuresOld[, !(namesOld %in% colnames(measures)), with = FALSE])

  sentomeasures <- update_info(sentomeasures, measures) # update information in sentomeasures object

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
#' measures need to be deleted. One can also supply a \code{list} of such \code{character} vectors, in which case
#' \code{do.combine = TRUE} is set automatically, such that the separately specified combinations are deleted.
#' @param do.combine a \code{logical} indicating if only measures for which all (\code{do.combine = TRUE}) or at least one
#' (\code{do.combine = FALSE}) from \code{toDelete} should occur in each sentiment measure's name for deletion. If
#' \code{do.combine = TRUE}, the \code{toDelete} argument can only consist of one lexicon, one feature, and one time weighting
#' scheme at maximum.
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
#' l <- setup_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpusSample, l, ctr)
#'
#' # different deletions
#' del1 <- measures_delete(sentomeasures, c("equal_weight"))
#' del2 <- measures_delete(sentomeasures, c("equal_weight", "linear"), do.combine = FALSE)
#' del3 <- measures_delete(sentomeasures, c("linear", "LM_en"))
#' del4 <- measures_delete(sentomeasures, list(c("linear", "wsj"), c("linear", "economy")))
#'
#' @export
measures_delete <- function(sentomeasures, toDelete, do.combine = TRUE) {
  check_class(sentomeasures, "sentomeasures")

  allOpts <- unlist(get_dimensions(sentomeasures))
  valid <- unlist(toDelete) %in% allOpts
  if (any(!valid)) {
    stop("Following components make up none of the sentiment measures: ", paste0(unique(toDelete[!valid]), collapse = ', '))
  }

  measures <- get_measures(sentomeasures)
  namesList <- stringi::stri_split(colnames(measures), regex = "--")
  if (is.list(toDelete)) do.combine <- TRUE
  if (do.combine == TRUE) fun <- all
  else fun <- any
  if (is.list(toDelete)) {
    ind <- rep(FALSE, length(namesList))
    for (com in toDelete) {
      inds <- sapply(namesList, function(x) return(fun(com %in% x)))
      ind[inds == TRUE] <- TRUE
    }
  } else ind <- sapply(namesList, function(x) return(fun(toDelete %in% x)))
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
#' object. The global sentiment measures as outputted can easily be added to regressions as an additional
#' variable using the \code{x} argument in the \code{\link{sento_model}} function. The measures are constructed from
#' weights that indicate the importance (and sign) along each component from the \code{lexicons}, \code{features},
#' and \code{time} dimensions. There is no condition in terms of allowed weights. For example, the global index based
#' on the supplied lexicon weights (\code{"globLex"}) is obtained first by multiplying every sentiment measure with
#' its corresponding weight (meaning, the weight given to the lexicon the sentiment is computed with), then by taking
#' the average per date.
#'
#' @param sentomeasures a \code{sentomeasures} object created using \code{\link{sento_measures}}.
#' @param lexicons a \code{numeric} vector of weights, of size \code{length(sentomeasures$lexicons)}, in the same order.
#' By default set to 1, which means equally weighted.
#' @param features a \code{numeric} vector of weights, of size \code{length(sentomeasures$features)}, in the same order.
#' By default set to 1, which means equally weighted.
#' @param time a \code{numeric} vector of weights, of size \code{length(sentomeasures$time)}, in the same order. By default
#' set to 1, which means equally weighted.
#'
#' @return A \code{data.frame} with the different types of weighted global sentiment measures, named \code{"globLex"},
#' \code{"globFeat"}, \code{"globTime"} and \code{"global"}, with dates as row names. The last measure is an average
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
#' l <- setup_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
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
  global <- as.data.frame(globs)
  row.names(global) <- global$date
  global$date <- NULL

  return(global)
}

