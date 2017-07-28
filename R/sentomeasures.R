
### TODO: normalization of measures + statistics

#' One-way road towards sentomeasures object
#'
#' @description Wrapper function which assembles calls to \code{compute_sentiment()} and \code{perform_agg()}, and includes
#' the input \code{corpuS} and computed sentiment scores in its output. Serves as the most direct way towards a panel of
#' textual sentiment measures, and a \code{sentomeasures} object.
#'
#' @param corpuS a \code{corpuS} object.
#' @param lexicons output from a \code{setup_lexicons()} call.
#' @param ctr output from a \code{ctr_agg()} call.
#'
#' @return A \code{sentomeasures} object.
#'
#' @seealso \code{\link{compute_sentiment}}, \code{\link{perform_agg}}
#'
#' @import data.table
#' @export
sento_measures<- function(corpuS, lexicons, ctr) {

  check_class(corpuS, "corpuS")

  toAgg <- compute_sentiment(corpuS, lexicons, how = ctr$howWithin)
  sentomeasures <- perform_agg(toAgg, ctr)

  return(sentomeasures)
}

#' Setup lexicons format
#'
#' @description Structures provided lexicons and potentially valence words. Makes use of the \code{as_key()} function from the
#' \pkg{quanteda} package.
#'
#' @param lexiconsRaw a list of lexicons, with each element being a \code{data.frame} with a words column and a polarity score
#' column. The lexicons should be appropriately named for clarity in terms of subsequently obtained sentiment measures.
#' Alternatively, this argument can be a \code{character} vector specifying which built-in lexicons
#' (\code{Sentometrics::LEXICONS}) to use.
#' @param valenceWords a \code{data.frame} with a words column and a type indicating ... ### (negators, amplifiers, etc.)
#' Alternatively, this argument can be a \code{character} vector specifying which built-in valence words to use.
#' @param do.split a \code{logical} that if \code{TRUE} splits every lexicon into a separate positive polarity and negative
#' polarity lexicon.
#'
#' @return a list with each lexicon as a \code{data.table} list element according to its name, and the element
#' \code{valenceWords} that comprises the valence words. Every \code{x} column contains the words, every \code{y} column
#' containts the polarity or other quantitative information.
#'
#' @seealso \code{\link[sentimentr]{as_key}}
#'
#' @export
setup_lexicons <- function(lexiconsRaw, valenceWords = NULL, do.split = FALSE) {

  ### format of lexicons_raw and valenceWords to define
  ### add built-in lexicons (add possibility for lexiconsRaw to be character + valenceWords input + combined input)

  if (!is.character(lexiconsRaw)) {
    # check for duplicated lexicon names
    if (sum(duplicated(names(lexiconsRaw))) > 0) {
      duplics <- unique(names(lexiconsRaw[duplicated(names(lexiconsRaw))]))
      stop(paste0("Names of lexicons are not unique. Following names occur at least twice: ",
                  paste0(duplics, collapse = ", ")))
    }
    # convert to sentimentr lexicons and valence shifters format
    lexicons <- suppressWarnings(lapply(lexiconsRaw, sentimentr::as_key))
    lexicons <- lapply(lexicons, function(x) {names(x) <- c("x", "y"); return(x)})
    names(lexicons) <- names(lexiconsRaw)
  } else {
    ### warn if some named lexicons are not part of LEXICONS
    lexicons <- Sentometrics::LEXICONS[lexiconsRaw]
  }
  if (!is.null(valenceWords)) {
    if (!is.character(valenceWords)) {
      val <- suppressWarnings(sentimentr::as_key(valenceWords, comparison = NULL))
    } else {
      val <- Sentometrics::VALENCE[valenceWords]
    }
    # discard valence words that also occur in at least one of the lexicons
    sames <- c()
    for(lex in lexicons) {
      same <- which(val$x %in% lex$x) # x are the words (sentimentr format)
      sames <- c(sames, same)
    }
  }

  # split each lexicon into a positive and a negative polarity words only lexicon
  if (do.split) {
    lexiconsPos <- lapply(lexicons, function(lex) return(lex[lex$y > 0]))
    names(lexiconsPos) <- paste0(names(lexicons), "_POS")

    lexiconsNeg <- lapply(lexicons, function(lex) return(lex[lex$y < 0]))
    names(lexiconsNeg) <- paste0(names(lexicons), "_NEG")

    lexicons <- c(lexiconsPos, lexiconsNeg)
  }

  if (!is.null(valenceWords)) lexicons[["valenceWords"]] <- val[!unique(sames), ]

  return(lexicons)
}

.compute_sentiment <- function(corpuS, lexicons, how = get_hows()$words) {

  check_class(corpuS, "corpuS")

  if (length(how) > 1) how <- how[1]

  lexNames <- names(lexicons)[names(lexicons) != "valenceWords"]
  features <- corpuS$features

  # frequency-based document-feature matrix (rows are corpus ids, columns are words)
  dfm <- quanteda::dfm(quanteda::tokenize(corpuS, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE,
                                          remove_separators = TRUE, ngrams = 1), verbose = FALSE)

  ### add remove = stopwords("english") in dfm... (or other specified stopwords)?
  ### valence words: ngrams (1:n), negation words in text ==> not_, pos lexicon = pos + not_ neg, one-word lexicons ideally

  allWords <- quanteda::featnames(dfm)
  wCounts <- quanteda::rowSums(dfm, na.rm = TRUE)

  if (how == "counts" | how == "equal-weight") {
    fdm <- quanteda::t(dfm) # feature-document matrix
  } else {
    if (how == "proportional") { # proportional w.r.t. words frequency vs. total words frequency per document
      weights <- quanteda::tf(dfm, scheme = "prop") # weight = (words freq. / total words freq.) per document
    } else if (how == "tf-idf") {
      weights <- quanteda::tfidf(dfm, normalize = TRUE)
    } else stop("Please select an appropriate aggregation 'how'.")
    fdmWeighted <- quanteda::t(weights)
  }

  s <- as.data.table(matrix(0, nrow = quanteda::ndoc(corpuS), ncol = length(lexNames)))
  names(s) <- lexNames

  for (lexicon in lexNames) {
    lexWords <- lexicons[[lexicon]]$x
    lexScores <- lexicons[[lexicon]]$y

    # locate polarized words and set weights to their polarity or keep at zero
    allScores <- rep(0, length(allWords))
    polInd <- allWords %in% lexWords
    polWords <- allWords[polInd]
    allScores[polInd] <- lexScores[lexWords %in% polWords]
    names(allScores) <- allWords

    # scores per document equal to (frequency * weight * polarity score)
    if (how == "counts") {
      scores <- quanteda::rowSums(quanteda::t(fdm * allScores))
    } else if (how == "equal-weight") {
      scores <- quanteda::rowSums(quanteda::t(fdm * allScores)) / wCounts
    } else scores <- quanteda::rowSums(quanteda::t(fdmWeighted * allScores))

    # set NA/NaN sentiment to 0 (e.g. if document contains no words) and put scores under appropriate lexicon column
    scores[is.na(scores)] <- 0
    s[, (lexicon) := scores]
  }

  # structure: date - feature1 - ... - word_count - lexicon1 (sentiment) - ...
  s <- as.data.table(cbind(quanteda::docvars(corpuS), word_count = wCounts, s))

  # compute feature-sentiment per document for all lexicons and order by date
  sent <- get_features_sentiment(s, features, lexNames)
  sent <- sent[order(date)]

  sentOut <- list(corpuS = corpuS,
                  sentiment = sent,
                  features = features,
                  lexicons = lexNames)

  return(sentOut)
}

#' Computation of document-level sentiment across features and lexicons
#'
#' @description Given a corpus of texts, computes sentiment per document starting from the bag-of-words approach,
#' based on the lexicons provided and a preferred aggregation across words per document scheme. Relies partly on the
#' \pkg{quanteda} package. The scores computed are net sentiment (sum of positive minus sum of negative scores). For a
#' separate calculation of positive (resp. negative) sentiment, one has to provide distinct positive (resp. negative)
#' lexicons. This can be done using the \code{do.split} option in the \code{setup_lexicons()}, which automatically splits
#' any lexicon into positive and negative polarity.
#'
#' @param corpuS a \code{corpuS} object.
#' @param lexicons output from a \code{setup_lexicons()} call.
#' @param how a single \code{character} vector defining how aggregation within documents will be performed. For currently
#' available options on how aggregation can occer, access \code{get_hows()$words}.
#'
#' @return A list containing:
#' \item{corpuS}{the supplied \code{corpuS} object.}
#' \item{sent}{a sentiment scores \code{data.table} with dates and features--lexicons sentiment scores columns.}
#' \item{features}{a \code{character} vector of the different features.}
#' \item{lexicons}{a \code{character} vector of the different lexicons used.}
#'
#' @export
compute_sentiment <- compiler::cmpfun(.compute_sentiment)

.get_features_sentiment <- function(sent, features, lexNames) {

  # multiply lexicons with features to obtain feature-sentiment scores per lexicon
  for (lexicon in lexNames) {
    for (feature in features) {
      name <- paste0(lexicon, "--", feature)
      sent[, name] <- sent[, lexicon, with = FALSE] * sent[, feature, with = FALSE]
    }
  }
  sent[, eval(c(lexNames, features)) := NULL] # remove since replaced by lexicon--feature columns

  return(sent)
}

get_features_sentiment <- compiler::cmpfun(.get_features_sentiment)

#' Aggregate textual sentiment across documents and time
#'
#' @description Condense document-level textual sentiment scores into a panel of textual sentiment
#' measures by aggregating across documents and time.
#'
#' @param toAgg output from a \code{compute_sentiment()} call, a list with as main component a sentiment scores
#' \code{data.table} with dates and features--lexicons sentiment scores columns.
#' @param ctr output from a \code{ctr_agg()} call.
#'
#' @return A list containing:
#' \item{measures}{a \code{sentomeasures} object.}
#' \item{features}{a \code{character} vector of the different features.}
#' \item{lexicons}{a \code{character} vector of the different lexicons used.}
#' \item{time}{a \code{character} vector of the different time weighting schemes used.}
#'
#' @seealso \code{\link{compute_sentiment}}, \code{\link{ctr_agg}}
#'
#' @export
perform_agg <- function(toAgg, ctr) {

  howDocs <- ctr$howDocs
  howTime <- ctr$howTime
  do.ignoreZeros <- ctr$do.ignoreZeros
  by <- ctr$by
  lag <- ctr$lag
  otherVars <- ctr$other # list

  aggDocs <- agg_documents(toAgg, by = by, how = howDocs, do.ignoreZeros = do.ignoreZeros)
  sentomeasures <- agg_time(aggDocs, lag = lag, how = howTime, otherVars)

  if (!("sentomeasures" %in% class(sentomeasures))) class(sentomeasures) <- c("sentomeasures")

  return(sentomeasures)
}

agg_documents <- function(toAgg, by, how = get_hows()$docs, do.ignoreZeros = FALSE) {

  features <- toAgg$features
  lexNames <- toAgg$lexicons
  sent <- toAgg$sentiment

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
  if (do.ignoreZeros) sent[, names(sent)] <-
    sent[, names(sent), with = FALSE][, lapply(.SD, function(x) replace(x, which(x == 0), NA))]

  # aggregate feature-sentiment per document by date for all lexicon columns
  if (how == "equal-weight") {
    measures <- sent[, lapply(.SD, mean, na.rm = TRUE), by = date]
  } else if (how == "proportional") { # proportional w.r.t. words in document vs. total words in all documents per date
    measures <- sent[, lapply(.SD, function(x) sum(x * word_count / sum(word_count, na.rm = TRUE), na.rm = TRUE)), by = date]
  }
  measures$word_count <- NULL

  sentomeasures <- list(measures = measures,
                        features = features,
                        lexicons = lexNames,
                        time = NA,
                        by = by,
                        sentiment = sent)

  class(sentomeasures) <- c("sentomeasures")

  return(sentomeasures)
}

agg_time <- function(sentomeasures, lag, how = get_hows()$time, ...) {

  check_class(sentomeasures, "sentomeasures")

  dots <- tryCatch(list(...)[[1]], # extract list from list of list
                   error = function(x) list(...))

  # construct or pass on own weights data.frame and check for duplicated names
  if (how == "own") weights <- dots$weights
  else weights <- setup_time_weights(lag, how, dots)

  if (sum(duplicated(colnames(weights))) > 0) {
    duplics <- unique(colnames(weights)[duplicated(colnames(weights))])
    stop(paste0("Names of weighting schemes are not unique. Following names occur at least twice: ",
                paste0(duplics, collapse = ", ")))
  }

  # apply rolling time window, if not too large, for every weights column and combine all new measures column-wise
  measures <- sentomeasures$measures
  toRoll <- measures[, 2:ncol(measures)]

  n <- nrow(weights)
  m <- nrow(measures)
  if (n > m)
    stop("Rolling time aggregation window (= ", n, ") is too large for number of observations per measure (= ", m, ")")

  for (i in 1:ncol(weights)) {
    w <- weights[, i]
    name <- colnames(weights)[i]

    # add <- data.frame(zoo::rollapplyr(toRoll, width = n, by.column = TRUE, FUN = roll_weights, w = w, align = "right"))
    add <- RcppRoll::roll_sum(as.matrix(toRoll), n = n, weights = as.vector(w), align = "right")
    colnames(add) <- paste0(colnames(toRoll), "--", name)

    if (i == 1) measuresAggTime <- add
    else measuresAggTime <- cbind(measuresAggTime, add)
  }
  measuresAggTime <- as.data.table(measuresAggTime)

  if (n > 1) date <- measures$date[-1:-(n-1)]
  else date <- measures$date
  measuresAggTime$date <- date
  measuresAggTime <- setcolorder(measuresAggTime, c("date", colnames(measuresAggTime)[-ncol(measuresAggTime)]))

  sentomeasures$measures <- measuresAggTime
  sentomeasures$time <- colnames(weights)

  return(sentomeasures)
}

#' Merge sentiment measures
#'
#' @description Merge (further aggregate) measures by combining across the lexicons, features and time weighting schemes
#' dimensions. The combination occurs by taking the mean of the relevant measures.
#'
#' @param ctr output from a \code{ctr_merge()} call.
#'
#' @return A modified \code{sentomeasures} object.
#'
#' @seealso \code{\link{ctr_merge}}
#'
#' @export
merge_measures <- function(ctr) {

  ### melt from data.table useful?
  ### option for computation of "global" sentiment measure

  sentomeasures <- ctr$sentomeasures
  measures <- sentomeasures$measures
  toMerge <- ctr[c("lex", "feat", "time")]
  do.keep <- ctr$do.keep

  if (do.keep) {
    measuresOld <- measures
    namesOld <- colnames(measures)
  }

  # loop over lex(icon), feat(ure) and time lists
  for (across in toMerge[!is.na(toMerge)]) {
    # loop over set of aggregation levels to merge (combine) into given name (e.g. lex12 = c("lex1", "lex2"))
    for (i in seq_along(across)) {
      name <- names(across)[i]
      cols <- across[[i]]

      # find all sentiment columns aggregated at one of the 'cols' aggregation levels and stack them into ls
      ls <- list()
      for (el in cols) {
        locs <- colnames(measures)[stringr::str_detect(colnames(measures), paste0(el, "\\b"))] # exact match required
        ls[[el]] <- measures[, locs, with = FALSE, drop = FALSE]
        measures <- measures[, !locs, with = FALSE, drop = FALSE]
      }

      # take element-wise average for every row/column combination across columns to merge
      if (ncol(ls[[1]] >= 2)) {
        all <- abind::abind(ls, along = 3)
        merged <- apply(all, c(1, 2), mean)
      } else merged <- rowSums(abind::abind(ls, along = 2))

      # insert new name at name location of aggregation level (e.g. 'lex1--top1' + 'lex2--top1' = 'name--top1')
      nms <- stringr::str_split(colnames(merged), "--") # list
      loc <- which(stringr::str_detect(nms[[1]], el))[1]
      nmsNew <- lapply(nms, function(x) {
        x[loc] <- name
        return(paste0(x, collapse = "--"))
      })
      colnames(merged) <- unlist(nmsNew)

      # add back merged columns for further merging if needed
      measures <- cbind(measures, merged)
    }
  }

  # add old unmerged measures to merged measures (if do.keep is TRUE)
  if (do.keep) measures <- cbind(measures, measuresOld[, !(namesOld %in% colnames(measures))])

  # update information in sentomeasures object
  sentomeasures <- update_info(sentomeasures, measures)

  return(sentomeasures)
}

#' Setup control for aggregation into sentiment measures
#'
#' @description Sets up control for aggregation of document-level textual sentiment into textual
#' sentiment measures (indices).
#'
#' @details For currently available options on how aggregation can occer (via the \code{howWithin},
#' \code{howDocs} and \code{howTime} parameters), call \code{get_hows()}.
#'
#' @param howWithin a single \code{character} vector defining how aggregation within documents will be performed.
#' @param howDocs a single \code{character} vector defining how aggregation across documents per date will be performed.
#' @param howTime a single \code{character} vector defining how aggregation across dates will be performed.
#' @param do.ignoreZeros a \code{logical} indicating whether zero sentiment values have to be ignored while aggregation
#' across documents.
#' @param by a single \code{character} vector, either \code{"day", "week", "month"} or \code{"year"}, to indicate at what
#' level the dates should be aggregated. Dates will be displayed as the first day of the period, if applicable (e.g.
#' \code{"2017-03-01"} for March 2017).
#' @param lag a single \code{integer} vector, being the time lag to be specified for aggregation across time.
#' @param alphas a \code{numeric} vector of all exponential smoothing factors to calculate weights for, used if
#'  \code{howTime == "exponential"}.
#' @param orders a \code{numeric} vector of all Almon orders to calcalute weights for, used if \code{howTime == "almon"}.
#' @param do.inverse a \code{logical} indicating if for every Almon polynomial its inverse has to be calculated too, used if
#' \code{howTime == "almon"}.
#' @param do.normalize a \code{logical} indicating if every Almon polynomial weights column should sum to one, used if
#' \code{howTime == "almon"}.
#' @param weights an own weighting scheme as a \code{data.frame} with the number of rows equal to the desired (implicit)
#' \code{lag}, used if \code{howTime == "own"}.
#'
#' @return A list encapsulating the control parameters.
#'
#' @seealso \code{\link{get_hows}}
#'
#' @export
ctr_agg <- function(howWithin = "tf-idf", howDocs = "equal-weight", howTime = "equal-weight",
                    do.ignoreZeros = FALSE, by = "day", lag = 1, alphas = seq(0.1, 0.5, by = 0.1),
                    orders = 1:3, do.inverse = TRUE, do.normalize = TRUE, weights = NULL) {

  # get all supported options for each aggregation level
  hows <- get_hows()

  # check if provided aggregations are supported
  warned <- 0
  if (!(howWithin %in% hows[["words"]])) {
    warning(paste0(howWithin, " is no current option for aggregation across words."))
    warned <- warned + 1
  }
  if (!(howDocs %in% hows[["docs"]])) {
    warning(paste0(howDocs, " is no current option for aggregation across docs."))
    warned <- warned + 1
  }
  if (!(howTime %in% hows[["time"]])) {
    warning(paste0(howTime, " is no current option for aggregation across time."))
    warned <- warned + 1
  }
  if (howTime == "own" & is.null(weights)) {
    warning(paste0("Provide a weights data.frame if howTime == 'own'."))
    warned <- warned + 1
  }
  if (howTime != "own" & !is.null(weights)) {
    howTime <- "own"
    warning(paste0("howTime == 'own' is enforced since a valid (not NULL) weights data.frame was supplied."))
  }
  if (lag <= 0) {
    warning("Lag should be greater than zero.")
    warned <- warned + 1
  }
  if (!(by %in% c("year", "month", "week", "day"))) {
    warning(paste0(by, " is no current 'by' option."))
    warned <- warned + 1
  }
  if (warned > 0) stop("Wrong inputs. See warning messages for specifics.")

  other <- list(alphas = alphas, orders = orders, do.inverse = do.inverse, do.normalize = do.normalize, weights = weights)

  ctr <- list(howWithin = howWithin,
              howDocs = howDocs,
              howTime = howTime,
              do.ignoreZeros = do.ignoreZeros,
              by = by,
              lag = lag,
              other = other)

  return(ctr)
}

#' Setup control for merging sentiment measures
#'
#' @description Sets up control for the optional merging (additional aggregation) of sentiment measures.
#'
#' @param sentomeasures a \code{sentomeasures} object.
#' @param lex a list with unique lexicons to merge at given name, e.g. \code{list(lex12 = c("lex1", "lex2"))}.
#' @param feat a list with unique features to merge at given name, e.g. \code{list(feat12 = c("feat1", "feat2"))}.
#' @param time a list with unique time weighting schemes to merge at given name, e.g. \code{list(tw12 = c("tw1", "tw2"))}.
#' @param do.keep a \code{logical} indicating if the original sentiment measures should be kept (i.e. the merged sentiment
#' measures will be added to the current sentiment measures as additional indices if \code{TRUE}).
#'
#' @return A list encapsulating the control parameters.
#'
#' @export
ctr_merge <- function(sentomeasures, lex = NA, feat = NA, time = NA, do.keep = FALSE) {

  check_class(sentomeasures, "sentomeasures")

  missings <- c()
  tooFew <- c()

  # check if columns to merge exist (missings) and if all merges have at least two columns to combine and are unique (tooFew)
  if (all(!is.na(lex))) {
    missings <- c(missings, unlist(lex)[!(unlist(lex) %in% sentomeasures$lexicons)])
    for (i in seq_along(lex)) {
      if (length(lex[[i]]) <= 1 | length(unique(lex[[i]])) != length(lex[[i]]))
        tooFew <- c(tooFew, names(lex)[i])
    }
  }
  if (all(!is.na(feat))) {
    missings <- c(missings, unlist(feat)[!(unlist(feat) %in% sentomeasures$features)])
    for (i in seq_along(feat)) {
      if (length(feat[[i]]) <= 1 | length(unique(feat[[i]])) != length(feat[[i]]))
        tooFew <- c(tooFew, names(feat)[i])
    }
  }
  if (all(!is.na(time))) {
    missings <- c(missings, unlist(time)[!(unlist(time) %in% sentomeasures$time)])
    for (i in seq_along(time)) {
      if (length(time[[i]]) <= 1 | length(unique(time[[i]])) != length(time[[i]]))
        tooFew <- c(tooFew, names(time)[i])
    }
  }

  msg1 <- c()
  msg2 <- c()
  if (length(missings) > 0) {
    msg1 <- paste0("Following columns to merge are not found: ",
                   paste0(missings, collapse = ", "), ".")
    warning(msg1)
  }
  if (length(tooFew) > 0) {
    msg2 <- paste0("Following merges have less than two or not all unique columns: ",
                   paste0(tooFew, collapse = ", "), ".")
    warning(msg2)
  }
  if (length(msg1) > 0 | length((msg2) > 0)) stop("Wrong inputs. See warning messages for specifics.")

  ctr <- list(sentomeasures = sentomeasures,
              lex = lex,
              feat = feat,
              time = time,
              do.keep = do.keep)

  return(ctr)
}

#' Select a subset of sentiment measures
#'
#' @description Selects the subset of sentiment measures which include either all of the given selection components or at least
#' one of them.
#'
#' @param sentomeasures a \code{sentomeasures} object.
#' @param toSelect a vector of components (lexicon, time weighting and features) which form the measures selected.
#' @param do.all a \code{logical} indicating if only measures for wich all (\code{TRUE}) or at least one (\code{FALSE}) of the
#' selection components should occur in the subset.
#'
#' @return A modified \code{sentomeasures} object, with only the measures required.
#'
#' @export
select_measures <- function(sentomeasures, toSelect, do.all = TRUE) {

  check_class(sentomeasures, "sentomeasures")

  allOpts <- c(sentomeasures$features, sentomeasures$lexicons, sentomeasures$time)
  valid <- toSelect %in% allOpts
  if (any(!valid)) {
    stop("Following components make up none of the sentiment measures: ", paste0(toSelect[!valid], collapse = ', '))
  }

  measures <- sentomeasures$measures
  namesList <- stringr::str_split(colnames(measures), "--")

  if (do.all) fun <- all
  else fun <- any
  ind <- sapply(namesList, function(x) return(fun(toSelect %in% x)))

  if (!any(ind)) {
    warning("No appropriate combination is found. Input sentomeasures object is returned.")
    return(sentomeasures)
  }
  else ind[1] <- TRUE # include date column

  measuresNew <- measures[, ind, with = FALSE]

  # update information in sentomeasures object
  sentomeasures <- update_info(sentomeasures, measuresNew)

  return(sentomeasures)
}

#' Plot sentiment measures
#'
#' @description Plotting function for all sentiment measures in the provided \code{sentomeasures} object, shown in one plot.
#'
#' @param x a \code{sentomeasures} object.
#' @param ... currently not used.
#'
#' @return A simple ggplot2 plot.
#'
#' @import ggplot2
#' @export
plot.sentomeasures <- function(x, ...) {

  sentomeasures <- x

  # melt sentiment measures for plotting
  measures <- sentomeasures$measures
  measuresMelt <- melt(measures, id.vars = "date")

  # prepare plot object
  plot <- ggplot(data = measuresMelt, aes(x = date, y = value, color = variable)) +
    geom_line() +
    geom_hline(yintercept = 0, size = 0.50, linetype = "dotted") +
    scale_x_date(name = "Date", labels = scales::date_format("%m-%Y")) +
    scale_y_continuous(name = "Sentiment") +
    ggthemes::theme_tufte(ticks = TRUE) +
    theme(legend.title = element_blank(),
          legend.position = "none",
          text = element_text(size = 11),
          axis.text.x = element_text(angle = 0, hjust = 1),
          axis.ticks = element_blank())

  return(plot)
}

