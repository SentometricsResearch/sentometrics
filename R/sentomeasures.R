
#' One-way road towards a sentomeasures object
#'
#' @description Wrapper function which assembles calls to \code{compute_sentiment()} and \code{perform_agg()}, and includes
#' the input \code{sentocorpus} and computed sentiment scores in its output. Serves as the most direct way towards a panel of
#' textual sentiment measures, and a \code{sentomeasures} object.
#'
#' @param sentocorpus a \code{sentocorpus} object.
#' @param lexicons output from a \code{setup_lexicons()} call.
#' @param ctr output from a \code{ctr_agg()} call.
#'
#' @return A \code{sentomeasures} object, which is a list containing:
#' \item{measures}{a \code{data.table} with a \code{date} column and all textual sentiment measures as remaining columns.}
#' \item{features}{a \code{character} vector of the different features.}
#' \item{lexicons}{a \code{character} vector of the different lexicons used.}
#' \item{time}{a \code{character} vector of the different time weighting schemes used.}
#' \item{by}{a single \code{character} vector specifying the time interval of aggregation used.}
#' \item{stats}{a \code{data.frame} with a series of elementary statistics (mean, standard deviation, maximum, minimum, and
#' average correlation with all other measures) for each individual sentiment measure.}
#' \item{sentiment}{a sentiment scores \code{data.table} with a \code{date} and feature--lexicon sentiment scores columns.}
#' \item{howWithin}{a \code{character} vector to remind how sentiment within documents was aggregated.}
#' \item{howDocs}{a \code{character} vector to remind how sentiment across documents was aggregated.}
#'
#' @seealso \code{\link{compute_sentiment}}, \code{\link{perform_agg}}
#'
#' @examples
#' # computation of sentiment measures based on a provided control function
#' data("useconomynews")
#' corpus <- sento_corpus(corpusdf = useconomynews)
#' l <- setup_lexicons(lexicons[c("LM_eng", "HENRY_eng")], valence[["valence_eng"]])
#' ctr <- ctr_agg(howWithin = "tf-idf",
#'                howDocs = "proportional",
#'                howTime = c("equal_weight", "linear", "almon"),
#'                by = "month",
#'                lag = 3,
#'                ordersAlm = 1:3,
#'                do.inverseAlm = TRUE,
#'                do.normalizeAlm = TRUE)
#' sentomeasures <- sento_measures(corpus, l, ctr)
#'
#' @import data.table
#' @export
sento_measures<- function(sentocorpus, lexicons, ctr) {
  check_class(sentocorpus, "sentocorpus")
  toAgg <- compute_sentiment(sentocorpus, lexicons, how = ctr$howWithin)
  sentomeasures <- perform_agg(toAgg, ctr)
  return(sentomeasures)
}

#' Setup lexicons (and valence word list) for use in sentiment analysis
#'
#' @description Structures provided lexicons and potentially integrates valence words. One can also provide (part of) the
#' built-in lexicons from \code{data("lexicons")} or a valence word list from \code{data("valence")} as an argument.
#' Makes use of the \code{as_key()} function from the \pkg{quanteda} package to make the output coherent and check for
#' duplicates.
#'
#' @param lexiconsIn a list of (raw) lexicons, each element being a \code{data.table} or \code{data.frame} with respectively a
#' words column and a polarity score column. The lexicons should be appropriately named for clarity in terms of subsequently
#' obtained sentiment measures. Alternatively, a subset of the already formatted built-in lexicons accessible via
#' \code{lexicons} can be declared too, as part of the same list input. If only (some of) the package built-in lexicons want
#' to be used, ony can simply supply \code{lexicons[c(...)]} as an argument to either \code{sento_measures()} or
#' \code{compute_sentiment()}. However, it is strongly recommended to pass the lexicons (and a valence word list) that want to
#' be used through this function.
#' @param valenceIn a single valence word list as a \code{data.table} or \code{data.frame} with respectively a words column, a
#' type column (1 for negators, 2 for amplifiers/intensifiers, and 3 for deamplifiers/downtoners) and a score column.
#' Suggested scores are -1, 2 and 0.5 respectively, and should be the same within each type. Alternatively, this argument can
#' be one of the already formatted built-in valence word lists accessible via \code{valence}. If \code{NULL}, no valence word
#' list is part of the output.
#' @param do.split a \code{logical} that if \code{TRUE} splits every lexicon into a separate positive polarity and negative
#' polarity lexicon.
#'
#' @return A list with each lexicon as a \code{data.table} list element according to its name, and a list element named
#' \code{valence} that comprises the valence words. Every \code{x} column contains the words, every \code{y} column
#' contains the polarity score, and for the valence word list, \code{t} contains the word type. If a valence word list is
#' provided, all lexicons are expanded by copying the respective lexicon, and changing the words and scores according to the
#' valence word type: "NOT_" is added for negators, "VERY_" is added for amplifiers and "HARDLY_" is added for deamplifiers.
#' Lexicon scores are multiplied by -1, 2 and 0.5 by default, respectively, or the first value of the scores column of the
#' valence word list.
#'
#' @seealso \code{\link[sentimentr]{as_key}}
#'
#' @examples
#' # sets up output list straight from built-in word lists including valence words
#' l1 <- c(lexicons[c("LM_eng", "HENRY_eng")], valence[["eng"]])
#'
#' # using function, including a self-made lexicon, with and without valence shifters
#' lexIn <- c(list(myLexicon = data.frame(w = c("nice", "boring"), s = c(2, -1))),
#'            lexicons[c("GI_eng")])
#' valIn <- valence[["valence_eng"]]
#' l2 <- setup_lexicons(lexIn)
#' l3 <- setup_lexicons(lexIn, valIn)
#' l4 <- setup_lexicons(lexIn, valIn, do.split = TRUE)
#'
#' @export
setup_lexicons <- function(lexiconsIn, valenceIn = NULL, do.split = FALSE) {

  if (!is.list(lexiconsIn))
    stop("The 'lexicons' input should be a list.")
  if (!is.data.frame(valenceIn) & !is.null(valenceIn))
    stop("The 'valence' argument should be a data.table or data.frame if not NULL.")
  if (any(is.na(names(lexiconsIn))))
    stop("At least one lexicon's name is NA. Please provide proper list names.")

  # check for duplicated lexicon names
  if (sum(duplicated(names(lexiconsIn))) > 0) {
    duplics <- unique(names(lexiconsIn[duplicated(names(lexiconsIn))]))
    stop(paste0("Names of lexicons are not unique. Following names occur at least twice: ",
                paste0(duplics, collapse = ", ")))
  }
  lexNames <- names(lexiconsIn)

  # convert to sentimentr format while supressing warnings on removal of duplicated values
  lexicons <- suppressWarnings(lapply(lexiconsIn, sentimentr::as_key, comparison = NULL))
  lexicons <- lapply(lexicons, function(x) {names(x) <- c("x", "y"); return(x)})
  names(lexicons) <- lexNames

  if (!is.null(valenceIn)) {
    names(valenceIn) <- c("x", "t", "y")
    valTypes <- unique(valenceIn$t)
    scores <- c(valenceIn[valenceIn$t == 1, ]$y[1], valenceIn[valenceIn$t == 2, ]$y[1], valenceIn[valenceIn$t == 3, ]$y[1])
    lexicons <- expand_lexicons(lexicons, types = valTypes, scores = )
  }
  # split each lexicon into a positive and a negative polarity words only lexicon
  if (do.split) {
    lexiconsPos <- lapply(lexicons, function(lex) return(lex[lex$y > 0]))
    names(lexiconsPos) <- paste0(names(lexicons), "_POS")
    lexiconsNeg <- lapply(lexicons, function(lex) return(lex[lex$y < 0]))
    names(lexiconsNeg) <- paste0(names(lexicons), "_NEG")
    lexicons <- c(lexiconsPos, lexiconsNeg)
  }
  if (!is.null(valenceIn)) {
    lexicons[["valence"]] <- valenceIn[!duplicated(valenceIn$x), ]
  }

  return(lexicons)
}

.compute_sentiment <- function(sentocorpus, lexicons, how = get_hows()$words) {
  check_class(sentocorpus, "sentocorpus")

  if (length(how) > 1) how <- how[1]

  if ("valence" %in% names(lexicons)) {
    cat("Modify corpus to account for valence words... ")
    quanteda::texts(sentocorpus) <- include_valence(quanteda::texts(sentocorpus), lexicons[["valence"]])
    cat("Done.", "\n")
  }
  lexNames <- names(lexicons)[names(lexicons) != "valence"]
  features <- sentocorpus$features

  # frequency-based document-feature matrix (rows are corpus ids, columns are words)
  dfm <- quanteda::dfm(quanteda::tokenize(sentocorpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE,
                                          remove_separators = TRUE, ngrams = 1), verbose = FALSE)

  allWords <- quanteda::featnames(dfm)
  wCounts <- quanteda::rowSums(dfm, na.rm = TRUE)

  if (how == "counts" | how == "equal_weight") {
    fdm <- quanteda::t(dfm) # feature-document matrix
  } else {
    if (how == "proportional") { # proportional w.r.t. words frequency vs. total words frequency per document
      weights <- quanteda::tf(dfm, scheme = "prop") # weight = (words freq. / total words freq.) per document
    } else if (how == "tf-idf") {
      weights <- quanteda::tfidf(dfm, normalize = TRUE)
    } else stop("Please select an appropriate aggregation 'how'.")
    fdmWeighted <- quanteda::t(weights)
  }

  s <- as.data.table(matrix(0, nrow = quanteda::ndoc(sentocorpus), ncol = length(lexNames)))
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
    } else if (how == "equal_weight") {
      scores <- quanteda::rowSums(quanteda::t(fdm * allScores)) / wCounts
    } else scores <- quanteda::rowSums(quanteda::t(fdmWeighted * allScores))
    # set NA/NaN sentiment to 0 (e.g. if document contains no words) and put scores under appropriate lexicon column
    scores[is.na(scores)] <- 0
    s[, (lexicon) := scores]
  }
  # structure: date - feature1 - ... - word_count - lexicon1 (sentiment) - ...
  s <- as.data.table(cbind(id = quanteda::docnames(sentocorpus), quanteda::docvars(sentocorpus), word_count = wCounts, s))

  # compute feature-sentiment per document for all lexicons and order by date
  sent <- get_features_sentiment(s, features, lexNames)
  sent <- sent[order(date)]

  sentOut <- list(corpus = sentocorpus, # not the same as input corpus if accounted for valence shifters
                  sentiment = sent,
                  features = features,
                  lexicons = lexNames,
                  howWithin = how)

  return(sentOut)
}

#' Computation of document-level sentiment across features and lexicons
#'
#' @description Given a corpus of texts, computes sentiment per document using the bag-of-words approach,
#' based on the lexicons provided and a choice of aggregation across words per document scheme. Relies partly on the
#' \pkg{quanteda} package. The scores computed are net sentiment (sum of positive minus sum of negative scores).
#'
#' @details
#' For a separate calculation of positive (resp. negative) sentiment, one has to provide distinct positive (resp. negative)
#' lexicons. This can be done using the \code{do.split} option in the \code{setup_lexicons()} function, which automatically
#' splits any lexicon into positive and negative polarity. \code{NA}s are converted to 0, under the assumption that this is
#' equivalent to no sentiment.
#'
#' @param sentocorpus a \code{sentocorpus} object.
#' @param lexicons output from a \code{setup_lexicons()} call.
#' @param how a single \code{character} vector defining how aggregation within documents will be performed. For currently
#' available options on how aggregation can occer, see \code{get_hows()$words}.
#'
#' @return A list containing:
#' \item{corpus}{the supplied \code{sentocorpus} object.}
#' \item{sentiment}{a sentiment scores \code{data.table} with a \code{date} and feature--lexicon sentiment scores columns.}
#' \item{features}{a \code{character} vector of the different features.}
#' \item{lexicons}{a \code{character} vector of the different lexicons used.}
#' \item{howWithin}{a \code{character} vector to remind how sentiment within documents was aggregated.}
#'
#' @examples
#' # sentiment computation based on raw frequency counts
#' data("useconomynews")
#' corpus <- sento_corpus(corpusdf = useconomynews)
#' l <- setup_lexicons(lexicons[c("LM_eng", "HENRY_eng")], valence[["valence_eng"]])
#' sent <- compute_sentiment(corpus, l, how = "counts")
#'
#' @export
compute_sentiment <- compiler::cmpfun(.compute_sentiment)

.get_features_sentiment <- function(sent, features, lexNames) {
  for (lexicon in lexNames) { # multiply lexicons with features to obtain feature-sentiment scores per lexicon
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
#' \code{data.table} with dates and feature--lexicon sentiment scores columns.
#' @param ctr output from a \code{ctr_agg()} call.
#'
#' @return A \code{sentomeasures} object.
#'
#' @seealso \code{\link{compute_sentiment}}, \code{\link{ctr_agg}}
#'
#' @examples
#' # computation of sentiment and aggregation into sentiment measures
#' data("useconomynews")
#' corpus <- sento_corpus(corpusdf = useconomynews)
#' l <- setup_lexicons(lexicons[c("LM_eng", "HENRY_eng")], valence[["valence_eng"]])
#' sent <- compute_sentiment(corpus, l, how = "counts")
#' ctr <- ctr_agg(howTime = c("linear"), by = "year", lag = 3)
#' sentomeasures <- perform_agg(sent, ctr)
#'
#' @export
perform_agg <- function(toAgg, ctr) {
  howDocs <- ctr$howDocs
  howTime <- ctr$howTime
  do.ignoreZeros <- ctr$do.ignoreZeros
  by <- ctr$by
  lag <- ctr$lag
  otherVars <- ctr$other # list or empty
  aggDocs <- agg_documents(toAgg, by = by, how = howDocs, do.ignoreZeros = do.ignoreZeros)
  sentomeasures <- agg_time(aggDocs, lag = lag, how = howTime, otherVars)
  if (!("sentomeasures" %in% class(sentomeasures))) class(sentomeasures) <- c("sentomeasures")
  return(sentomeasures)
}

agg_documents <- function(toAgg, by, how = get_hows()$docs, do.ignoreZeros = FALSE) {

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
  if (do.ignoreZeros)
    sent[, names(sent)] <- sent[, names(sent), with = FALSE][, lapply(.SD, function(x) replace(x, which(x == 0), NA))]

  # aggregate feature-sentiment per document by date for all lexicon columns
  s <- sent[, -1]
  if (how == "equal_weight") {
    attribWeights[["W"]] <- data.table(id = sent$id, s[, w := 1 / .N, by = date][, "w"])
    s[, w := NULL]
    measures <- s[, lapply(.SD, mean, na.rm = TRUE), by = date]
  } else if (how == "proportional") { # proportional w.r.t. words in document vs. total words in all documents per date
    attribWeights[["W"]] <- data.table(id = sent$id, s[, list(w = word_count / sum(word_count, na.rm = TRUE)), by = date])
    measures <- s[, lapply(.SD, function(x) sum(x * word_count / sum(word_count, na.rm = TRUE), na.rm = TRUE)), by = date]
  }
  measures$word_count <- NULL

  sentomeasures <- list(measures = measures,
                        features = features,
                        lexicons = lexNames,
                        time = NA,
                        by = by,
                        stats = NA,
                        sentiment = sent,
                        howWithin = toAgg$howWithin,
                        howDocs = how,
                        attribWeights = attribWeights)

  class(sentomeasures) <- c("sentomeasures")

  return(sentomeasures)
}

agg_time <- function(sentomeasures, lag, how = get_hows()$time, ...) {
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
  measures <- sentomeasures$measures
  toRoll <- measures[, -1]
  n <- nrow(weights)
  m <- nrow(measures)
  if (n > m)
    stop("Rolling time aggregation window (= ", n, ") is too large for number of observations per measure (= ", m, ")")

  for (i in 1:ncol(weights)) {
    name <- colnames(weights)[i]
    add <- RcppRoll::roll_sum(as.matrix(toRoll), n = n, weights = as.vector(weights[, i]), normalize = FALSE, align = "right")
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
  sentomeasures$stats <- compute_stats(sentomeasures)

  return(sentomeasures)
}

#' Merge sentiment measures
#'
#' @description Merge (further aggregate) measures by combining across the lexicons, features and time weighting schemes
#' dimensions. The combination occurs by taking the mean of the relevant measures.
#'
#' @param ctr output from a \code{ctr_merge()} call.
#'
#' @return A modified \code{sentomeasures} object, with only the sentiment measures required, including updated information
#' and statistics, but the original sentiment scores \code{data.table} untouched.
#'
#' @seealso \code{\link{ctr_merge}}
#'
#' @examples
#' # construct a sentomeasures object to start with
#' data("useconomynews")
#' corpus <- sento_corpus(corpusdf = useconomynews)
#' l <- setup_lexicons(lexicons[c("LM_eng", "HENRY_eng")], valence[["valence_eng"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpus, l, ctr)
#'
#' # set up control function and perform the merging
#' ctrMerge <- ctr_merge(sentomeasures,
#'                       time = list(W = c("equal_weight", "linear")),
#'                       feat = list(journals = c("wsj", "wapo")),
#'                       do.keep = TRUE)
#' sentomeasuresMerged <- merge_measures(ctrMerge)
#'
#' @export
merge_measures <- function(ctr) {

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
        merged <- apply(all, c(1, 2), mean)
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
  if (do.keep) measures <- cbind(measures, measuresOld[, !(namesOld %in% colnames(measures))])

  sentomeasures <- update_info(sentomeasures, measures) # update information in sentomeasures object

  return(sentomeasures)
}

#' Merge sentiment measures into one global sentiment measure
#'
#' @description Merges all sentiment measures into one global textual sentiment measure based on a set of weights provided to
#' indicate the importance of each component in the \code{lexicons}, \code{features} and \code{time} vectors as part of the
#' \code{sentomeasures} object. The global measure is composed as the multiplication of the individual weights across the
#' three dimensions times the sentiment value per date observation.
#'
#' @param sentomeasures a \code{sentomeasures} object.
#' @param lex a \code{numeric} vector of weights, of size \code{length(sentomeasures$lexicons)}, in the same order and
#' summing to one. By default set to 1, which means equally weighted.
#' @param feat a \code{numeric} vector of weights, of size \code{length(sentomeasures$features)}, in the same order and
#' summing to one. By default set to 1, which means equally weighted.
#' @param time a \code{numeric} vector of weights, of size \code{length(sentomeasures$time)}, in the same order and summing
#' to one. By default set to 1, which means equally weighted.
#'
#' @return A modified \code{sentomeasures} object, with only the global sentiment measure, including updated statistics, but
#' the other list elements and the original sentiment scores \code{data.table} untouched.
#'
#' @seealso \code{\link{ctr_merge}}
#'
#' @examples
#' # construct a sentomeasures object to start with
#' data("useconomynews")
#' corpus <- sento_corpus(corpusdf = useconomynews)
#' l <- setup_lexicons(lexicons[c("LM_eng", "HENRY_eng")], valence[["valence_eng"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpus, l, ctr)
#'
#' # merge into one global sentiment measure, with specified weighting for lexicons and features
#' global <- merge_to_global(sentomeasures, lex = c(0.40, 0.60),
#'                                          feat = c(0.10, 0.20, 0.30, 0.40),
#'                                          time = 1)
#'
#' @export
merge_to_global <- function(sentomeasures, lex = 1, feat = 1, time = 1) {
  check_class(sentomeasures, "sentomeasures")

  lexicons <- sentomeasures$lexicons
  nL <- length(lexicons)
  features <- sentomeasures$features
  nF <- length(features)
  times <- sentomeasures$time
  nT <- length(times)

  # modify weight vectors if equal to default value of 1
  if (length(lex) == 1)
    if (lex == 1) lex <- rep(1/nL, nL)
  if (length(feat) == 1)
    if (feat == 1) feat <- rep(1/nF, nF)
  if (length(time) == 1)
    if (time == 1) time <- rep(1/nT, nT)

  # check appropriateness of weight vectors
  if (sum(lex) != 1 | length(lex) != nL | sum(feat) != 1 | length(feat) != nF | sum(time) != 1 | length(time) != nT)
    stop("Numeric weights must be equal in length to the respective number of components and sum to one.")

  measures <- sentomeasures$measures
  measuresLong <- to_long(measures) # long format

  # make named weight lists
  lexList <- as.list(lex)
  names(lexList) <- lexicons
  featList <- as.list(feat)
  names(featList) <- features
  timeList <- as.list(time)
  names(timeList) <- times

  # extract different weights based on how measuresLong is ordered and add a global weights (w) column
  wLex <- unlist(lexList[measuresLong[["lexicon"]]])
  wFeat <- unlist(featList[measuresLong[["feature"]]])
  wTime <- unlist(timeList[measuresLong[["time"]]])

  # add a global weights column as the multiplication of the individual weights across the three dimensions per row
  measuresLong[, "w" := wLex * wFeat * wTime]
  global <- measuresLong[, list(global = sum(value * w)), by = date]

  sentomeasures$measures <- global
  sentomeasures$stats <- compute_stats(sentomeasures)

  return(sentomeasures)
}

#' Setup control for aggregation into sentiment measures
#'
#' @description Sets up control object for aggregation of document-level textual sentiment into textual
#' sentiment measures (indices).
#'
#' @details For currently available options on how aggregation can occer (via the \code{howWithin},
#' \code{howDocs} and \code{howTime} parameters), call \code{get_hows()}.
#'
#' @param howWithin a single \code{character} vector defining how aggregation within documents will be performed. Should
#' \code{length(howWithin) > 1}, the first element is used.
#' @param howDocs a single \code{character} vector defining how aggregation across documents per date will be performed.
#' Should \code{length(howDocs) > 1}, the first element is used.
#' @param howTime a \code{character} vector defining how aggregation across dates will be performed. More than one choice
#' is possible here.
#' @param do.ignoreZeros a \code{logical} indicating whether zero sentiment values have to be ignored while aggregation
#' across documents.
#' @param by a single \code{character} vector, either \code{"day", "week", "month"} or \code{"year"}, to indicate at what
#' level the dates should be aggregated. Dates will be displayed as the first day of the period, if applicable (e.g.
#' \code{"2017-03-01"} for March 2017).
#' @param lag a single \code{integer} vector, being the time lag to be specified for aggregation across time. By default
#' equal to 1, meaning no aggregation across time.
#' @param alphasExp a \code{numeric} vector of all exponential smoothing factors to calculate weights for, used if
#'  \code{"exponential" \%in\% howTime}. Values should be betwoon 0 and 1 (both excluded).
#' @param ordersAlm a \code{numeric} vector of all Almon polynomial orders to calcalute weights for, used if
#' \code{"almon" \%in\% howTime}.
#' @param do.inverseAlm a \code{logical} indicating if for every Almon polynomial its inverse has to be calculated too, used
#' if \code{"almon" \%in\% howTime}.
#' @param do.normalizeAlm a \code{logical} indicating if every Almon polynomial weights column should sum to one, used if
#' \code{"almon" \%in\% howTime}.
#' @param weights an own weighting scheme as a \code{data.frame} with the number of rows equal to the desired \code{lag}, used
#' if \code{"own" \%in\% howTime}.
#'
#' @return A list encapsulating the control parameters.
#'
#' @seealso \code{\link{get_hows}}
#'
#' @examples
#' # simple control function (using equal_weight default options)
#' ctr1 <- ctr_agg(howTime = c("linear"), by = "year", lag = 3)
#'
#' # more elaborate control function (particular attention to time weighting schemes)
#' ctr2 <- ctr_agg(howWithin = "tf-idf",
#'                 howDocs = "proportional",
#'                 howTime = c("equal_weight", "linear", "almon", "exponential", "own"),
#'                 do.ignoreZeros = TRUE,
#'                 by = "day",
#'                 lag = 20,
#'                 ordersAlm = 1:3,
#'                 do.inverseAlm = TRUE,
#'                 do.normalizeAlm = TRUE,
#'                 alphasExp = c(0.20, 0.50, 0.70, 0.95),
#'                 weights = data.frame(myWeights = runif(20)))
#'
#' @export
ctr_agg <- function(howWithin = "equal_weight", howDocs = "equal_weight", howTime = "equal_weight",
                    do.ignoreZeros = FALSE, by = "day", lag = 1, alphasExp = seq(0.1, 0.5, by = 0.1),
                    ordersAlm = 1:3, do.inverseAlm = TRUE, do.normalizeAlm = TRUE, weights = NULL) {

  if (length(howWithin) > 1) howWithin <- howWithin[1]
  if (length(howDocs) > 1) howDocs <- howDocs[1]

  # check if provided aggregation specifications are supported
  hows <- get_hows() # get all supported options for each aggregation level
  warned <- 0
  if (!(howWithin %in% hows[["words"]])) {
    warning(paste0(howWithin, " is no current option for aggregation across words."))
    warned <- warned + 1
  }
  if (!(howDocs %in% hows[["docs"]])) {
    warning(paste0(howDocs, " is no current option for aggregation across docs."))
    warned <- warned + 1
  }
  if (!all(howTime %in% hows[["time"]])) {
    warning(paste0(howTime[!(howTime %in% hows[["time"]])], " is no current option for aggregation across time. "))
    warned <- warned + 1
  }
  if ("own" %in% howTime & is.null(weights)) {
    warning(paste0("Provide a 'weights' data.frame if 'own' provided as an option in 'howTime'."))
    warned <- warned + 1
  }
  if (!("own" %in% howTime) & !is.null(weights)) {
    howTime <- c(howTime, "own")
    warning(paste0("The option 'own' is added to 'howTime' since a valid (not NULL) 'weights' data.frame was supplied."))
  }
  if ("own" %in% howTime) {
    if (lag != nrow(weights)) {
      lag <- nrow(weights)
      warning("Argument 'lag' is set equal to the number of rows in 'weights' data.frame.")
    }
  }
  if (max(alphasExp) >= 1 & min(alphasExp) <= 0) {
    warning("Values in 'alphasExp' should be between 0 and 1 (both excluded).")
    warned <- warned + 1
  }
  if (lag <= 0) {
    warning("Argument 'lag' should be greater than zero.")
    warned <- warned + 1
  }
  if (!(by %in% c("year", "month", "week", "day"))) {
    warning(paste0(by, " is no current 'by' option."))
    warned <- warned + 1
  }
  if (warned > 0) stop("Wrong inputs. See warning messages for specifics.")

  other <- list(alphasExp = alphasExp, ordersAlm = ordersAlm, do.inverseAlm = do.inverseAlm,
                do.normalizeAlm = do.normalizeAlm, weights = weights)

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
#' @description Sets up control object for the optional merging (additional aggregation) of sentiment measures.
#'
#' @param sentomeasures a \code{sentomeasures} object.
#' @param lex a list with unique lexicons to merge at given name, e.g. \cr
#' \code{list(lex12 = c("lex1", "lex2"))}. See \code{sentomeasures$lexicons} for the exact names to use.
#' @param feat a list with unique features to merge at given name, e.g. \cr
#' \code{list(feat12 = c("feat1", "feat2"))}. See \code{sentomeasures$features} for the exact names to use.
#' @param time a list with unique time weighting schemes to merge at given name, e.g. \cr
#' \code{list(tw12 = c("tw1", "tw2"))}. See \code{sentomeasures$time} for the exact names to use.
#' @param do.keep a \code{logical} indicating if the original sentiment measures should be kept (i.e. the merged sentiment
#' measures will be added to the current sentiment measures as additional indices if \code{TRUE}).
#'
#' @return A list encapsulating the control parameters.
#'
#' @examples
#' # construct a sentomeasures object to start with
#' data("useconomynews")
#' corpus <- sento_corpus(corpusdf = useconomynews)
#' l <- setup_lexicons(lexicons[c("LM_eng", "HENRY_eng")], valence[["valence_eng"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpus, l, ctr)
#'
#' # set up a correct control function
#' ctrMerge <- ctr_merge(sentomeasures,
#'                       time = list(W = c("equal_weight", "linear")),
#'                       lex = list(LEX = c("LM_eng", "HENRY_eng")),
#'                       feat = list(journals = c("wsj", "wapo")),
#'                       do.keep = TRUE)
#'
#' \dontrun{
#' # produces an informative error message
#' ctrMerge <- ctr_merge(sentomeasures,
#'                       time = list(W = c("equal_weight", "almon1")),
#'                       lex = list(LEX = c("LM_eng", "HENRY_eng")),
#'                       feat = list(journals = c("notInHere", "wapo")))
#' }
#'
#' @export
ctr_merge <- function(sentomeasures, feat = NA, lex = NA, time = NA, do.keep = FALSE) {
  check_class(sentomeasures, "sentomeasures")

  # check if columns to merge exist (missings) and if all merges have at least two columns to combine and are unique (tooFew)
  missings <- c()
  tooFew <- c()
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

  # assemble warning messages if any
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
#' @description Selects the subset of sentiment measures which include either all of the given selection components combined,
#' or those who's name consist of at least one of the selection components.
#'
#' @param sentomeasures a \code{sentomeasures} object.
#' @param toSelect a vector of components (lexicon, time weighting and feature names) which form the measures selected.
#' @param do.combine a \code{logical} indicating if only measures for wich all (\code{TRUE}) or at least one (\code{FALSE}) of
#' the selection components should occur in each sentiment measure's name in the subset.
#'
#' @return A modified \code{sentomeasures} object, with only the sentiment measures required, including updated information
#' and statistics, but the original sentiment scores \code{data.table} untouched.
#'
#' @examples
#' # construct a sentomeasures object to start with
#' data("useconomynews")
#' corpus <- sento_corpus(corpusdf = useconomynews)
#' l <- setup_lexicons(lexicons[c("LM_eng", "HENRY_eng")], valence[["valence_eng"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpus, l, ctr)
#'
#' # different selections
#' sel1 <- select_measures(sentomeasures, c("equal_weight"))
#' sel2 <- select_measures(sentomeasures, c("equal_weight", "linear"), do.combine = FALSE)
#' sel3 <- select_measures(sentomeasures, c("linear", "LM_eng"))
#' sel4 <- select_measures(sentomeasures, c("linear", "LM_eng", "wsj", "economy"),
#'                         do.combine = FALSE)
#'
#' @export
select_measures <- function(sentomeasures, toSelect, do.combine = TRUE) {
  check_class(sentomeasures, "sentomeasures")

  allOpts <- c(sentomeasures$features, sentomeasures$lexicons, sentomeasures$time)
  valid <- toSelect %in% allOpts
  if (any(!valid)) {
    stop("Following components make up none of the sentiment measures: ", paste0(toSelect[!valid], collapse = ', '))
  }

  measures <- sentomeasures$measures
  namesList <- stringi::stri_split(colnames(measures), regex = "--")
  if (do.combine) fun <- all
  else fun <- any
  ind <- sapply(namesList, function(x) return(fun(toSelect %in% x)))
  if (!any(ind)) {
    warning("No appropriate combination is found. Input sentomeasures object is returned.")
    return(sentomeasures)
  } else ind[1] <- TRUE # include date column
  measuresNew <- measures[, ind, with = FALSE]

  sentomeasures <- update_info(sentomeasures, measuresNew) # update information in sentomeasures object

  return(sentomeasures)
}

#' Plot sentiment measures
#'
#' @method plot sentomeasures
#'
#' @description Straightforward plotting method for all sentiment measures in the provided \code{sentomeasures} object,
#' shown in one plot. We suggest to make use of the \code{select_measures()} function when you desire to plot only a subset of
#' the sentiment measures.
#'
#' @param x a \code{sentomeasures} object.
#' @param group a value from \code{c("lexicon", "feature", "time", "all")}. The first three choices display
#' all measures from the same group in the same color. The choice \code{"all"} displays every single sentiment
#' measure in a separate color, but this may look visually overwhelming very fast.
#' @param ... not used.
#'
#' @return Returns a simple \pkg{ggplot2} plot, which can be added onto (or to alter its default elements) by using the
#' \code{+} operator (see examples).
#'
#' @examples
#' # construct a sentomeasures object to start with
#' data("useconomynews")
#' corpus <- sento_corpus(corpusdf = useconomynews)
#' l <- setup_lexicons(lexicons[c("LM_eng", "HENRY_eng")], valence[["valence_eng"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpus, l, ctr)
#'
#' # plot sentiment measures
#' plot(sentomeasures)
#' plot(sentomeasures, group = "lexicon")
#'
#' # adjust appearance of plot
#' p <- plot(sentomeasures)
#' p <- p + theme_dark() +
#'   scale_x_date(name = "date") +
#'   scale_y_continuous(name = "newName")
#' p
#'
#' @import ggplot2
#' @export
plot.sentomeasures <- function(x, group = "all", ...) {

  if (!(group %in% c("lexicon", "feature", "time", "all")))
    stop("The 'group' argument should be either 'lexicon', 'feature', 'time' or 'all'.")

  # melt sentiment measures for plotting
  sentomeasures <- x
  measures <- sentomeasures$measures
  if (group == "all") {
    measuresMelt <- melt(measures, id.vars = "date")
    group <- "variable"
    legendPos <- "none"
  } else {
    measuresMelt <- to_long(measures)
    legendPos <- "top"
  }

  plot <- ggplot(data = measuresMelt, aes_string(x = "date", y = "value", color = group)) +
    geom_line() +
    geom_hline(yintercept = 0, size = 0.50, linetype = "dotted") +
    scale_x_date(name = "Date", date_labels = "%m-%Y") +
    scale_y_continuous(name = "Sentiment") +
    ggthemes::theme_tufte() +
    theme(legend.title = element_blank(), legend.position = legendPos, text = element_text(size = 11))

  return(plot)
}

#' Add and fill missing dates
#'
#' @description Adds missing dates between earliest and latest date, such that time series is continuous on a period-by-period
#' basis. Fills in these dates with either \code{0} or the respective latest non-missing value.
#'
#' @param sentomeasures a \code{sentomeasures} object.
#' @param fill an element of \code{c("zero", "latest")}, the first assumes missing dates represent zero sentiment, the
#' latter assumes missing dates represent constant sentiment.
#'
#' @return A modified \code{sentomeasures} object.
#'
#' @examples
#' # construct a sentomeasures object to start with
#' data("useconomynews")
#' corpus <- sento_corpus(corpusdf = useconomynews)
#' l <- setup_lexicons(lexicons[c("LM_eng", "HENRY_eng")], valence[["valence_eng"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpus, l, ctr)
#'
#' # fill measures
#' f1 <- fill_measures(sentomeasures)
#' f2 <- fill_measures(sentomeasures, fill = "latest")
#'
#' @export
fill_measures <- function(sentomeasures, fill = "zero") {
  check_class(sentomeasures, "sentomeasures")

  by <- sentomeasures$by
  measures <- sentomeasures$measures
  dates <- measures$date
  ts <- seq(dates[1], dates[length(dates)], by = by)
  dt <- data.table(date = ts)

  # join and fill as provided to new measures
  measuresFill <- merge(dt, measures, by = "date", all = TRUE) # fills with NA
  if (fill == "zero") {
    measuresFill[is.na(measuresFill)] <- 0
  } else if (fill == "latest") {
    measuresFill <- zoo::na.locf(measuresFill)
  } else stop("Input variable 'fill' should be either 'zero' or 'latest'.")
  measuresFill <- data.table(date = ts, measuresFill[, lapply(.SD, as.numeric), .SDcols = colnames(measures)[-1]])

  sentomeasures$measures <- measuresFill

  return(sentomeasures)
}

#' Scaling and centering of sentiment measures
#'
#' @description Scales and centers the sentiment measures from a \code{sentomeasures} object, column-per-column. By default,
#' the measures are normalized. \code{NA}s are removed first.
#'
#' @param x a \code{sentomeasures} object.
#' @param center a \code{logical}, see documentation for the generic \code{\link{scale}}.
#' @param scale a \code{logical}, see documentation for the generic \code{\link{scale}}.
#'
#' @return A modified \code{sentomeasures} object, with the measures replaced by the scaled measures as well as updated
#' statistics.
#'
#' @examples
#' # construct a sentomeasures object to start with
#' data("useconomynews")
#' corpus <- sento_corpus(corpusdf = useconomynews)
#' l <- setup_lexicons(lexicons[c("LM_eng", "HENRY_eng")], valence[["valence_eng"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpus, l, ctr)
#'
#' # scale sentimeant measures
#' scaled <- scale(sentomeasures)
#'
#' @export
scale.sentomeasures <- function(x, center = TRUE, scale = TRUE) {
  sentomeasures <- x
  dates <- sentomeasures$measures[, 1]
  measures <- sentomeasures$measures[, -1] # drop date column
  measuresNorm <- scale(measures, center, scale)
  sentomeasures$measures <- data.table(dates, measuresNorm)
  sentomeasures$stats <- compute_stats(sentomeasures)
  return(sentomeasures)
}

