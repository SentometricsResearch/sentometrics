
### TO DO: aggregation at different frequencies (daily, weekly, monthly)

#' One-way towards sentmeasures object
#'
#' @description Wrapper function which assembles calls to \code{compute_sentiment()} and \code{perform_agg()}, and includes the input
#' \code{corpuS} and computed sentiment scores in its output. Serves as the most direct way towards a panel of textual sentiment measures,
#' and a \code{sentmeasures} object.
#'
#' @return A \code{sentmeasures} object.
#'
#' @seealso \code{\link{compute_sentiment}}, \code{\link{perform_agg}}
#'
#' @export
sentmeasures <- function(corpus, lexicons, ctrlAgg) {

  if (!("corpuS" %in% class(corpus))) stop("Provide a corpuS object.")

  toAgg <- compute_sentiment(corpus, lexicons, ctrlAgg$howWithin)
  sentmeasures <- perform_agg(toAgg, ctrlAgg)

  return(sentmeasures)
}

#' Setup lexicons format
#'
#' @description Structures provided lexicons and potentially valence words. Makes use of the \code{as_key()} function from the
#' \pkg{quanteda} package.
#'
#' @param lexiconsRaw a list of lexicons, with each element being a \code{data.frame} with a words column and a polarity
#' score columns. The lexicons should be appropriately named for clarity in terms of subsequently obtained sentiment measures.
#' Alternatively, this argument can be a character vector specifying which built-in lexicons to be used.
#' @param valenceWords a \code{data.frame} with a words column and a type indicating 1 (positive) or -1 (negative).
#' Alternatively, this argument can be a character vector specifying which built-in valence words to use.
#' @param split a logical that if \code{TRUE} splits every lexicon into a separate positive polarity and negative polarity lexicon.
#'
#' @return a list with each lexicon as an element according to its name, and the element \code{valenceWords} that comprises
#' the valence words.
#'
#' @seealso \code{\link[quanteda]{as_key}}
#'
#' @export
setup_lexicons <- function(lexiconsRaw, valenceWords, split = FALSE) {

  ### format of lexicons_raw and valenceWords to define
  ### add legion of built-in lexicons (+ add possibility for lexiconsRaw to be a character vector + valenceWords argument and input)

  # check for duplicated lexicon names
  if (sum(duplicated(names(lexiconsRaw))) > 0) {
    duplics <- unique(names(lexiconsRaw[duplicated(names(lexiconsRaw))]))
    stop(paste0("Names of lexicons are not unique. Following names occur at least twice: ",
                paste0(duplics, collapse = ", ")))
  }

  # convert to sentimentr lexicons and valence shifters format
  lexicons <- suppressWarnings(lapply(lexiconsRaw, sentimentr::as_key))
  names(lexicons) <- names(lexiconsRaw)

  val <- suppressWarnings(sentimentr::as_key(valenceWords))

  # discard valence words that also occur in at least one of the lexicons
  sames <- c()
  for(lex in lexicons) {
      same <- which(val$x %in% lex$x) # x are the words (sentimentr format)
      sames <- c(sames, same)
  }

  # split each lexicon into a positive and a negative polarity words only lexicon
  if (split) {
    lexiconsPos <- lapply(lexicons, function(lex) return(lex[lex$y > 0]))
    names(lexiconsPos) <- paste0(names(lexicons), "_POS")

    lexiconsNeg <- lapply(lexicons, function(lex) return(lex[lex$y < 0]))
    names(lexiconsNeg) <- paste0(names(lexicons), "_NEG")

    lexicons <- c(lexiconsPos, lexiconsNeg)
  }

  lexicons[["valenceWords"]] <- val[!unique(sames), ]

  return(lexicons)
}

.compute_sentiment <- function(corpuS, lexicons, how = get_hows()$words) {

  if (length(how) > 1) how <- how[1]

  lexNames <- names(lexicons)[names(lexicons) != "valenceWords"]
  features <- corpuS$features

  # frequency-based document-feature matrix (rows are corpus ids, columns are words)
  dfm <- quanteda::dfm(quanteda::tokenize(corpuS,
                                          remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                                          ngrams = 1))

  ### add remove = stopwords("english") in dfm... (or other specified stopwords)?
  ### valence words: ngrams (1:n), negation words in text ==> not_, pos lexicon = pos + not_ neg, one-word lexicons ideally

  allWords <- quanteda::featnames(dfm)

  wCounts <- rowSums(dfm, na.rm = TRUE)

  if (how == "counts" | how == "equal-weight") {
    fdm <- t(dfm) # feature-document matrix
  } else {
    if (how == "proportional") { # proportional w.r.t. words frequency vs. total words frequency per document
      weights <- quanteda::tf(dfm, scheme = "prop") # weight = (words freq. / total words freq.) per document
    } else if (how == "tf-idf") {
      weights <- quanteda::tfidf(dfm, normalize = TRUE)
    } else stop("Please select an appropriate aggregation 'how'.")

    fdmWeighted <- t(weights)
  }

  s <- data.table::as.data.table(matrix(0, nrow = quanteda::ndoc(corpuS), ncol = length(lexNames)))
  names(s) <- lexNames

  for (lexicon in lexNames) {
    lexWords <- lexicons[[lexicon]]$x
    lexScores <- lexicons[[lexicon]]$y

    ### no need to distinguish between + and - lexicons, as polarity is either +1 or -1, so rowSums() gives net sentiment
    ### for separate calculations of + and -, users have to provide distinct lexicons (split option in setup_lexicons())

    # locate polarized words and set weights to their polarity or keep at zero
    allScores <- rep(0, length(allWords))
    polInd <- allWords %in% lexWords
    polWords <- allWords[allWords %in% lexWords]
    allScores[polInd] <- lexScores[lexWords %in% polWords]
    names(allScores) <- allWords

    # scores per document equal to (frequency * weight * polarity score)
    if (how == "counts") {
      scores <- rowSums(t(fdm * allScores))
    } else if (how == "equal-weight") {
      scores <- rowSums(t(fdm * allScores)) / wCounts
    } else scores <- rowSums(t(fdmWeighted * allScores))

    # put scores under appropriate lexicon column
    s[, (lexicon) := scores]
  }

  # structure: id - date - feature1 - ... - word_count - lexicon1 (sentiment) - ...
  s <- data.table::as.data.table(cbind(quanteda::docvars(corpuS), word_count = wCounts, s))

  # compute feature-sentiment per document for all lexicons
  sent <- get_features_sentiment(s, features, lexNames)

  sentOut <- list(corpuS = corpuS,
                  sentiment = sent,
                  features = features,
                  lexicons = lexNames)

  return(sentOut)
}

#' Computation of document-level sentiment across features and lexicons
#'
#' @description Given a corpus of texts, computes sentiment per document starting from the bag-of-words approach,
#' based on the lexicons provided and a preferred aggregation across words per document scheme. Relies partly on
#' the \pkg{quanteda} package.
#'
#' @param corpuS a \code{corpuS} object.
#' @param lexicons output from a \code{setup_lexicons()} call.
#' @param how a single character vector defining how aggregation within documents will be performed.
#' For currently available options on how aggregation can occer, access \code{get_hows()$words}.
#'
#' @return A list containing:
#' \item{corpuS}{the supplied \code{corpuS} object.}
#' \item{sent}{a sentiment scores \code{data.frame} with document ids, dates and features--lexicons sentiment scores columns.}
#' \item{features}{a character vector of the different features.}
#' \item{lexicons}{a character vector of the different lexicons used.}
#'
#' @export
compute_sentiment <- compiler::cmpfun(.compute_sentiment)

#' Aggregate textual sentiment across documents and time
#'
#' @description Condense document-level textual sentiment scores into a panel of textual sentiment
#' measures by aggregating across documents and time.
#'
#' @param toAgg output from a \code{compute_sentiment()} call, a list with as main component a sentiment scores
#' \code{data.frame} with document ids, dates and features--lexicons sentiment scores columns.
#' @param ctrlAgg output from a \code{ctrl_agg()} call.
#'
#' @return A list containing:
#' \item{measures}{a \code{sentmeasures} object.}
#' \item{features}{a character vector of the different features.}
#' \item{lexicons}{a character vector of the different lexicons used.}
#' \item{time}{a character vector of the different time weighting schemes used.}
#'
#' @seealso \code{\link{compute_sentiment}}, \code{\link{ctrl_agg}}
#'
#' @export
perform_agg <- function(toAgg, ctrlAgg) {

  howDocs <- ctrlAgg$howDocs
  howTime <- ctrlAgg$howTime
  ignoreZeros <- ctrlAgg$ignoreZeros
  lag <- ctrlAgg$lag
  otherVars <- ctrlAgg$other # list

  aggDocs <- agg_documents(toAgg, howDocs, ignoreZeros)
  sentmeasures <- agg_time(aggDocs, lag, howTime, otherVars)

  if (!("sentmeasures" %in% class(sentmeasures))) class(sentmeasures) <- c("sentmeasures")

  return(sentmeasures)
}

get_features_sentiment <- function(sent, features, lexNames) {

  # multiply lexicons with features to obtain feature-sentiment scores per lexicon
  for (lexicon in lexNames) {
    for (feature in features) {
      name <- paste0(lexicon, "--", feature)
      sent[, name] <- sent[, lexicon, with = FALSE] * sent[, feature, with = FALSE]
    }
  }
  sent <- data.table::as.data.table(sent)
  sent[, eval(c(lexNames, features)) := NULL] # remove since replaced by lexicon--feature columns

  return(sent)
}

agg_documents <- function(toAgg, how = get_hows()$docs, ignoreZeros = FALSE) {

  features <- toAgg$features
  lexNames <- toAgg$lexicons
  sent <- toAgg$sentiment

  # ignore documents with zero sentiment in aggregation (if ignoreZeros is TRUE)
  if (ignoreZeros) sent[, names(sent)] <-
    sent[, names(sent), with = FALSE][, lapply(.SD, function(x) replace(x, which(x == 0), NA))]

  # aggregate feature-sentiment per document by date for all lexicon columns
  if (how == "equal-weight") {
    measures <- sent[, lapply(.SD, mean, na.rm = TRUE),
                     by = date][order(date)]
  } else if (how == "proportional") { # proportional w.r.t. words in document vs. total words in all documents per date
    measures <- sent[, lapply(.SD, sum(.SD * word_count / sum(word_count, na.rm = TRUE), na.rm = TRUE), na.rm = TRUE),
                     by = date][order(date)]
  }
  measures$word_count <- NULL

  sentmeasures <- list(measures = measures,
                       features = features,
                       lexicons = lexNames,
                       time = NA,
                       sentiment = sent)

  class(sentmeasures) <- c("sentmeasures")

  return(sentmeasures)
}

agg_time <- function(sentmeasures, lag, how = get_hows()$time, ...) {

  if (!("sentmeasures" %in% class(sentmeasures))) stop("Please provide a sentmeasures object as first argument.")

  # construct or pass on weights data.frame and check for duplicated names
  if (how == "own") weights <- ...$weights
  else weights <- setup_time_weights(lag, how, ...)

  if (sum(duplicated(colnames(weights))) > 0) {
    duplics <- unique(colnames(weights)[duplicated(colnames(weights))])
    stop(paste0("Names of weighting schemes are not unique. Following names occur at least twice: ",
                paste0(duplics, collapse = ", ")))
  }

  # convert sentiment measures into xts object and order by date
  measures <- sentmeasures$measures
  toRoll <- xts::xts(measures[, 2:ncol(measures)], order.by = measures$date) ### date should be in an appropriate format

  # apply rolling time window for every weights column and combine all new measures column-wise
  n <- nrow(weights)
  for (i in 1:ncol(weights)) {
    w <- weights[, i]
    name <- colnames(weights)[i]

    ### to speed up with RcppRoll
    add <- data.frame(zoo::rollapplyr(toRoll, width = n, by.column = TRUE, roll_weights, w = w, align = "right"))
    colnames(add) <- paste0(colnames(toRoll), "--", name)

    if (i == 1) measuresAggTime <- add
    else measuresAggTime <- cbind(measuresAggTime, add)

  }

  sentmeasures$measures <- measuresAggTime
  sentmeasures$features <- sentmeasures$features
  sentmeasures$lexicons <- sentmeasures$lexicons
  sentmeasures$time <- colnames(weights)

  return(sentmeasures)
}

#' Merge sentiment measures
#'
#' @description Merge (further aggregate) measures by combining across the lexicons, features and time weighting schemes
#' dimensions. The combination occurs by taking the mean of the relevant measures.
#'
#' @param ctrlMerge output from a \code{ctrl_merge()} call.
#'
#' @return A modified \code{sentmeasures} object.
#'
#' @seealso \code{\link{ctrl_merge}}
#'
#' @export
merge_measures <- function(ctrlMerge) {

  ### melt from data.table useful?
  ### option for computation of "global" sentiment measure

  sentmeasures <- ctrlMerge$sentmeasures
  measures <- sentmeasures$measures
  toMerge <- ctrlMerge[2:4]
  keep <- ctrlMerge$keep

  if (keep) {
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
        locs <- stringr::str_detect(colnames(measures), paste0(el, "\\b")) # exact match required
        ls[[el]] <- measures[, locs, drop = FALSE]
        measures <- measures[, !locs, drop = FALSE]
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

  # add old unmerged measures to merged measures (if keep is TRUE)
  if (keep) measures <- cbind(measures, measuresOld[, !(namesOld %in% colnames(measures))])

  # update information in sentmeasures object
  newNames <- stringr::str_split(colnames(measures), "--")

  sentmeasures$measures <- measures
  sentmeasures$lexicons <- unique(sapply(newNames, "[", 1))
  sentmeasures$features <- unique(sapply(newNames, "[", 2))
  sentmeasures$time <- unique(sapply(newNames, "[", 3))

  return(sentmeasures)
}

#' Setup control for aggregation into sentiment measures
#'
#' @description Sets up control for aggregation of document-level textual sentiment into textual
#' sentiment measures (indices).
#'
#' @details For currently available options on how aggregation can occer, call \code{get_hows()}.
#'
#' @param howWithin a single character vector defining how aggregation within documents will be performed.
#' @param howDocs a single character vector defining how aggregation across documents per date will be performed.
#' @param howTime a single character vector defining how aggregation across dates will be performed.
#' @param ignoreZeros a logical indicating whether zero sentiment values have to be ignored while aggregation
#' across documents.
#' @param lag the time lag to be specified for aggregation across time.
#' @param ... other parameters required for the time weighting schemes.
#'
#' @return A list encapsulating the control parameters.
#'
#' @seealso \code{\link{get_hows}}
#'
#' @export
ctrl_agg <- function(howWithin, howDocs, howTime, ignoreZeros, lag, ...) {

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
  if (warned > 0) stop("Wrong inputs. See warning messages for specifics.")

  ctrlAgg <- list(howWithin = howWithin,
                  howDocs = howDocs,
                  howTime = howTime,
                  ignoreZeros = ignoreZeros,
                  lag = lag,
                  other = list(...))

  return(ctrlAgg)
}

#' Setup control for merging sentiment measures
#'
#' @description Sets up control for the optional merging (additional aggregation) of sentiment measures.
#'
#' @param sentmeasures a \code{sentmeasures} object.
#' @param lex a list with unique lexicons to merge at given name, e.g. \code{list(lex12 = c("lex1", "lex2"))}.
#' @param feat a list with unique features to merge at given name, e.g. \code{list(feat12 = c("feat1", "feat2"))}.
#' @param time a list with unique time weighting schemes to merge at given name, e.g. \code{list(tw12 = c("tw1", "tw2"))}.
#' @param keep a logical indicating if the original sentiment measures should be kept (i.e. the merged sentiment measures
#' will be added to the current sentiment measures as additional indices if \code{TRUE}).
#'
#' @return A list encapsulating the control parameters.
#'
#' @export
ctrl_merge <- function(sentmeasures, lex = NA, feat = NA, time = NA, keep = FALSE) {

  missings <- c()
  tooFew <- c()

  # check if columns to merge exist (missings) and if all merges have at least two columns to combine and are all unique (tooFew)
  if (all(!is.na(lex))) {
    missings <- c(missings, unlist(lex)[!(unlist(lex) %in% sentmeasures$lexicons)])
    for (i in seq_along(lex)) {
      if (length(lex[[i]]) <= 1 | length(unique(lex[[i]])) != length(lex[[i]]))
        tooFew <- c(tooFew, names(lex)[i])
    }
  }

  if (all(!is.na(feat))) {
    missings <- c(missings, unlist(feat)[!(unlist(feat) %in% sentmeasures$features)])
    for (i in seq_along(feat)) {
      if (length(feat[[i]]) <= 1 | length(unique(feat[[i]])) != length(feat[[i]]))
        tooFew <- c(tooFew, names(feat)[i])
    }
  }

  if (all(!is.na(time))) {
    missings <- c(missings, unlist(time)[!(unlist(time) %in% sentmeasures$time)])
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
  if (length(msg1) > 0 | length((msg2) > 0)) stop("Wrong input. See warning messages for specifics.")

  ctrlMerge <- list(sentmeasures = sentmeasures,
                    lex = lex,
                    feat = feat,
                    time = time,
                    keep = keep)

  return(ctrlMerge)
}

#' Select a subset of sentiment measures
#'
#' @description Selects a subset of sentiment measures.
#'
#' @param sentmeasures a \code{sentmeasures} object.
#' @param toSelect a vector of components (lexicon, time weighting and features) which form the measures selected.
#'
#' @return A modified \code{sentmeasures} object, with only the measures required.
#'
#' @export
select_measures <- function(sentmeasures, toSelect) {

  allOpts <- c(sentmeasures$features, sentmeasures$lexicons, sentmeasures$time)

  valid <- toSelect %in% allOpts
  if (any(!valid)) {
    stop("Following components make up none of the sentiment measures: ", paste0(toSelect[!valid], collapse = ', '))
  }

  namesList <- stringr::str_split(colnames(sentmeasures$measures), "--")

  ind <- sapply(namesList, function(x) return(all(toSelect %in% x)))

  sentmeasures$measures <- sentmeasures$measures[, ind]

  newNames <- stringr::str_split(colnames(sentmeasures$measures), "--")
  sentmeasures$lexicons <- unique(sapply(newNames, "[", 1))
  sentmeasures$features <- unique(sapply(newNames, "[", 2))
  sentmeasures$time <- unique(sapply(newNames, "[", 3))

  return(sentmeasures)
}

