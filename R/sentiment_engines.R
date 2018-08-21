
spread_sentiment_features <- function(sent, features, lexNames) {
  for (lexicon in lexNames) { # multiply lexicons with features to obtain feature-sentiment scores per lexicon
    nms <- paste0(lexicon, "--", features)
    sent[, nms] <- sent[[lexicon]] * sent[, features, with = FALSE]
  }
  sent[, eval(c(lexNames, features)) := NULL][] # remove since replaced by lexicon--feature columns
  return(sent)
}

tokenise_texts <- function(corpus, what = "word") {
  tok <- quanteda::tokens(
    corpus,
    what = what,
    ngrams = 1,
    remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE
  )
  tok <- quanteda::tokens_tolower(tok) # to lowercase
  wCounts <- quanteda::ntoken(tok)
  return(list(tok = tok, wCounts = wCounts))
}

compute_sentiment_onegrams <- function(dfm, how, lexicons, wCounts) {

  if (how == "counts" || how == "proportional" || how == "proportionalPol") {
    fdm <- quanteda::t(dfm) # feature-document matrix
  } else if (how == "tf-idf") {
    weights <- quanteda::dfm_tfidf(dfm, scheme_tf = "prop")
    fdmWeighted <- quanteda::t(weights)
  } else stop("Please select an appropriate aggregation 'how'.")

  lexNames <- names(lexicons)
  s <- as.data.table(matrix(0, nrow = nrow(dfm), ncol = length(lexNames)))
  names(s) <- lexNames
  allWords <- quanteda::featnames(dfm)
  for (lexicon in lexNames) { # locate polarized words and set weights to their polarity or keep at zero
    lexWords <- lexicons[[lexicon]]$x
    lexScores <- lexicons[[lexicon]]$y
    names(lexScores) <- lexWords
    allScores <- rep(0, length(allWords))
    polInd <- allWords %in% lexWords
    allScores[polInd] <- lexScores[allWords[polInd]]
    names(allScores) <- allWords
    if (how == "counts") {
      scores <- quanteda::rowSums(quanteda::t(fdm * allScores))
    } else if (how == "proportional") {
      scores <- quanteda::rowSums(quanteda::t(fdm * allScores)) / wCounts
    } else if (how == "proportionalPol") {
      wordScores <- quanteda::t(fdm * allScores) # every row is a document
      scores <- quanteda::rowSums(wordScores) / quanteda::rowSums(wordScores != 0)
    } else scores <- quanteda::rowSums(quanteda::t(fdmWeighted * allScores))
    scores[is.na(scores)] <- 0 # set NA/NaN sentiment to 0 (e.g., if document contains no words)
    s[, (lexicon) := scores]
  }

  return(s[])
}

compute_sentiment_lexicons <- function(tok, lexicons, how, wCounts) {
  doValence <- "valence" %in% names(lexicons)
  if (doValence == TRUE) {
    RcppParallel::setThreadOptions(numThreads = RcppParallel::defaultNumThreads() - 1)
    s <- as.data.table(compute_sentiment_bigrams(as.list(tok), lexicons, how)) # C++ implementation
  } else {
    dfm <- quanteda::dfm(tok, tolower = FALSE, verbose = FALSE) # rows: corpus ids, columns: words, values: frequencies
    s <- compute_sentiment_onegrams(dfm, how, lexicons, wCounts)
  }
  return(s)
}

#' Compute document-level sentiment across features and lexicons
#'
#' @author Samuel Borms
#'
#' @description Given a corpus of texts, computes sentiment per document using the bag-of-words approach
#' based on the lexicons provided and a choice of aggregation across words per document. Relies partly on the
#' \pkg{quanteda} package. The scores computed are net sentiment (sum of positive minus sum of negative scores).
#'
#' @details
#' For a separate calculation of positive (resp. negative) sentiment, one has to provide distinct positive (resp. negative)
#' lexicons. This can be done using the \code{do.split} option in the \code{\link{setup_lexicons}} function, which splits out
#' the lexicons into a positive and a negative polarity counterpart. \code{NA}s are converted to 0, under the assumption that
#' this is equivalent to no sentiment. By default, if the \code{dfm} argument is left unspecified, a document-feature matrix
#' (dfm) is created based on a tokenisation that removes punctuation, numbers and symbols, but does not remove
#' stopwords. The number of words for each document is computed based on that same tokenisation. A valence shifter and its
#' neighbouring word, for example 'NOT_good', is counted as one word. All tokens are converted to lowercase, in
#' line with what the \code{\link{setup_lexicons}} function does for the lexicons and valence shifters.
#'
#' @param x either a \code{sentocorpus} object created with \code{\link{sento_corpus}}, a \pkg{quanteda}
#' \code{\link[quanteda]{corpus}} object, or a \code{character} vector. The latter two do not incorporate a
#' date dimension. In case of a \code{\link[quanteda]{corpus}} object, the \code{numeric} columns from the
#' \code{\link[quanteda]{docvars}} are considered as features over which sentiment will be computed. In
#' case of a \code{character} vector, sentiment is only computed across lexicons.
#' @param lexicons output from a \code{\link{setup_lexicons}} call.
#' @param how a single \code{character} vector defining how aggregation within documents should be performed. For currently
#' available options on how aggregation can occur, see \code{\link{get_hows}()$words}.
#' @param nCore a single \code{numeric} at least equal to 1 to indicate the number of cores to use for a parallel sentiment
#' computation. We use the \code{\%dopar\%} construct from the \pkg{foreach} package. By default, \code{nCore = 1}, which
#' implies no parallelization.
#' @param dfm (optional) an output from a \pkg{quanteda} \code{\link[quanteda]{dfm}} call, such that users can specify their
#' own tokenisation scheme (via \code{\link[quanteda]{tokens}}) as well as other parameters related to the construction of
#' a document-feature matrix (dfm). Make sure the document-feature matrix is constructed from the texts in the
#' \code{sentocorpus} object, otherwise, results will be spurious or errors may occur.
#'
#' @return A \code{list} containing:
#' \item{corpus}{the supplied \code{x} object, transformed into a \code{\link[quanteda]{corpus}} if a \code{character} vector.}
#' \item{sentiment}{the sentiment scores \code{data.table} with a \code{"word_count"} column and all
#' lexicon--feature sentiment scores columns.}
#' \item{features}{a \code{character} vector of the different features.}
#' \item{lexicons}{a \code{character} vector of the different lexicons used.}
#' \item{howWithin}{the supplied \code{how} argument.}
#'
#' The last three elements are only present if \code{x} is a \code{sentocorpus} object. In that case, the
#' \code{"sentiment"} \code{data.table} also has a \code{"date"} column, meaning it can be used for further
#' aggregation into sentiment time series with the \code{\link{perform_agg}} function.
#'
#' @seealso \code{\link[quanteda]{dfm}}, \code{\link[quanteda]{tokens}}
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' l1 <- list_lexicons[c("LM_en", "HENRY_en")]
#' l2 <- setup_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#'
#' # from a sentocorpus object
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 200)
#' sent <- compute_sentiment(corpusSample, l1, how = "counts")
#'
#' # from a character vector
#' sent <- compute_sentiment(usnews[["texts"]][1:200], l1, how = "counts")
#'
#' \dontrun{
#' # from a corpus object, parallelized
#' corpusQ <- quanteda::corpus(usnews, text_field = "texts")
#' sent <- compute_sentiment(corpusQ, l2, how = "counts", nCore = 2)}
#'
#' \dontrun{
#' # using a user-supplied dfm with default settings
#' tok <- quanteda::tokens_tolower(quanteda::tokens(corpus))
#' dfm <- quanteda::dfm(tok, verbose = FALSE)
#' sent <- compute_sentiment(corpus, l1, how = "counts", dfm = dfm)}
#'
#' @importFrom compiler cmpfun
#' @export
compute_sentiment <- function(x, lexicons, how = "proportional", nCore = 1, dfm = NULL) {
  if (!is_names_correct(names(lexicons)))
    stop("At least one lexicon's name contains '-'. Please provide proper names.")
  UseMethod("compute_sentiment", x)
}

.compute_sentiment.sentocorpus <- function(x, lexicons, how, nCore = 1, dfm = NULL) {
  sentocorpus <- x
  sentOut <- list(corpus = sentocorpus) # original corpus in output

  features <- names(quanteda::docvars(sentocorpus))[-1] # drop date column

  cat("Compute sentiment... ")

  tokenised <- tokenise_texts(sentocorpus, what = "word")
  tok <- tokenised[["tok"]]
  wCounts <- tokenised[["wCounts"]]

  s <- compute_sentiment_lexicons(tok, lexicons, how, wCounts) # compute sentiment per document for all lexicons

  # reconstruct sentiment to id - date - features - word_count - lexicons/sentiment, and compute feature-sentiment
  lexNames <- names(lexicons)[names(lexicons) != "valence"]
  s <- as.data.table(cbind(id = quanteda::docnames(sentocorpus), quanteda::docvars(sentocorpus), word_count = wCounts, s))
  sent <- spread_sentiment_features(s, features, lexNames)
  sent <- sent[order(date)] # order by date

  cat("Done.", "\n")

  sentOut <- c(sentOut, list(sentiment = sent, features = features, lexicons = lexNames, howWithin = how))

  return(sentOut)
}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.sentocorpus <- compiler::cmpfun(.compute_sentiment.sentocorpus)

.compute_sentiment.corpus <- function(x, lexicons, how, nCore = 1, dfm = NULL) {
  corpus <- x
  sentOut <- list(corpus = corpus) # original corpus in output

  isNumeric <- sapply(quanteda::docvars(corpus), is.numeric)
  if (length(isNumeric) == 0) features <- NULL else features <- names(isNumeric[isNumeric])

  cat("Compute sentiment... ")

  tokenised <- tokenise_texts(corpus, what = "word")
  tok <- tokenised[["tok"]]
  wCounts <- tokenised[["wCounts"]]

  s <- compute_sentiment_lexicons(tok, lexicons, how, wCounts) # compute sentiment per document for all lexicons

  # spread sentiment across features if present and reformat
  if (!is.null(features)) {
      s <- as.data.table(cbind(id = quanteda::docnames(corpus), quanteda::docvars(corpus)[features], word_count = wCounts, s))
      lexNames <- names(lexicons)[names(lexicons) != "valence"]
      sent <- spread_sentiment_features(s, features, lexNames) # compute feature-sentiment per document for all lexicons
  } else {
    sent <- as.data.table(cbind(id = quanteda::docnames(corpus), word_count = wCounts, s))
  }

  cat("Done.", "\n")

  sentOut[["sentiment"]] <- sent

  return(sentOut)
}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.corpus <- compiler::cmpfun(.compute_sentiment.corpus)

.compute_sentiment.character <- function(x, lexicons, how, nCore = 1, dfm = NULL) {
  corpus <- quanteda::corpus(x)
  sentOut <- list(corpus = corpus) # original corpus in output

  cat("Compute sentiment... ")

  tokenised <- tokenise_texts(corpus, what = "word")
  tok <- tokenised[["tok"]]
  wCounts <- tokenised[["wCounts"]]

  s <- compute_sentiment_lexicons(tok, lexicons, how, wCounts) # compute sentiment per document for all lexicons

  cat("Done.", "\n")

  sentOut[["sentiment"]] <- data.table(word_count = wCounts, s)

  return(sentOut)
}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.character <- compiler::cmpfun(.compute_sentiment.character)

