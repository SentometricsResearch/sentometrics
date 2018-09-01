
spread_sentiment_features <- function(sent, features, lexNames) {
  for (lexicon in lexNames) { # multiply lexicons with features to obtain feature-sentiment scores per lexicon
    nms <- paste0(lexicon, "--", features)
    sent[, nms] <- sent[[lexicon]] * sent[, features, with = FALSE]
  }
  sent[, eval(c(lexNames, features)) := NULL][] # remove since replaced by lexicon--feature columns
  sent
}

tokenise_texts <- function(x, tokens = NULL) { # x is a (sento)corpus object or a character vector
  if (is.null(tokens)) {
    # tok <- quanteda::tokens(
    #   x, what = "fasterword", ngrams = 1,
    #   remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE
    # )
    # tok <- quanteda::tokens_tolower(tok) # to lowercase
    tok <- tokenizers::tokenize_words(x, lowercase = TRUE, strip_punct = TRUE, strip_numeric = TRUE)
  } else tok <- tokens
  wCounts <- sapply(tok, length)
  return(list(tok = tok, wCounts = wCounts))
}

# compute_sentiment_onegrams <- function(dfm, lexicons, how, wCounts) {
#
#   if (how == "counts" || how == "proportional" || how == "proportionalPol") {
#     fdm <- quanteda::t(dfm) # feature-document matrix
#   } else if (how == "tf-idf") {
#     weights <- quanteda::dfm_tfidf(dfm, scheme_tf = "prop")
#     fdmWeighted <- quanteda::t(weights)
#   } else stop("Please select an appropriate aggregation 'how'.")
#
#   lexNames <- names(lexicons)
#   s <- as.data.table(matrix(0, nrow = nrow(dfm), ncol = length(lexNames)))
#   names(s) <- lexNames
#   allWords <- quanteda::featnames(dfm)
#   for (lexicon in lexNames) { # locate polarized words and set weights to their polarity or keep at zero
#     lexWords <- lexicons[[lexicon]]$x
#     lexScores <- lexicons[[lexicon]]$y
#     names(lexScores) <- lexWords
#     allScores <- rep(0, length(allWords))
#     polInd <- allWords %in% lexWords
#     allScores[polInd] <- lexScores[allWords[polInd]]
#     names(allScores) <- allWords
#     if (how == "counts") {
#       scores <- quanteda::rowSums(quanteda::t(fdm * allScores))
#     } else if (how == "proportional") {
#       scores <- quanteda::rowSums(quanteda::t(fdm * allScores)) / wCounts
#     } else if (how == "proportionalPol") {
#       wordScores <- quanteda::t(fdm * allScores) # every row is a document
#       scores <- quanteda::rowSums(wordScores) / quanteda::rowSums(wordScores != 0)
#     } else scores <- quanteda::rowSums(quanteda::t(fdmWeighted * allScores))
#     scores[is.na(scores)] <-
#     s[, (lexicon) := scores]
#   }
#   s
# }

compute_sentiment_lexicons <- function(tok, lexicons, how, nCore = 2) {
  threads <- min(RcppParallel::defaultNumThreads(), nCore)
  RcppParallel::setThreadOptions(numThreads = threads)
  if ("valence" %in% names(lexicons)) {
    s <- as.data.table(compute_sentiment_bigrams(tok, lexicons, how))
  } else {
    s <- as.data.table(compute_sentiment_onegrams(tok, lexicons, how))
  }
  s
}

#' Compute document-level sentiment across features and lexicons
#'
#' @author Samuel Borms
#'
#' @description Given a corpus of texts, computes (net) sentiment per document using the bag-of-words approach
#' based on the lexicons provided and a choice of aggregation across words per document. If the \code{lexicons} argument
#' has no \code{"valence"} element, the sentiment computed corresponds to simple unigram matching with the lexicons. If
#' valence shifters are included in \code{lexicons}, these have the effect of modifying the polarity of a word detected from
#' the lexicon if appearing right before such word (examples: not good, very bad or can't defend).
#'
#' @details
#' For a separate calculation of positive (resp. negative) sentiment, one has to provide distinct positive (resp. negative)
#' lexicons. This can be done using the \code{do.split} option in the \code{\link{setup_lexicons}} function, which splits out
#' the lexicons into a positive and a negative polarity counterpart. All \code{NA}s are converted to 0, under the assumption
#' that this is equivalent to no sentiment. If \code{tokens = NULL} (as per default), texts are tokenised as unigrams using
#' the \code{\link[tokenizers]{tokenize_words}} function. Punctuation and numbers are removed, but not stopwords. The number
#' of words for each document is computed based on that same tokenisation. All tokens are converted to lowercase, in line
#' with what the \code{\link{setup_lexicons}} function does for the lexicons and valence shifters.
#'
#' @param x either a \code{sentocorpus} object created with \code{\link{sento_corpus}}, a \pkg{quanteda}
#' \code{\link[quanteda]{corpus}} object, or a \code{character} vector. The latter two do not incorporate a
#' date dimension. In case of a \code{\link[quanteda]{corpus}} object, the \code{numeric} columns from the
#' \code{\link[quanteda]{docvars}} are considered as features over which sentiment will be computed. In
#' case of a \code{character} vector, sentiment is only computed across lexicons.
#' @param lexicons output from a \code{\link{setup_lexicons}} call, i.e., a named \code{list} of lexicons.
#' @param how a single \code{character} vector defining how aggregation within documents should be performed. For currently
#' available options on how aggregation can occur, see \code{\link{get_hows}()$words}.
#' @param tokens a \code{list} of tokenized documents, to specify your own tokenisation scheme. Can result from the
#' \pkg{quanteda}'s \code{\link[quanteda]{tokens}} function, the \pkg{tokenizers} package, or other. Make sure the tokens are
#' constructed from (the texts from) the input corpus, are unigrams, and preferably set to lowercase, otherwise, results
#' will be spurious or errors may occur. By default set to \code{NULL}.
#' @param nCore a positive \code{numeric} passed on to the \code{numThreads} argument of the
#' \code{\link[RcppParallel]{setThreadOptions}} function, to parallelize the sentiment computation across texts. A
#' value of 1 implies no parallelisation. Parallelisation is expected to improve speed of the sentiment computation
#' only for sufficiently large corpora, say, in the order of having at least 100,000 documents.
#'
#' @return A \code{list} containing:
#' \item{corpus}{the supplied \code{x} object (except if \code{x} is a \code{character} vector).}
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
#' sent <- compute_sentiment(corpusSample, l1, how = "proportionalPol")
#'
#' # from a character vector
#' sent <- compute_sentiment(usnews[["texts"]][1:200], l1, how = "counts")
#'
#' \dontrun{
#' # from a corpus object, parallelized
#' corpusQ <- quanteda::corpus(usnews, text_field = "texts")
#' sent <- compute_sentiment(corpusQ, l2, how = "counts", nCore = 2)}
#'
#' @importFrom compiler cmpfun
#' @export
compute_sentiment <- function(x, lexicons, how = "proportional", tokens = NULL, nCore = 2) {
  if (!is_names_correct(names(lexicons)))
    stop("At least one lexicon's name contains '-'. Please provide proper names.")
  if (!(how %in% get_hows()[["words"]]))
    stop("Please select an appropriate aggregation 'how'.")
  if (length(nCore) != 1 || !is.numeric(nCore))
    stop("The 'nCore' argument should be a numeric vector of size one.")
  if (!is.null(tokens)) stopifnot(is.list(tokens))

  UseMethod("compute_sentiment", x)
}

.compute_sentiment.sentocorpus <- function(x, lexicons, how, tokens = NULL, nCore = 2) {
  nCore <- check_nCore(nCore)

  sentocorpus <- x
  sentOut <- list(corpus = sentocorpus) # original corpus in output

  features <- names(quanteda::docvars(sentocorpus))[-1] # drop date column

  tokenised <- tokenise_texts(quanteda::texts(sentocorpus), tokens)
  tok <- tokenised[["tok"]]
  wCounts <- tokenised[["wCounts"]]

  s <- compute_sentiment_lexicons(tok, lexicons, how, nCore) # compute sentiment per document for all lexicons

  # reconstruct sentiment to id - date - features - word_count - lexicons/sentiment, and compute feature-sentiment
  lexNames <- names(lexicons)[names(lexicons) != "valence"]
  s <- as.data.table(cbind(id = quanteda::docnames(sentocorpus), quanteda::docvars(sentocorpus), word_count = wCounts, s))
  sent <- spread_sentiment_features(s, features, lexNames)
  sent <- sent[order(date)] # order by date

  sentOut <- c(sentOut, list(sentiment = sent[], features = features, lexicons = lexNames, howWithin = how))

  return(sentOut)
}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.sentocorpus <- compiler::cmpfun(.compute_sentiment.sentocorpus)

.compute_sentiment.corpus <- function(x, lexicons, how, tokens = NULL, nCore = 2) {
  nCore <- check_nCore(nCore)

  corpus <- x
  sentOut <- list(corpus = corpus) # original corpus in output

  isNumeric <- sapply(quanteda::docvars(corpus), is.numeric)
  if (length(isNumeric) == 0) features <- NULL else features <- names(isNumeric[isNumeric])

  tokenised <- tokenise_texts(quanteda::texts(corpus), tokens)
  tok <- tokenised[["tok"]]
  wCounts <- tokenised[["wCounts"]]

  s <- compute_sentiment_lexicons(tok, lexicons, how, nCore) # compute sentiment per document for all lexicons

  # spread sentiment across features if present and reformat
  if (!is.null(features)) {
      s <- as.data.table(cbind(id = quanteda::docnames(corpus), quanteda::docvars(corpus)[features], word_count = wCounts, s))
      lexNames <- names(lexicons)[names(lexicons) != "valence"]
      sent <- spread_sentiment_features(s, features, lexNames) # compute feature-sentiment per document for all lexicons
  } else {
    sent <- as.data.table(cbind(id = quanteda::docnames(corpus), word_count = wCounts, s))
  }

  sentOut[["sentiment"]] <- sent[]

  return(sentOut)
}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.corpus <- compiler::cmpfun(.compute_sentiment.corpus)

.compute_sentiment.character <- function(x, lexicons, how, tokens = NULL, nCore = 2) {
  nCore <- check_nCore(nCore)

  tokenised <- tokenise_texts(x, tokens)
  tok <- tokenised[["tok"]]
  wCounts <- tokenised[["wCounts"]]

  s <- compute_sentiment_lexicons(tok, lexicons, how, nCore) # compute sentiment per document for all lexicons

  return(data.table(word_count = wCounts, s[]))
}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.character <- compiler::cmpfun(.compute_sentiment.character)

