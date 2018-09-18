
spread_sentiment_features <- function(sent, features, lexNames) {
  for (lexicon in lexNames) { # multiply lexicons with features to obtain feature-sentiment scores per lexicon
    nms <- paste0(lexicon, "--", features)
    sent[, nms] <- sent[[lexicon]] * sent[, features, with = FALSE]
  }
  sent[, eval(c(lexNames, features)) := NULL] # remove since replaced by lexicon--feature columns
  sent
}

tokenise_texts <- function(x, tokens = NULL) { # x is a (sento)corpus object or a character vector
  if (is.null(tokens)) {
    x <- stringi::stri_trans_tolower(x)
    tok <- stringi::stri_split_boundaries(x, type = "word", skip_word_none = TRUE, skip_word_number = TRUE)
  } else tok <- tokens
  tok
}

compute_sentiment_lexicons <- function(tok, lexicons, how, nCore = 2) {
  threads <- min(RcppParallel::defaultNumThreads(), nCore)
  RcppParallel::setThreadOptions(numThreads = threads)
  if (is.null(lexicons[["valence"]])) {
    s <- as.data.table(compute_sentiment_onegrams(tok, lexicons, how))
  } else {
    s <- as.data.table(compute_sentiment_valence(tok, lexicons, how))
  }
  s
}

#' Compute document-level sentiment across features and lexicons
#'
#' @author Samuel Borms
#'
#' @description Given a corpus of texts, computes (net) sentiment per document using the bag-of-words approach
#' based on the lexicons provided and a choice of aggregation across words per document.
#'
#' @details For a separate calculation of positive (resp. negative) sentiment, one has to provide distinct positive (resp.
#' negative) lexicons. This can be done using the \code{do.split} option in the \code{\link{sento_lexicons}} function, which
#' splits out the lexicons into a positive and a negative polarity counterpart. All \code{NA}s are converted to 0, under the
#' assumption that this is equivalent to no sentiment. If \code{tokens = NULL} (as per default), texts are tokenised as
#' unigrams using the \code{\link[tokenizers]{tokenize_words}} function. Punctuation and numbers are removed, but not
#' stopwords. The number of words for each document is computed based on that same tokenisation. All tokens are converted
#' to lowercase, in line with what the \code{\link{sento_lexicons}} function does for the lexicons and valence shifters.
#'
#' @section Calculation:
#' If the \code{lexicons} argument has no \code{"valence"} element, the sentiment computed corresponds to simple unigram
#' matching with the lexicons [\emph{unigrams} approach]. If valence shifters are included in \code{lexicons} with a
#' corresponding \code{"y"} column, these have the effect of modifying the polarity of a word detected from the lexicon if
#' appearing right before such word (examples: not good, very bad or can't defend) [\emph{bigrams} approach]. If the valence
#' table contains a \code{"t"} column, valence shifters are searched for in a cluster centered around a detected polarity word
#' [\emph{clusters} approach]. The latter approach is similar along the one utilized by the \pkg{sentimentr} package, but
#' simplified. A cluster amounts to four words before and two words after a polarity word. A cluster never overlaps with a
#' preceding one. The polarity of a cluster is calculated as \eqn{n(1 + 0.80d)S + \sum s}. The polarity score of the detected
#' word is \eqn{S}, \eqn{s} represents polarities of eventual other sentiment words, and \eqn{d} is the difference between
#' the number of amplifiers (\code{t = 2}) and the number of deamplifiers (\code{t = 3}). If there is an odd number of
#' negators (\code{t = 1}), \eqn{n = -1} and amplifiers are counted as deamplifiers, else \eqn{n = 1}. All scores, whether
#' per unigram, per bigram or per cluster, are summed within a document, before the scaling defined by the \code{how}
#' argument is applied. The \code{how = "proportionalPol"} option divides each document's sentiment score by the
#' number of detected polarized words (counting words that appear multiple times by their frequency), instead
#' of the total number of words which the \code{how = "proportional"} option gives. The \code{how = "counts"} option
#' does no normalisation.
#'
#' @param x either a \code{sentocorpus} object created with \code{\link{sento_corpus}}, a \pkg{quanteda}
#' \code{\link[quanteda]{corpus}} object, or a \code{character} vector. The latter two do not incorporate a
#' date dimension. In case of a \code{\link[quanteda]{corpus}} object, the \code{numeric} columns from the
#' \code{\link[quanteda]{docvars}} are considered as features over which sentiment will be computed. In
#' case of a \code{character} vector, sentiment is only computed across lexicons.
#' @param lexicons a \code{sentolexicons} object created using \code{\link{sento_lexicons}}.
#' @param how a single \code{character} vector defining how aggregation within documents should be performed. For currently
#' available options on how aggregation can occur, see \code{\link{get_hows}()$words}.
#' @param tokens a \code{list} of tokenised documents, to specify your own tokenisation scheme. Can result from the
#' \pkg{quanteda}'s \code{\link[quanteda]{tokens}} function, the \pkg{tokenizers} package, or other. Make sure the tokens are
#' constructed from (the texts from) the \code{x} argument, are unigrams, and preferably set to lowercase, otherwise, results
#' may be spurious and errors could occur. By default set to \code{NULL}.
#' @param nCore a positive \code{numeric} that will be passed on to the \code{numThreads} argument of the
#' \code{\link[RcppParallel]{setThreadOptions}} function, to parallelise the sentiment computation across texts. A
#' value of 1 implies no parallelisation. Parallelisation is expected to improve speed of the sentiment computation
#' only for sufficiently large corpora, say, in the order of having at least 100,000 documents.
#'
#' @return A \code{list} containing:
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
#' l1 <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")])
#' l2 <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#' l3 <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")],
#'                      list_valence_shifters[["en"]][, c("x", "t")])
#'
#' # from a sentocorpus object, unigram approach
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 200)
#' sent1 <- compute_sentiment(corpusSample, l1, how = "proportionalPol")
#'
#' # from a character vector, bigram approach
#' sent2 <- compute_sentiment(usnews[["texts"]][1:200], l2, how = "counts")
#'
#' # from a corpus object, cluster approach
#' corpusQ <- quanteda::corpus(usnews, text_field = "texts")
#' corpusQSample <- quanteda::corpus_sample(corpusQ, size = 200)
#' sent3 <- compute_sentiment(corpusQSample, l3, how = "counts", nCore = 2)
#'
#' # from an already tokenised corpus, using the 'tokens' argument
#' toks <- as.list(quanteda::tokens(corpusQSample, what = "fastestword"))
#' sent4 <- compute_sentiment(corpusQSample, l1[1], how = "counts",
#'                            tokens = toks)
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
  check_class(lexicons, "sentolexicons")

  UseMethod("compute_sentiment", x)
}

.compute_sentiment.sentocorpus <- function(x, lexicons, how, tokens = NULL, nCore = 2) {
  nCore <- check_nCore(nCore)
  features <- names(quanteda::docvars(x))[-1] # drop date column
  tok <- tokenise_texts(quanteda::texts(x), tokens)
  s <- compute_sentiment_lexicons(tok, lexicons, how, nCore) # compute sentiment per document for all lexicons
  lexNames <- colnames(s)[-1]
  s <- cbind(id = quanteda::docnames(x), quanteda::docvars(x), s) # id - date - features - word_count - lexicons/sentiment
  sent <- spread_sentiment_features(s, features, lexNames) # compute feature-sentiment
  sent <- sent[order(date)] # order by date
  sentOut <- list(sentiment = sent, features = features, lexicons = lexNames, howWithin = how)
  sentOut
}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.sentocorpus <- compiler::cmpfun(.compute_sentiment.sentocorpus)

.compute_sentiment.corpus <- function(x, lexicons, how, tokens = NULL, nCore = 2) {
  nCore <- check_nCore(nCore)
  isNumeric <- sapply(quanteda::docvars(x), is.numeric)
  if (length(isNumeric) == 0) features <- NULL else features <- names(isNumeric[isNumeric])
  tok <- tokenise_texts(quanteda::texts(x), tokens)
  s <- compute_sentiment_lexicons(tok, lexicons, how, nCore) # compute sentiment per document for all lexicons
  if (!is.null(features)) { # spread sentiment across features if present and reformat
      lexNames <- colnames(s)[-1]
      s <- cbind(id = quanteda::docnames(x), quanteda::docvars(x)[features], s)
      sent <- spread_sentiment_features(s, features, lexNames)[] # compute feature-sentiment per document for all lexicons
  } else {
    sent <- cbind(id = quanteda::docnames(x), s)
  }
  sent
}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.corpus <- compiler::cmpfun(.compute_sentiment.corpus)

.compute_sentiment.character <- function(x, lexicons, how, tokens = NULL, nCore = 2) {
  nCore <- check_nCore(nCore)
  tok <- tokenise_texts(x, tokens)
  s <- compute_sentiment_lexicons(tok, lexicons, how, nCore) # compute sentiment per document for all lexicons
  s
}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.character <- compiler::cmpfun(.compute_sentiment.character)

