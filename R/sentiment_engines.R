
spread_sentiment_features <- function(sent, features, lexNames) {
  for (lexicon in lexNames) { # multiply lexicons with features to obtain feature-sentiment scores per lexicon
    nms <- paste0(lexicon, "--", features)
    sent[, nms] <- sent[[lexicon]] * sent[, features, with = FALSE]
  }
  sent[, eval(c(lexNames, features)) := NULL] # remove since replaced by lexicon--feature columns
  sent
}

tokenize_texts <- function(x, tokens = NULL, type = "word") { # x is a character vector
  if (is.null(tokens)) {
    if (type == "word") {
      x <- stringi::stri_trans_tolower(x)
      tok <- stringi::stri_split_boundaries(x, type = "word", skip_word_none = TRUE, skip_word_number = TRUE)
    } else if (type == "sentence") {
      tok <- stringi::stri_split_boundaries(x, type = "sentence")
    }
  } else tok <- tokens
  tok
}

compute_sentiment_lexicons <- function(tok, lexicons, how, nCore = 1) {
  threads <- min(RcppParallel::defaultNumThreads(), nCore)
  RcppParallel::setThreadOptions(numThreads = threads)
  if (is.null(lexicons[["valence"]])) {
    s <- as.data.table(compute_sentiment_onegrams(tok, lexicons, how))
  } else {
    s <- as.data.table(compute_sentiment_valence(tok, lexicons, how))
  }
  s
}

compute_sentiment_multiple_languages <- function(x, lexicons, languages, features, how, tokens = NULL, nCore = 1, do.sentence = FALSE) {

  ids <- quanteda::docnames(x) # original ids to keep same order in output

  # step 1: split corpus by language
  corpByLang <- lapply(languages, function(l) quanteda::corpus_subset(x, language == l))
  names(corpByLang) <- languages

  # step 2: compute sentiment for each subcorpus (language)
  sentByLang <- vector("list", 0)
  for (l in languages) {
    corpus <- corpByLang[[l]]
    if (!l %in% names(lexicons)) {
      stop(paste0("No lexicon found for language: ", l))
    }
    lex <- lexicons[[l]]
    if (do.sentence == FALSE) {
      tok <- tokenize_texts(quanteda::texts(corpus), tokens)
      sent <- compute_sentiment_lexicons(tok, lex, how, nCore)
      lexNames <- colnames(sent)[-1]
      sent <- cbind(id = quanteda::docnames(corpus), quanteda::docvars(corpus), sent)
      sent <- spread_sentiment_features(sent, features, lexNames) # compute feature-sentiment
      sentByLang[[l]] <- sent
    } else {
      sentByLang[[l]] <- compute_sentiment_by_sentences(corpus, lexicons = lex, how = how, nCore = nCore)
    }
  }

  # step 3: merge subsets of languages
  if ("sentence_id" %in% colnames(sentByLang[[1]])) {
    s <- Reduce(function(...) merge(..., by = c("id", "language", "date", "word_count", "sentence_id"), all = TRUE), sentByLang)
  } else {
    s <- Reduce(function(...) merge(..., by = c("id", "language", "date", "word_count"), all = TRUE), sentByLang)
  }

  if ("language" %in% colnames(s)) {
    s[, "language" := NULL]
  }
  s[order(match(id, ids))]
}

#' Compute document-level sentiment across features and lexicons
#'
#' @author Samuel Borms, Jeroen Van Pelt, Andres Algaba
#'
#' @description Given a corpus of texts, computes (net) sentiment per document using the bag-of-words approach
#' based on the lexicons provided and a choice of aggregation across words per document.
#'
#' @details For a separate calculation of positive (resp. negative) sentiment, one has to provide distinct positive (resp.
#' negative) lexicons. This can be done using the \code{do.split} option in the \code{\link{sento_lexicons}} function, which
#' splits out the lexicons into a positive and a negative polarity counterpart. All \code{NA}s are converted to 0, under the
#' assumption that this is equivalent to no sentiment. If \code{tokens = NULL} (as per default), texts are tokenized as
#' unigrams using the \code{\link[tokenizers]{tokenize_words}} function. Punctuation and numbers are removed, but not
#' stopwords. The number of words for each document is computed based on that same tokenization. All tokens are converted
#' to lowercase, in line with what the \code{\link{sento_lexicons}} function does for the lexicons and valence shifters.
#'
#' @section Calculation:
#' If the \code{lexicons} argument has no \code{"valence"} element, the sentiment computed corresponds to simple unigram
#' matching with the lexicons [\emph{unigrams} approach]. If valence shifters are included in \code{lexicons} with a
#' corresponding \code{"y"} column, these have the effect of modifying the polarity of a word detected from the lexicon if
#' appearing right before such word (examples: not good, very bad or can't defend) [\emph{bigrams} approach]. If the valence
#' table contains a \code{"t"} column, valence shifters are searched for in a cluster centered around a detected polarity
#' word [\emph{clusters} approach]. The latter approach is similar along the one utilized by the \pkg{sentimentr} package,
#' but simplified. A cluster amounts to four words before and two words after a polarity word. A cluster never overlaps with
#' a preceding one. Roughly speaking, the polarity of a cluster is calculated as \eqn{n(1 + 0.80d)S + \sum s}. The polarity
#' score of the detected word is \eqn{S}, \eqn{s} represents polarities of eventual other sentiment words, and \eqn{d} is
#' the difference between the number of amplifiers (\code{t = 2}) and the number of deamplifiers (\code{t = 3}). If there
#' is an odd number of negators (\code{t = 1}), \eqn{n = -1} and amplifiers are counted as deamplifiers, else \eqn{n = 1}.
#' All scores, whether per unigram, per bigram or per cluster, are summed within a document, before the scaling defined
#' by the \code{how} argument is applied.
#'
#' The sentiment calculation on sentence level approaches each sentence as if it is a document. Depending on
#' the input either the unigrams, bigrams or clusters approach is used. For the latter, we replicated exactly
#' the default \pkg{sentimentr} package method, with a cluster of five words before and two words after the polarized
#' word. If there are commas around the polarized word, the cluster is limited (extended) to the words after
#' the previous comma and before the next comma. Adversative conjunctions (\eqn{adv}, \code{t = 4})
#' are accounted for here. If the value \eqn{(1 + 0.25adv)} is greater than 1, it is added to the overall
#' amplification weight, if smaller, it is subtracted from the total deamplification weight.
#'
#' The \code{how = "proportionalPol"} option divides each document's sentiment
#' score by the number of detected polarized words (counting words that appear multiple times by their frequency), instead
#' of the total number of words which the \code{how = "proportional"} option gives. The \code{how = "counts"} option
#' does no normalization. The \code{squareRootCounts} divides the sentiment by the square root of the number of tokens in each text.
#'  The \code{how = "UShaped"} option, gives a higher weight to words at the beginning and end of the texts.
#'  The \code{how = "invertedUShaped"} option gives a lower weight to words at the beginning
#' and the end of the texts. The \code{how = "exponential"} option gives gradually more weight the later the word appears in the text.
#' The \code{ how = "invertedExponential"} option gives gradually less weight, the later the words appears in the text. The \code{
#' how = "TF"} option gives a weight proportional to the number of times a word appears in a text. The \code{how = "logarithmicTF"} option
#' gives the same weight as \code{"TF"} but logarithmically scaled. The \code{how = "augmentedTF"} option can be used to prevent
#' a bias towards longer documents. The weight is determined by dividing the raw frequency of a token by the raw frequency
#' of the most occurring term in the document. The \code{how = "IDF"} option uses the logarithm of the division of the raw frequency of a word by the number of texts
#' in which the word appears. By doing this, words appearing in multiple texts get a lower weight. The \code{how = "TFIDF"},
#' \code{how = "logarithmicTFIDF"}, \code{how = "augmentedTFIDF"} options use the same weights as there \code{IDF}-variant
#' but then multiplied with the \code{how = "IDF"} option. See the vignette for more details.
#'
#' @param x either a \code{sentocorpus} object created with \code{\link{sento_corpus}}, a \pkg{quanteda}
#' \code{\link[quanteda]{corpus}} object, or a \code{character} vector. The latter two do not incorporate a
#' date dimension. In case of a \code{\link[quanteda]{corpus}} object, the \code{numeric} columns from the
#' \code{\link[quanteda]{docvars}} are considered as features over which sentiment will be computed. In
#' case of a \code{character} vector, sentiment is only computed across lexicons.
#' @param lexicons a \code{sentolexicons} object created using \code{\link{sento_lexicons}}.
#' @param how a single \code{character} vector defining how aggregation within documents should be performed. For currently
#' available options on how aggregation can occur, see \code{\link{get_hows}()$words}.
#' @param tokens a \code{list} of tokenized documents, to specify your own tokenization scheme. Can result from the
#' \pkg{quanteda}'s \code{\link[quanteda]{tokens}} function, the \pkg{tokenizers} package, or other. Make sure the tokens are
#' constructed from (the texts from) the \code{x} argument, are unigrams, and preferably set to lowercase, otherwise, results
#' may be spurious and errors could occur. By default set to \code{NULL}.
#' @param nCore a positive \code{numeric} that will be passed on to the \code{numThreads} argument of the
#' \code{\link[RcppParallel]{setThreadOptions}} function, to parallelize the sentiment computation across texts. A
#' value of 1 (default) implies no parallelization. Parallelization is expected to improve speed of the sentiment
#' computation only for sufficiently large corpora.
#' @param do.sentence a \code{logical} to indicate whether the sentiment computation should be done on
#' sentence-level rather than document-level. By default \code{do.sentence = TRUE}. Sentiment computation on
#' sentence level follows the methodology defined in the \pkg{sentimentr} package.
#'
#' @return If \code{x} is a \code{sentocorpus} object, a \code{sentiment} object, i.e., a \code{data.table} containing
#' the sentiment scores \code{data.table} with an \code{"id"}, a \code{"date"} and a \code{"word_count"} column,
#' and all lexicon--feature sentiment scores columns. A \code{sentiment} object can be used for aggregation into
#' time series with the \code{\link{aggregate.sentiment}} function.
#'
#' @return If \code{x} is a \pkg{quanteda} \code{\link[quanteda]{corpus}} object, a sentiment scores
#' \code{data.table} with an \code{"id"} and a \code{"word_count"} column, and all lexicon--feature
#' sentiment scores columns.
#'
#' @return If \code{x} is a \code{character} vector, a sentiment scores
#' \code{data.table} with a \code{"word_count"} column, and all lexicon--feature sentiment scores columns.
#'
#' @return If \code{x} is a \code{SimpleCorpus} object, a sentiment scores
#' \code{data.table} with a \code{"word_count"} column, and all lexicon--feature sentiment scores columns.
#'
#' @return If \code{x} is a \code{VCorpus} object, a sentiment scores
#' \code{data.table} with an \code{"id"} and a \code{"word_count"} column, and all lexicon--feature
#' sentiment scores columns.
#'
#' @return If \code{do.sentence = TRUE}, an additional column \code{"sentence_id"} along the \code{"id"}
#' column is added. To use the resulting object for aggregation into time series with
#' the \code{\link{aggregate.sentiment}} function, it should first be aggregated to document level with the
#' \code{aggregate_sentences} function.
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' l1 <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")])
#' l2 <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")],
#'                      list_valence_shifters[["en"]])
#' l3 <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")],
#'                      list_valence_shifters[["en"]][, c("x", "t")])
#'
#' # from a sentocorpus object, unigrams approach
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 200)
#' sent1 <- compute_sentiment(corpusSample, l1, how = "proportionalPol")
#'
#' # from a character vector, bigrams approach
#' sent2 <- compute_sentiment(usnews[["texts"]][1:200], l2, how = "counts")
#'
#' # from a corpus object, clusters approach
#' corpusQ <- quanteda::corpus(usnews, text_field = "texts")
#' corpusQSample <- quanteda::corpus_sample(corpusQ, size = 200)
#' sent3 <- compute_sentiment(corpusQSample, l3, how = "counts")
#'
#' # from an already tokenized corpus, using the 'tokens' argument
#' toks <- as.list(quanteda::tokens(corpusQSample, what = "fastestword"))
#' sent4 <- compute_sentiment(corpusQSample, l1[1], how = "counts", tokens = toks)
#'
#' # from a SimpleCorpus object, unigrams approach
#' txt <- system.file("texts", "txt", package = "tm")
#' sc <- tm::SimpleCorpus(tm::DirSource(txt, encoding = "UTF-8"),
#'                        control = list(language = "eng"))
#' sent5 <- compute_sentiment(sc, l1, how = "proportional")
#'
#' # from a VCorpus object, unigrams approach
#' reut21578 <- system.file("texts", "crude", package = "tm")
#' vcorp <- tm::VCorpus(tm::DirSource(reut21578, mode = "binary"),
#'                      list(reader = tm::readReut21578XMLasPlain))
#' sent6 <- compute_sentiment(vcorp, l1, how = "proportional")
#'
#' # from a sentocorpus object, unigrams approach with tf-idf weighting
#' sent7 <- compute_sentiment(corpusSample, l1, how = "TFIDF")
#'
#' # tokenisation and computation sentence-by-sentence
#' sent8 <- compute_sentiment(corpusSample, l1, how = "squareRootCounts",
#'                            do.sentence = TRUE)
#'
#' # from an artificially constructed multilingual corpus
#' usnews[["language"]] <- "en" # add language column
#' usnews$language[1:100] <- "fr"
#' l_en <- sento_lexicons(list("FEEL_en" = list_lexicons$FEEL_en_tr))
#' l_fr <- sento_lexicons(list("FEEL_fr" = list_lexicons$FEEL_fr))
#' lexicons <- list(en = l_en, fr = l_fr)
#' corpusLang <- sento_corpus(corpusdf = usnews[1:250, ])
#' sent9 <- compute_sentiment(corpusLang, lexicons, how = "proportional")
#'
#' @importFrom compiler cmpfun
#' @export
compute_sentiment <- function(x, lexicons, how = "proportional", tokens = NULL, nCore = 1, do.sentence = FALSE) {
  if (!(how %in% get_hows()[["words"]]))
    stop("Please select an appropriate aggregation 'how'.")
  if (length(nCore) != 1 || !is.numeric(nCore))
    stop("The 'nCore' argument should be a numeric vector of size one.")
  if (!is.null(tokens)) stopifnot(is.list(tokens))
  if (!inherits(lexicons, "sentolexicons")) { # if a list
    for (lex in lexicons) {
      check_class(lex, "sentolexicons")
      if (!is_names_correct(names(lex)))
        stop("At least one lexicon's name contains '-'. Please provide proper names.")
    }
  } else {
    check_class(lexicons, "sentolexicons")
    if (!is_names_correct(names(lexicons)))
      stop("At least one lexicon's name contains '-'. Please provide proper names.")
  }

  if (!inherits(lexicons, "sentolexicons")) {
    # only a sentocorpus can have a language column and deal with a list of sentolexicons
    if (!("sentocorpus" %in% class(x))) {
      stop("List of multiple lexicons only allowed in combination with a sentocorpus.")
    } else {
      if (!is.null(quanteda::docvars(x, field = "language"))) {
        # if language is in sentocorpus, each language needs to be covered by the list of sentolexicons objects
        if (!all(unique(quanteda::docvars(x, field = "language")) %in% names(lexicons))) {
          stop("Lexicons do not cover all languages in corpus.")
        }
        nms <- c(names(lexicons), sapply(lexicons, names))
        if (sum(duplicated(nms)) > 0) { # check for duplicated lexicon names
          duplics <- unique(nms[duplicated(nms)])
          stop(paste0("Names of lexicons within and/or across languages are not unique. ",
                      "Following names occur at least twice: ", paste0(duplics, collapse = ", "), "."))
        }
      } else { # if language not in sentocorpus, one sentolexicons object is expected and not a list of sentolexicons objects
        stop("List of sentolexicons objects only allowed if multiple languages in sentocorpus.")
      }
    }
  } else {
    if (inherits(x, "sentocorpus")) {
      if("language" %in% names(quanteda::docvars(x)))
        stop("Provide a list of sentolexicons when using language in the sentocorpus.")
    }
  }

  UseMethod("compute_sentiment", x)
}

.compute_sentiment.sentocorpus <- function(x, lexicons, how, tokens = NULL, nCore = 1, do.sentence = FALSE) {
  nCore <- check_nCore(nCore)
  if ("language" %in% names(quanteda::docvars(x))) {
    features <- names(quanteda::docvars(x))[-c(1:2)] # drop date and language column
    languages <- unique(quanteda::docvars(x, field = "language"))
    lex <- lexicons[[languages]]
  } else {
    features <- names(quanteda::docvars(x))[-1] # drop date column
    languages <- "NA" ### TODO: NA or "NA"?
    lex <- lexicons
  }
  if (length(languages) == 1) {
    # if only one language in corpus then regular approach
    if (do.sentence == FALSE) {
      tok <- tokenize_texts(quanteda::texts(x), tokens)
      s <- compute_sentiment_lexicons(tok, lex, how, nCore) # compute sentiment per document for all lexicons
      lexNames <- colnames(s)[-1]
      s <- cbind(id = quanteda::docnames(x), quanteda::docvars(x), s) # id - date - features - word_count - lexicons/sentiment
      sent <- spread_sentiment_features(s, features, lexNames) # compute feature-sentiment
    } else {
      sent <- compute_sentiment_by_sentences(x, lexicons = lex, how = how, nCore = nCore)
    }
  } else {
    sent <- compute_sentiment_multiple_languages(x, lexicons, languages, features, how, tokens, nCore, do.sentence)
  }
  if ("language" %in% colnames(sent)) {
    sent[, "language" := NULL]
  }
  # sent <- sent[order(date)] # order by date
  class(sent) <- c("sentiment", class(sent))
  sent
}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.sentocorpus <- compiler::cmpfun(.compute_sentiment.sentocorpus)

.compute_sentiment.corpus <- function(x, lexicons, how, tokens = NULL, nCore = 1, do.sentence = FALSE) {
  nCore <- check_nCore(nCore)

  if (!do.sentence) {
    if (ncol(quanteda::docvars(x)) == 0) {
      features <- NULL
    } else {
      isNumeric <- sapply(quanteda::docvars(x), is.numeric)
      if (sum(isNumeric) == 0) features <- NULL else features <- names(isNumeric[isNumeric])
    }

    tok <- tokenize_texts(quanteda::texts(x), tokens)
    s <- compute_sentiment_lexicons(tok, lexicons, how, nCore) # compute sentiment per document for all lexicons
    if (!is.null(features)) { # spread sentiment across numeric features if present and reformat
        lexNames <- colnames(s)[-1]
        s <- cbind(id = quanteda::docnames(x), quanteda::docvars(x)[features], s)
        sent <- spread_sentiment_features(s, features, lexNames)[] # compute feature-sentiment per document for all lexicons
    } else {
      sent <- cbind(id = quanteda::docnames(x), s)
    }
  } else {
    sent <- compute_sentiment_by_sentences(x, lexicons, how, tokens, nCore = nCore)
  }
  sent
}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.corpus <- compiler::cmpfun(.compute_sentiment.corpus)

.compute_sentiment.character <- function(x, lexicons, how, tokens = NULL, nCore = 1, do.sentence = FALSE) {
  nCore <- check_nCore(nCore)
  if (!do.sentence) {
    tok <- tokenize_texts(x, tokens)
    s <- compute_sentiment_lexicons(tok, lexicons, how, nCore) # compute sentiment per document for all lexicons
  } else {
    sentences <- tokenize_texts(x, type = "sentence")
    sentencesSplit <- unlist(sentences)
    tok <- tokenize_texts(sentencesSplit, tokens)
    s <- compute_sentiment_by_sentences(NULL, lexicons, how, tok, nCore)
  }
  s
}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.character <- compiler::cmpfun(.compute_sentiment.character)

.compute_sentiment.VCorpus <- function(x, lexicons, how, tokens = NULL, nCore = 1, do.sentence = FALSE) {

  x <- sento_corpus(data.table::data.table("id" = unname(unlist(NLP::meta(x, "id"))),
                                           "date" = as.POSIXct(do.call("c", NLP::meta(x, "datetimestamp"))),
                                           "texts" = as.character(x$content)))
  compute_sentiment(x, lexicons, how, tokens, nCore, do.sentence)
}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.VCorpus <- compiler::cmpfun(.compute_sentiment.VCorpus) ### TODO: document option in main function

.compute_sentiment.SimpleCorpus <- function(x, lexicons, how, tokens = NULL, nCore = 1, do.sentence = FALSE) {
  # only language available in metadata so no transformation to sentocorpus
  nCore <- check_nCore(nCore)
  texts <- as.character(x$content)
  if (!do.sentence) {
    tok <- tokenize_texts(texts, tokens)
    s <- compute_sentiment_lexicons(tok, lexicons, how, nCore) # compute sentiment per document for all lexicons
  } else {
    sentences <- tokenize_texts(texts, type = "sentence")
    sentencesSplit <- unlist(sentences)
    tok <- tokenize_texts(sentencesSplit, tokens)
    s <- compute_sentiment_by_sentences(NULL, lexicons, how, tok, nCore)
  }
  s
}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.SimpleCorpus <- compiler::cmpfun(.compute_sentiment.SimpleCorpus) ### TODO: document option in main function

### TODO: check if functions applied to "sentiment" objects still function when coming from sentence calculation
compute_sentiment_by_sentences <- function(x, lexicons, how, tokens = NULL, nCore = 1) {
  count <- text_id <- sentence_id <- NULL
  if (!(how %in% get_hows()[["words"]]))
    stop("Please select an appropriate aggregation 'how'.")
  if (length(nCore) != 1 || !is.numeric(nCore))
    stop("The 'nCore' argument should be a numeric vector of size one.")
  if (!is.null(tokens)) stopifnot(is.list(tokens))
  if (!is_names_correct(names(lexicons)))
    stop("At least one lexicon's name contains '-'. Please provide proper names.")
  if (!is.null(lexicons[["valence"]])) {
    if (is.null(lexicons[["valence"]][["t"]])) {
      stop("Column x and t expected for valence shifters.")
    }
  }
  check_class(lexicons, "sentolexicons")

  threads <- min(RcppParallel::defaultNumThreads(), nCore)
  RcppParallel::setThreadOptions(numThreads = threads)

  if (is.null(tokens)) {
    dv <- quanteda::docvars(x)
    sentences <- tokenize_texts(quanteda::texts(x), type = "sentence")
    sentencesSplit <- unlist(sentences)

    # prepare data table with ids for text and sentence
    sentDT <- data.table::data.table(count = lapply(sentences, length))
    if (is.list(sentences)) {
      sentDT <- sentDT[ , "text_id" := .I]
    } else {
      sentDT <- sentDT[ , "text_id" := 1]
    }
    if (dim(dv)[2] == 0)
      sentDT <- cbind(id = quanteda::docnames(x), sentDT)
    else
      sentDT <- cbind(id = quanteda::docnames(x), sentDT, quanteda::docvars(x))
    sentDT <- sentDT[rep(1 : .N, count)][, "count" := NULL]
    sentDT <- sentDT[, "sentence_id" := seq(.N), by = list(cumsum(c(0, abs(diff(text_id)))))][, "text_id" := NULL]

    sentencesSplit <- lapply(sentencesSplit, function(sent) gsub(",", " C_C ", sent))
    tokens <- tokenize_texts(sentencesSplit, tokens = tokens, type = "word")

  } else {
    sentDT <- data.table::data.table(count = lapply(tokens, length))
    sentDT <- sentDT[, sentence_id := .I][, count := NULL]
  }
  ### TODO: check if tokens are expected on sentence- or word-level? + document
  # tokens <- lapply(tokens, function(tok) gsub("c_c", ",", tok))

  hasValenceShifters <- !is.null(lexicons[["valence"]])
  s <- compute_sentiment_sentences(tokens, lexicons, how, hasValenceShifters) # call to C++ code

  lexNames <- colnames(s)[-1]
  s <- cbind(s, sentDT)
  if (!is.null(x)) {
    if (inherits(x, "sentocorpus")) {
      isNumeric <- sapply(dv, is.numeric)
      if (sum(isNumeric) == 0) features <- NULL else features <- names(isNumeric[isNumeric])
      s <- spread_sentiment_features(s, features, lexNames)
      setcolorder(s, c("id", "sentence_id", "date", "word_count"))
    } else if (inherits(x, "corpus")) {
      if (ncol(dv) == 0) {
        features <- NULL
      } else {
        isNumeric <- sapply(dv, is.numeric)
        if (sum(isNumeric) == 0) features <- NULL else features <- names(isNumeric[isNumeric])
      }
      if (!is.null(features)) { # spread sentiment across numeric features if present and reformat
        sent <- spread_sentiment_features(s, features, lexNames)[] # compute feature-sentiment per sentence for all lexicons
      }
      setcolorder(s, c("id", "sentence_id", "word_count"))
    }
  } else {
    setcolorder(s, c( "sentence_id", "word_count"))
  }
  s
}

#' Aggregate sentiment on sentence level into document-level sentiment
#'
#' @author Jeroen Van Pelt, Samuel Borms, Andres Algaba
#'
#' @description Aggregate sentiment on sentence level into document-level sentiment.
#'
#' @details This function aggregates sentiment on sentence level into document-level sentiment. It can then be
#' used in the \code{\link{aggregate.sentiment}} function.
#'
#' @param sentiment A \code{sentiment} object created with \code{\link{compute_sentiment}} and
#' \code{do.sentence = TRUE}.
#' @param how a single \code{character} vector defining how aggregation from sentence to document should be
#' performed. For currently available options on how aggregation can occur, see \code{\link{get_hows}()$docs}.
#' @param alphaExp a single \code{integer} vector as the weight smoothing factor, used if
#' \code{how == "exponential"} or \code{how == "inverseExponential"}. Value should be between 0 and 1
#' (both excluded); see \code{\link{weights_exponential}}.
#' @param do.ignoreZeros a \code{logical} indicating whether zero sentiment values have to be ignored in the
#' determination of the sentence weights while aggregating across sentences. By default
#' \code{do.ignoreZeros = TRUE}, such that sentences with a raw sentiment score of zero or for which a given
#' feature indicator is equal to zero are considered irrelevant.
#'
#' @return \code{sentiment} object with a \code{data.table} containing
#' the sentiment scores \code{data.table} with an \code{"id"}, a \code{"date"} and a \code{"word_count"} column,
#' and all lexicon--feature sentiment scores columns. A \code{sentiment} object can be used for aggregation into
#' time series with the \code{\link{aggregate.sentiment}} function.
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' l <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")],
#'                     list_valence_shifters[["en"]][, c("x", "t")])
#'
#' # compute sentence-level sentiment
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 200)
#' sent <- compute_sentiment(corpusSample, l, how = "squareRootCounts", do.sentence = TRUE)
#'
#' # aggregate into document-level sentiment
#' sentAggDocs <- aggregate_sentences(sent, how = "proportional")
#'
#' # further optional aggregation into time series
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "month", lag = 4)
#' sentomeasures <- aggregate(sentAggDocs, ctr)
#'
#' @export
aggregate_sentences <- function(sentiment, how = "proportional", do.ignoreZeros = TRUE, alphaExp = 0.1) {
  check_class(sentiment, "sentiment")
  if (!("sentence_id" %in% colnames(sentiment))) {
    stop("The input object 'sentiment' should contain the 'sentence_id' column.")
  }
  if (how %in% c("exponential", "inverseExponential")) {
    if(alphaExp >= 1 || alphaExp <= 0) {
      stop("The 'alphaExp' argument must be a number between 0 and 1 (both excluded).")
    }
  }
  if(!how %in% get_hows()$docs) {
    stop( paste0(how, " is no current option for aggregation across sentences."))
  }
  wc <- sentiment[, .(word_count)]
  dates <- sentiment[, .(date)]
  weights <-  aggregate_across(sentiment, how = how, do.ignoreZeros = do.ignoreZeros, by = "id")
  sw <- data.table(id = sentiment$id, sentiment[, -1:-4] * weights, wc, dates)
  s <- sw[, lapply(.SD, function(x) sum(x, na.rm = TRUE)), by = eval(c("date", "id"))]
  setcolorder(s, c("id", "date", "word_count"))
  class(s) <- c("sentiment", class(s))
  s
}

#' Bind sentiment objects row-wise
#'
#' @author Samuel Borms
#'
#' @description Combines multiple sentiment objects with the same column names into a new sentiment object. Duplicates
#' in terms of document identifiers across input objects are removed.
#'
#' @param ... \code{sentiment} objects to combine in the order given.
#'
#' @return A new, larger, \code{sentiment} object.
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' l <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")])
#'
#' corp1 <- sento_corpus(corpusdf = usnews[1:200, ])
#' corp2 <- sento_corpus(corpusdf = usnews[201:450, ])
#' corp3 <- sento_corpus(corpusdf = usnews[401:700, ])
#'
#' sent1 <- compute_sentiment(corp1, l, how = "proportionalPol")
#' sent2 <- compute_sentiment(corp2, l, how = "counts")
#' sent3 <- compute_sentiment(corp3, l, how = "proportional")
#'
#' sent <- sentiment_bind(sent1, sent2, sent3)
#' nrow(sent) # 700
#'
#' @export
sentiment_bind <- function(...) {
  all <- list(...)
  if (!all(sapply(all, inherits, "sentiment")))
    stop("Not all inputs are sentiment objects.")
  if (!length(unique(sapply(all, ncol))) == 1)
    stop("Column dimensions of sentiment objects are not all the same.")
  if (!all(duplicated(t(sapply(all, colnames)))[2:length(all)]))
    stop("Column names of sentiment objects are not all the same.")
  s <- data.table::rbindlist(all)[order(date)]
  s <- unique(s, by = "id")
  class(s) <- c("sentiment", class(s))
  s
}

#' Convert a sentiment table to a sentiment object
#'
#' @author Samuel Borms
#'
#' @description Converts a properly structured sentiment table into a \code{sentiment} object, that can be used
#' for further aggregation with the \code{\link{aggregate.sentiment}} function. This allows to start from document-level
#' sentiment scores not necessarily computed with \code{\link{compute_sentiment}}.
#'
#' @param s a \code{data.table} that can be converted into a \code{sentiment} object. It should have an \code{"id"},
#' a \code{"date"} and a \code{"word_count"} column. If other column names are provided with a separating \code{"--"},
#' the first part is considered the lexicon (or more generally, the sentiment computation method), and the second part
#' the feature. For sentiment column names without any \code{"--"}, a \code{"dummyFeature"} component is added.
#'
#' @return A \code{sentiment} object.
#'
#' @examples
#' set.seed(505)
#'
#' ids <- paste0("id", 1:200)
#' date <- sample(seq(as.Date("2015-01-01"), as.Date("2018-01-01"), by = "day"), 200, TRUE)
#' word_count <- sample(150:850, 200, replace = TRUE)
#' sent <- matrix(rnorm(200 * 8), nrow =  200)
#' s1 <- s2 <- s3 <- data.table(id = ids, date = date, word_count = word_count, sent)
#' m <- "method"
#'
#' colnames(s1)[-c(1:3)] <- paste0(m, 1:8)
#' sent1 <- to_sentiment(s1)
#'
#' colnames(s2)[-c(1:3)] <- c(paste0(m, 1:4, "--", "feat1"), paste0(m, 1:4, "--", "feat2"))
#' sent2 <- to_sentiment(s2)
#'
#' colnames(s3)[-c(1:3)] <- c(paste0(m, 1:3, "--", "feat1"), paste0(m, 1:3, "--", "feat2"),
#'                            paste0(m, 4:5))
#' sent3 <- to_sentiment(s3)
#'
#' # further aggregation from then on is easy
#' sentMeas1 <- aggregate(sent1, ctr_agg(lag = 10))
#'
#' @export
to_sentiment <- function(s) {
  stopifnot(is.data.table(s))
  colNames <- colnames(s)
  if (any(duplicated(colNames)))
    stop("No duplicated column names allowed.")
  if (!all(colNames[1:3] == c("id", "date", "word_count")))
    stop("The input object 's' should have an 'id', a 'date' and a 'word_count' column, in that order.")
  if (!inherits(s[["id"]], "character"))
    stop("The 'id' column should be of type character.")
  if (!inherits(s[["date"]], "Date"))
    stop("The 'date' column should be of type Date.")
  if (!all(sapply(4:ncol(s), function(i) inherits(s[[i]], "numeric"))))
    stop("All sentiment value columns should be of type numeric.")
  newNames <- sapply(stringi::stri_split(colNames[4:ncol(s)], regex = "--"), function(name) {
    if (length(name) == 1) return(paste0(name, "--", "dummyFeature"))
    else if (length(name) >= 2) return(paste0(name[1], "--", name[2]))
  })
  setnames(s, c("id", "date", "word_count", newNames))
  s <- s[order(date)]
  class(s) <- c("sentiment", class(s))
  s
}

#' Extract documents related to sentiment peaks
#'
#' @author Samuel Borms
#'
#' @description This function extracts the documents with most extreme sentiment (lowest, highest or both
#' in absolute terms). The extracted documents are unique, even when, for example, all most extreme
#' sentiment values (across sentiment calculation methods) occur only for one document.
#'
#' @param sentiment a \code{sentiment} object created using \code{\link{compute_sentiment}} or
#' \code{\link{to_sentiment}}.
#' @param n a positive \code{numeric} value to indicate the number of dates associated to sentiment peaks to extract.
#' If \code{n < 1}, it is interpreted as a quantile (for example, 0.07 would mean the 7\% most extreme dates).
#' @param type a \code{character} value, either \code{"pos"}, \code{"neg"} or \code{"both"}, respectively to look
#' for the \code{n} dates related to the most positive, most negative or most extreme (in absolute terms) sentiment
#' occurrences.
#' @param do.average a \code{logical} to indicate whether peaks should be selected based on the average sentiment
#' value per date.
#'
#' @return A vector of type \code{"character"} corresponding to the \code{n} extracted document identifiers.
#'
#' @examples
#' set.seed(505)
#'
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' l <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")])
#'
# compute sentiment to begin with
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 200)
#' sent <- compute_sentiment(corpusSample, l, how = "proportionalPol")
#'
#' # extract the peaks
#' peaksAbs <- peakdocs(sent, n = 5)
#' peaksAbsQuantile <- peakdocs(sent, n = 0.50)
#' peaksPos <- peakdocs(sent, n = 5, type = "pos")
#' peaksNeg <- peakdocs(sent, n = 5, type = "neg")
#'
#' @export
peakdocs <- function(sentiment, n = 10, type = "both", do.average = FALSE) {
  check_class(sentiment, "sentiment")
  stopifnot(n > 0)
  stopifnot(type %in% c("both", "neg", "pos"))

  nMax <- nrow(sentiment)
  if (n < 1) n <- n * nMax
  n <- floor(n)
  if (n >= nMax) stop("The 'n' argument asks for too many documents.")

  s <- sentiment[, -c(1:3)] # drop id, date and word_count columns
  m <- ncol(s)
  if (do.average == TRUE) {
    s <- rowMeans(s, na.rm = TRUE)
    ids <- sentiment[["id"]]
  } else ids <- rep(sentiment[["id"]], m)
  if (type == "both") s <- abs(s)
  indx <- order(s, decreasing = ifelse(type == "neg", FALSE, TRUE))[1:(m * n)]
  peakIds <- unique(ids[indx])[1:n]
  peakIds
}

