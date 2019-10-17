
spread_sentiment_features <- function(s, features, lexNames) {
  for (lexicon in lexNames) { # multiply lexicons with features to obtain feature-sentiment scores per lexicon
    nms <- paste0(lexicon, "--", features)
    s[, nms] <- s[[lexicon]] * s[, features, with = FALSE]
  }
  s[, eval(c(lexNames, features)) := NULL] # remove since replaced by lexicon--feature columns
  s[]
}

tokenize_texts <- function(x, tokens = NULL, type = "word") { # x embeds a character vector
  if (is.null(tokens)) {
    if (type == "word") {
      tok <- stringi::stri_split_boundaries(
        stringi::stri_trans_tolower(x),
        type = "word", skip_word_none = TRUE, skip_word_number = TRUE
      )
    } else if (type == "sentence") {
      sentences <- stringi::stri_split_boundaries(x, type = "sentence")
      tok <- lapply(sentences, function(sn) { # list of documents of list of sentences of words
        wo <- stringi::stri_split_boundaries(
          stringi::stri_trans_tolower(gsub(", ", " c_c ", sn)),
          type = "word", skip_word_none = TRUE, skip_word_number = TRUE
        )
        wo[sapply(wo, length) != 0]
      })
    }
  } else tok <- tokens
  tok
}

compute_sentiment_lexicons <- function(x, tokens, dv, lexicons, how, do.sentence = FALSE, nCore = 1) {
  threads <- min(RcppParallel::defaultNumThreads(), nCore)
  RcppParallel::setThreadOptions(numThreads = threads)
  if (is.character(x)) x <- quanteda::corpus(x)
  if (do.sentence == TRUE) {
    tokens <- tokenize_texts(quanteda::texts(x), tokens, type = "sentence")
    valenceType <- ifelse(is.null(lexicons[["valence"]]), 0,
                          ifelse(colnames(lexicons[["valence"]])[2] == "y", 1, 2))
    s <- compute_sentiment_sentences(unlist(tokens, recursive = FALSE),
                                     lexicons, how, valenceType) # call to C++ code
    dt <- data.table::data.table("id" = quanteda::docnames(x), "n" = sapply(tokens, length))
    if (!is.null(dv)) dt <- cbind(dt, dv)
    dt <- dt[rep(1:.N, n)][, "n" := NULL]
    dt[, "sentence_id" := seq(.N), by = id]
    s <- cbind(dt, s)
    if (inherits(x, "sento_corpus")) {
      data.table::setcolorder(s, c("id", "sentence_id", "date", "word_count"))
    } else {
      data.table::setcolorder(s, c("id", "sentence_id", "word_count"))
    }
  } else {
    tokens <- tokenize_texts(quanteda::texts(x), tokens, type = "word")
    if (is.null(lexicons[["valence"]])) { # call to C++ code
      s <- compute_sentiment_onegrams(tokens, lexicons, how)
    } else {
      # if (4 %in% unique(lexicons$valence[["t"]]))
      #   warning("Valence shifters of type 4 are only used for a sentiment calculation by sentence.")
      s <- compute_sentiment_valence(tokens, lexicons, how)
    }
    s <- data.table::data.table("id" = quanteda::docnames(x), s)
    if (!is.null(dv)) s <- cbind(s, dv)
    if (inherits(x, "sento_corpus")) data.table::setcolorder(s, c("id", "date", "word_count"))
  }
  s
}

compute_sentiment_multiple_languages <- function(x, lexicons, languages, features, how,
                                                 tokens = NULL, do.sentence = FALSE, nCore = 1) {
  ids <- quanteda::docnames(x) # original ids to keep same order in output

  # split corpus by language
  dvl <- quanteda::docvars(x, field = "language")
  idxs <- lapply(languages, function(l) which(dvl == l))
  names(idxs) <- languages

  # compute sentiment for each language subcorpus
  sentByLang <- stats::setNames(as.list(languages), languages)
  for (l in languages) {
    corpus <- quanteda::corpus_subset(x, language == l)
    quanteda::docvars(corpus, field = "language") <- NULL
    if (!(l %in% names(lexicons))) {
      stop(paste0("No lexicon found for language: ", l))
    }
    s <- compute_sentiment(corpus, lexicons[[l]], how, tokens[idxs[[l]]], do.sentence, nCore)
    sentByLang[[l]] <- s
  }

  # merge subsets of sentiment
  s <- do.call(merge, sentByLang)

  s[order(match(id, ids))]
}

#' Compute textual sentiment across features and lexicons
#'
#' @author Samuel Borms, Jeroen Van Pelt, Andres Algaba
#'
#' @description Given a corpus of texts, computes sentiment per document or sentence using the valence shifting
#' augmented bag-of-words approach, based on the lexicons provided and a choice of aggregation across words.
#'
#' @details For a separate calculation of positive (resp. negative) sentiment, provide distinct positive (resp.
#' negative) lexicons (see the \code{do.split} option in the \code{\link{sento_lexicons}} function). All \code{NA}s
#' are converted to 0, under the assumption that this is equivalent to no sentiment. Per default \code{tokens = NULL},
#' meaning the corpus is internally tokenized as unigrams, with punctuation and numbers but not stopwords removed.
#' All tokens are converted to lowercase, in line with what the \code{\link{sento_lexicons}} function does for the
#' lexicons and valence shifters. Word counts are based on that same tokenization.
#'
#' @section Calculation:
#' If the \code{lexicons} argument has no \code{"valence"} element, the sentiment computed corresponds to simple unigram
#' matching with the lexicons [\emph{unigrams} approach]. If valence shifters are included in \code{lexicons} with a
#' corresponding \code{"y"} column, the polarity of a word detected from a lexicon gets multiplied with the associated
#' value of a valence shifter if it appears right before the detected word (examples: not good or can't defend) [\emph{bigrams} approach]. If the valence
#' table contains a \code{"t"} column, valence shifters are searched for in a cluster centered around a detected polarity
#' word [\emph{clusters} approach]. The latter approach is a simplified version of the one utilized by the \pkg{sentimentr}
#' package. A cluster amounts to four words before and two words after a polarity word. A cluster never overlaps with
#' a preceding one. Roughly speaking, the polarity of a cluster is calculated as \eqn{n(1 + 0.80d)S + \sum s}. The polarity
#' score of the detected word is \eqn{S}, \eqn{s} represents polarities of eventual other sentiment words, and \eqn{d} is
#' the difference between the number of amplifiers (\code{t = 2}) and the number of deamplifiers (\code{t = 3}). If there
#' is an odd number of negators (\code{t = 1}), \eqn{n = -1} and amplifiers are counted as deamplifiers, else \eqn{n = 1}.
#'
#' The sentence-level sentiment calculation approaches each sentence as if it is a document. Depending on the input either
#' the unigrams, bigrams or clusters approach is used. We enhanced latter approach following more closely the default
#' \pkg{sentimentr} settings. They use a cluster of five words before and two words after a polarized word. The cluster
#' is limited to the words after a previous comma and before a next comma. Adversative conjunctions (\code{t = 4}) are
#' accounted for here. The cluster is reweighted based on the value \eqn{1 + 0.25adv}, where \eqn{adv} is the difference
#' between the number of adversative conjunctions found before and after the polarized word.
#'
#' @param x either a \code{sento_corpus} object created with \code{\link{sento_corpus}}, a \pkg{quanteda}
#' \code{\link[quanteda]{corpus}} object, a \pkg{tm} \code{\link[tm]{SimpleCorpus}} object, a \pkg{tm}
#' \code{\link[tm]{VCorpus}} object, or a \code{character} vector. Only a \code{sento_corpus} object incorporates
#' a date dimension. In case of a \code{\link[quanteda]{corpus}} object, the \code{numeric} columns from the
#' \code{\link[quanteda]{docvars}} are considered as features over which sentiment will be computed. In
#' case of a \code{character} vector, sentiment is only computed across lexicons.
#' @param lexicons a \code{sento_lexicons} object created using \code{\link{sento_lexicons}}.
#' @param how a single \code{character} vector defining how to perform aggregation within
#' documents or sentences. For available options, see \code{\link{get_hows}()$words}.
#' @param tokens a \code{list} of tokenized documents, or if \code{do.sentence = TRUE} a \code{list} of
#' a \code{list} of tokenized sentences. This allows to specify your own tokenization scheme. Can result from the
#' \pkg{quanteda}'s \code{\link[quanteda]{tokens}} function, the \pkg{tokenizers} package, or other. Make sure the tokens are
#' constructed from (the texts from) the \code{x} argument, are unigrams, and preferably set to lowercase, otherwise, results
#' may be spurious and errors could occur. By default set to \code{NULL}.
#' @param nCore a positive \code{numeric} that will be passed on to the \code{numThreads} argument of the
#' \code{\link[RcppParallel]{setThreadOptions}} function, to parallelize the sentiment computation across texts. A
#' value of 1 (default) implies no parallelization. Parallelization may improve speed of the sentiment
#' computation only for sufficiently large corpora.
#' @param do.sentence a \code{logical} to indicate whether the sentiment computation should be done on
#' sentence-level rather than document-level. By default \code{do.sentence = FALSE}. The methodology defined
#' in the \pkg{sentimentr} package is followed to carry out the computation.
#'
#' @return If \code{x} is a \code{sento_corpus} object: a \code{sentiment} object, i.e., a \code{data.table} containing
#' the sentiment scores \code{data.table} with an \code{"id"}, a \code{"date"} and a \code{"word_count"} column,
#' and all lexicon-feature sentiment scores columns. If \code{do.sentence = TRUE}, an additional
#' \code{"sentence_id"} column along the \code{"id"} column is added. The tokenized sentences are not
#' provided but can be obtained as \code{stringi::stri_split_boundaries(texts, type = "sentence")}.
#' A \code{sentiment} object can be aggregated (into time series) with the \code{\link{aggregate.sentiment}}
#' function.
#'
#' @return If \code{x} is a \pkg{quanteda} \code{\link[quanteda]{corpus}} object: a sentiment scores
#' \code{data.table} with an \code{"id"} and a \code{"word_count"} column, and all lexicon-feature
#' sentiment scores columns.
#'
#' @return If \code{x} is a \pkg{tm} \code{SimpleCorpus} object, a \pkg{tm} \code{VCorpus} object, or a \code{character}
#' vector: a sentiment scores \code{data.table} with an auto-created \code{"id"} column, a \code{"word_count"}
#' column, and all lexicon sentiment scores columns.
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' txt <- system.file("texts", "txt", package = "tm")
#' reuters <- system.file("texts", "crude", package = "tm")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' l1 <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")])
#' l2 <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")],
#'                      list_valence_shifters[["en"]])
#' l3 <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")],
#'                      list_valence_shifters[["en"]][, c("x", "t")])
#'
#' # from a sento_corpus object - unigrams approach
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpusSample <- quanteda::corpus_sample(corpus, size = 200)
#' sent1 <- compute_sentiment(corpusSample, l1, how = "proportionalPol")
#'
#' # from a character vector - bigrams approach
#' sent2 <- compute_sentiment(usnews[["texts"]][1:200], l2, how = "counts")
#'
#' # from a corpus object - clusters approach
#' corpusQ <- quanteda::corpus(usnews, text_field = "texts")
#' corpusQSample <- quanteda::corpus_sample(corpusQ, size = 200)
#' sent3 <- compute_sentiment(corpusQSample, l3, how = "counts")
#'
#' # from an already tokenized corpus - using the 'tokens' argument
#' toks <- as.list(quanteda::tokens(corpusQSample, what = "fastestword"))
#' sent4 <- compute_sentiment(corpusQSample, l1[1], how = "counts", tokens = toks)
#'
#' # from a SimpleCorpus object - unigrams approach
#' scorp <- tm::SimpleCorpus(tm::DirSource(txt))
#' sent5 <- compute_sentiment(scorp, l1, how = "proportional")
#'
#' # from a VCorpus object - unigrams approach
#' ## in contrast to what as.sento_corpus(vcorp) would do, the
#' ## sentiment calculator handles multiple character vectors within
#' ## a single corpus element as separate documents
#' vcorp <- tm::VCorpus(tm::DirSource(reuters))
#' sent6 <- compute_sentiment(vcorp, l1)
#'
#' # from a sento_corpus object - unigrams approach with tf-idf weighting
#' sent7 <- compute_sentiment(corpusSample, l1, how = "TFIDF")
#'
#' # sentence-by-sentence computation
#' sent8 <- compute_sentiment(corpusSample, l1, how = "proportionalSquareRoot",
#'                            do.sentence = TRUE)
#'
#' # from an artificially constructed multilingual corpus
#' usnews[["language"]] <- "en" # add language column
#' usnews$language[1:100] <- "fr"
#' lEn <- sento_lexicons(list("FEEL_en" = list_lexicons$FEEL_en_tr))
#' lFr <- sento_lexicons(list("FEEL_fr" = list_lexicons$FEEL_fr))
#' lexicons <- list(en = lEn, fr = lFr)
#' corpusLang <- sento_corpus(corpusdf = usnews[1:250, ])
#' sent9 <- compute_sentiment(corpusLang, lexicons, how = "proportional")
#'
#' @importFrom compiler cmpfun
#' @export
compute_sentiment <- function(x, lexicons, how = "proportional", tokens = NULL, do.sentence = FALSE, nCore = 1) {
  if (!(how %in% get_hows()[["words"]]))
    stop("Please select an appropriate aggregation 'how'.")
  if (length(nCore) != 1 || !is.numeric(nCore))
    stop("The 'nCore' argument should be a numeric vector of size one.")
  if (!is.null(tokens)) stopifnot(is.list(tokens))
  if (!is.null(tokens) && do.sentence == TRUE) stopifnot(all(sapply(tokens, is.list)))
  if (!inherits(lexicons, "sento_lexicons")) { # if a list
    for (lex in lexicons) {
      check_class(lex, "sento_lexicons")
      if (!is_names_correct(names(lex)))
        stop("At least one lexicon's name contains '-'. Please provide proper names.")
    }
  } else {
    check_class(lexicons, "sento_lexicons")
    if (!is_names_correct(names(lexicons)))
      stop("At least one lexicon's name contains '-'. Please provide proper names.")
  }
  if (!inherits(lexicons, "sento_lexicons")) {
    # only a sento_corpus can have a language column and deal with a list of sento_lexicons
    if (!("sento_corpus" %in% class(x))) {
      stop("List of multiple lexicons only allowed in combination with a sento_corpus.")
    } else {
      if ("language" %in% names(quanteda::docvars(x))) {
        # if language is in sento_corpus, each language needs to be covered by the list of sento_lexicons objects
        if (!all(unique(quanteda::docvars(x, field = "language")) %in% names(lexicons))) {
          stop("Lexicons do not cover all languages in corpus.")
        }
        nms <- c(names(lexicons), sapply(lexicons, names))
        if (sum(duplicated(nms)) > 0) { # check for duplicated lexicon names
          duplics <- unique(nms[duplicated(nms)])
          stop(paste0("Names of lexicons within and/or across languages are not unique. ",
                      "Following names occur at least twice: ", paste0(duplics, collapse = ", "), "."))
        }
      } else { # if language not in sento_corpus, one sento_lexicons object is expected and not a list
        stop("List of sento_lexicons objects only allowed if there is a language column in sento_corpus.")
      }
    }
  } else {
    if (inherits(x, "sento_corpus")) {
      if ("language" %in% names(quanteda::docvars(x)))
        stop("Provide a list of sento_lexicons objects when having a language column in sento_corpus.")
    }
  }

  UseMethod("compute_sentiment", x)
}

.compute_sentiment.sento_corpus <- function(x, lexicons, how = "proportional", tokens = NULL, do.sentence = FALSE, nCore = 1) {
  nCore <- check_nCore(nCore)

  languages <- tryCatch(unique(quanteda::docvars(x, field = "language")), error = function(e) NULL)
  if (is.null(languages)) {
    features <- names(quanteda::docvars(x))[-1] # drop date column
    dv <- data.table::as.data.table(quanteda::docvars(x)[c("date", features)])
    lexNames <- names(lexicons)[names(lexicons) != "valence"]
    s <- compute_sentiment_lexicons(x, tokens, dv, lexicons, how, do.sentence, nCore)
    s <- spread_sentiment_features(s, features, lexNames) # there is always at least one feature
  } else {
    features <- names(quanteda::docvars(x))[-c(1:2)] # drop date and language column
    s <- compute_sentiment_multiple_languages(x, lexicons, languages, features, how, tokens, do.sentence, nCore)
  }

  s <- s[order(date)] # order by date
  class(s) <- c("sentiment", class(s))
  s
}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.sento_corpus <- compiler::cmpfun(.compute_sentiment.sento_corpus)

.compute_sentiment.corpus <- function(x, lexicons, how = "proportional", tokens = NULL, do.sentence = FALSE, nCore = 1) {
  nCore <- check_nCore(nCore)

  if (ncol(quanteda::docvars(x)) == 0) {
    features <- dv <- NULL
  } else {
    isNumeric <- sapply(quanteda::docvars(x), is.numeric)
    if (sum(isNumeric) == 0) features <- NULL else features <- names(isNumeric[isNumeric])
    dv <- data.table::as.data.table(quanteda::docvars(x)[features])
  }

  s <- compute_sentiment_lexicons(x, tokens, dv, lexicons, how, do.sentence, nCore)

  if (!is.null(features)) { # spread sentiment across numeric features if present and reformat
    lexNames <- names(lexicons)[names(lexicons) != "valence"]
    s <- spread_sentiment_features(s, features, lexNames)
  }

  s
}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.corpus <- compiler::cmpfun(.compute_sentiment.corpus)

.compute_sentiment.character <- function(x, lexicons, how = "proportional", tokens = NULL, do.sentence = FALSE, nCore = 1) {
  nCore <- check_nCore(nCore)
  s <- compute_sentiment_lexicons(x, tokens, dv = NULL, lexicons, how, do.sentence, nCore)

  s
}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.character <- compiler::cmpfun(.compute_sentiment.character)

.compute_sentiment.VCorpus <- function(x, lexicons, how = "proportional", tokens = NULL, do.sentence = FALSE, nCore = 1) {
  compute_sentiment(unlist(lapply(x, "[[", "content")),
                    lexicons, how, tokens, do.sentence, nCore)
}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.VCorpus <- compiler::cmpfun(.compute_sentiment.VCorpus)

.compute_sentiment.SimpleCorpus <- function(x, lexicons, how = "proportional", tokens = NULL, do.sentence = FALSE, nCore = 1) {
  compute_sentiment(as.character(as.list(x)),
                    lexicons, how, tokens, do.sentence, nCore)
}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.SimpleCorpus <- compiler::cmpfun(.compute_sentiment.SimpleCorpus)

#' Merge sentiment objects horizontally and/or vertically
#'
#' @author Samuel Borms
#'
#' @description Combines multiple \code{sentiment} objects with possibly different column names
#' into a new \code{sentiment} object. Here, too, any resulting \code{NA} values are converted to zero.
#'
#' @param ... \code{sentiment} objects to merge.
#'
#' @return The new, combined, \code{sentiment} object, ordered by \code{"date"} and \code{"id"}.
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' l1 <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")])
#' l2 <- sento_lexicons(list_lexicons[c("FEEL_en_tr")])
#' l3 <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en", "FEEL_en_tr")])
#'
#' corp1 <- sento_corpus(corpusdf = usnews[1:200, ])
#' corp2 <- sento_corpus(corpusdf = usnews[201:450, ])
#' corp3 <- sento_corpus(corpusdf = usnews[401:700, ])
#'
#' s1 <- compute_sentiment(corp1, l1, "proportionalPol")
#' s2 <- compute_sentiment(corp2, l1, "counts")
#' s3 <- compute_sentiment(corp3, l1, "counts")
#' s4 <- compute_sentiment(corp2, l1, "counts", do.sentence = TRUE)
#' s5 <- compute_sentiment(corp3, l2, "proportional", do.sentence = TRUE)
#' s6 <- compute_sentiment(corp3, l1, "counts", do.sentence = TRUE)
#' s7 <- compute_sentiment(corp3, l3, "UShaped", do.sentence = TRUE)
#'
#' # straightforward row-wise merge
#' m1 <- merge(s1, s2, s3)
#' nrow(m1) == 700 # TRUE
#'
#' # another straightforward row-wise merge
#' m2 <- merge(s4, s6)
#'
#' # merge of sentence and non-sentence calculations
#' m3 <- merge(s3, s6)
#'
#' # different methods adds columns
#' m4 <- merge(s4, s5)
#' nrow(m4) == nrow(m2) # TRUE
#'
#' # different methods and weighting add rows and columns
#' ## rows are added only when the different weighting
#' ## approach for a specific method gives other sentiment values
#' m5 <- merge(s4, s7)
#' nrow(m5) > nrow(m4) # TRUE
#'
#' @export
merge.sentiment <- function(...) {
  inp <- list(...)
  if (!all(sapply(inp, inherits, "sentiment")))
    stop("Not all inputs are sentiment objects.")

  # cols <- sect <- unique(do.call(c, lapply(inp, colnames)))
  # for (dt in inp) {
  #   sect <- intersect(sect, colnames(dt))
  # }
  # dts <- lapply(inp, function(dt) {
  #   dt <- data.table::data.table(dt)
  #   # newCols <- setdiff(cols, colnames(dt))
  #   # if (length(newCols) > 0) dt[, setdiff(cols, colnames(dt)) := as.numeric(NA)]
  #   dt[]
  # })
  # s <- Reduce(function(...) merge(..., by = sect, all = TRUE), dts)[order(date, id)]

  s <- data.table::as.data.table(inp[[1]])
  for (j in 2:length(inp)) {
    dt <- data.table::data.table(inp[[j]]) # no change by reference
    s <- merge(s, dt, by = intersect(colnames(s), colnames(dt)), all = TRUE)
  }

  if ("sentence_id" %in% colnames(s)) {
    s[, "sentence_id" := ifelse(is.na(sentence_id), 1, sentence_id)]
    data.table::setcolorder(s, c("id", "sentence_id", "date"))
    s <- s[order(date, id, sentence_id)]
  } else {
    data.table::setcolorder(s, c("id", "date"))
    s <- s[order(date, id)]
  }
  for (j in seq_along(s)) data.table::set(s, i = which(is.na(s[[j]])), j = j, value = 0)

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
#' \code{\link{as.sentiment}}.
#' @param n a positive \code{numeric} value to indicate the number of documents associated to sentiment
#' peaks to extract. If \code{n < 1}, it is interpreted as a quantile (for example, 0.07 would mean the
#' 7\% most extreme documents).
#' @param type a \code{character} value, either \code{"pos"}, \code{"neg"} or \code{"both"}, respectively to look
#' for the \code{n} documents related to the most positive, most negative or most extreme (in absolute terms) sentiment
#' occurrences.
#' @param do.average a \code{logical} to indicate whether peaks should be selected based on the average sentiment
#' value per document.
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

#' Convert a sentiment table to a sentiment object
#'
#' @author Samuel Borms
#'
#' @description Converts a properly structured sentiment table into a \code{sentiment} object, that can be used
#' for further aggregation with the \code{\link{aggregate.sentiment}} function. This allows to start from
#' sentiment scores not necessarily computed with \code{\link{compute_sentiment}}.
#'
#' @param s a \code{data.table} or \code{data.frame} that can be converted into a \code{sentiment} object. It
#' should have at least an \code{"id"}, a \code{"date"}, a \code{"word_count"} and one sentiment scores column.
#' If other column names are provided with a separating \code{"--"}, the first part is considered the lexicon
#' (or more generally, the sentiment computation method), and the second part the feature. For sentiment column
#' names without any \code{"--"}, a \code{"dummyFeature"} component is added.
#'
#' @return A \code{sentiment} object.
#'
#' @examples
#' set.seed(505)
#'
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#'
#' ids <- paste0("id", 1:200)
#' dates <- sample(seq(as.Date("2015-01-01"), as.Date("2018-01-01"), by = "day"), 200, TRUE)
#' word_count <- sample(150:850, 200, replace = TRUE)
#' sent <- matrix(rnorm(200 * 8), nrow =  200)
#' s1 <- s2 <- data.table::data.table(id = ids, date = dates, word_count = word_count, sent)
#' s3 <- data.frame(id = ids, date = dates, word_count = word_count, sent,
#'                  stringsAsFactors = FALSE)
#' s4 <- compute_sentiment(usnews$texts[201:400],
#'                         sento_lexicons(list_lexicons["GI_en"]),
#'                         "counts", do.sentence = TRUE)
#' m <- "method"
#'
#' colnames(s1)[-c(1:3)] <- paste0(m, 1:8)
#' sent1 <- as.sentiment(s1)
#'
#' colnames(s2)[-c(1:3)] <- c(paste0(m, 1:4, "--", "feat1"), paste0(m, 1:4, "--", "feat2"))
#' sent2 <- as.sentiment(s2)
#'
#' colnames(s3)[-c(1:3)] <- c(paste0(m, 1:3, "--", "feat1"), paste0(m, 1:3, "--", "feat2"),
#'                            paste0(m, 4:5))
#' sent3 <- as.sentiment(s3)
#'
#' s4[, "date" := rep(dates, s4[, max(sentence_id), by = id][[2]])]
#' sent4 <- as.sentiment(s4)
#'
#' # further aggregation from then on is easy...
#' sentMeas1 <- aggregate(sent1, ctr_agg(lag = 10))
#' sent5 <- aggregate(sent4, ctr_agg(howDocs = "proportional"), do.full = FALSE)
#'
#' @export
as.sentiment <- function(s) {
  UseMethod("as.sentiment", s)
}

#' @export
as.sentiment.data.frame <- function(s) {
  as.sentiment(data.table::as.data.table(s))
}

#' @export
as.sentiment.data.table <- function(s) {
  stopifnot(data.table::is.data.table(s))
  colNames <- colnames(s)
  if (any(duplicated(colNames)))
    stop("No duplicated column names allowed.")

  nonSentCols <- c("id", "date", "word_count")
  if ("sentence_id" %in% colNames) nonSentCols <- c(nonSentCols[1], "sentence_id", nonSentCols[2:3])
  if (!all(c("id", "date", "word_count") %in% colNames))
    stop("The input object 's' should have an 'id', a 'date' and a 'word_count' column.")
  if (!inherits(s[["id"]], "character"))
    stop("The 'id' column should be of type character.")
  if (!inherits(s[["date"]], "Date"))
    stop("The 'date' column should be of type Date.")

  sentCols <- colNames[!(colNames %in% nonSentCols)]
  if (length(sentCols) == 0)
    stop("There should be at least one sentiment scores column.")
  if (!all(sapply(sentCols, function(i) is.numeric(s[[i]]))))
    stop("All sentiment value columns should be of type numeric.")
  newNames <- sapply(stringi::stri_split(sentCols, regex = "--"), function(name) {
    if (length(name) == 1) return(paste0(name, "--", "dummyFeature"))
    else if (length(name) >= 2) return(paste0(name[1], "--", name[2]))
  })

  data.table::setnames(s, sentCols, newNames)
  data.table::setcolorder(s, nonSentCols)
  s <- s[order(date)]
  class(s) <- c("sentiment", class(s))
  s
}

