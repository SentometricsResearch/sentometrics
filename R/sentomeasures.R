
#' Set up control for aggregation into sentiment measures
#'
#' @author Samuel Borms, Keven Bluteau
#'
#' @description Sets up control object for aggregation of document-level textual sentiment into textual
#' sentiment measures (indices).
#'
#' @details For currently available options on how aggregation can occer (via the \code{howWithin},
#' \code{howDocs} and \code{howTime} parameters), call \code{get_hows()}.
#'
#' @param howWithin a single \code{character} vector defining how aggregation within documents will be performed. Should
#' \code{length(howWithin) > 1}, the first element is used. For currently available options on how aggregation can occer, see
#' \code{get_hows()$words}.
#' @param howDocs a single \code{character} vector defining how aggregation across documents per date will be performed.
#' Should \code{length(howDocs) > 1}, the first element is used. For currently available options on how aggregation can occer,
#' see \code{get_hows()$docs}.
#' @param howTime a \code{character} vector defining how aggregation across dates will be performed. More than one choice
#' is possible here. For currently available options on how aggregation can occer, see \code{get_hows()$time}.
#' @param do.ignoreZeros a \code{logical} indicating whether zero sentiment values have to be ignored in the determination of
#' the document weights while aggregating across documents.
#' @param by a single \code{character} vector, either \code{"day", "week", "month"} or \code{"year"}, to indicate at what
#' level the dates should be aggregated. Dates will be displayed as the first day of the period, if applicable (e.g.
#' \code{"2017-03-01"} for March 2017).
#' @param lag a single \code{integer} vector, being the time lag to be specified for aggregation across time. By default
#' equal to 1, meaning no aggregation across time.
#' @param fill a single \code{character} vector, one of \code{c("zero", "latest", "none")}, to control how missing
#' sentiment values across the continuum of dates considered are added. This impacts the aggregation across time,
#' applying the \code{fill_measures()} function before aggregating, except if \code{fill == "none"}. By default equal to
#' \code{"zero"}, which sets the scores (and thus also the weights) of the added dates to zero in the time aggregation.
#' @param alphasExp a \code{numeric} vector of all exponential smoothing factors to calculate weights for, used if
#'  \code{"exponential" \%in\% howTime}. Values should be betwoon 0 and 1 (both excluded).
#' @param ordersAlm a \code{numeric} vector of all Almon polynomial orders to calcalute weights for, used if
#' \code{"almon" \%in\% howTime}.
#' @param do.inverseAlm a \code{logical} indicating if for every Almon polynomial its inverse has to be calculated too, used
#' if \code{"almon" \%in\% howTime}.
#' @param do.normalizeAlm a \code{logical} indicating if every Almon polynomial weights column should sum to one, used if
#' \code{"almon" \%in\% howTime}.
#' @param weights an optional own weighting scheme, always used if provided as a \code{data.frame} with the number of rows
#' equal to the desired \code{lag}. The automatic Almon polynomials and exponential weighting functions are created
#' sequentially; if the user wants only specific of such time weighting series it can use \code{almons()} and
#' \code{exponentials()}, select the columns it requires, combine it into a \code{data.frame} and supply it under this
#' argument (see examples).
#' @param dfm optional; an output from a \code{quanteda::dfm()} call, such that users can specify their own tokenization scheme
#' (via \code{quanteda::tokenize()}) as well as other parameters related to the construction of a document-feature matrix
#' (dfm). By default, a dfm is created based on a tokenization that removes punctuation, numbers, symbols and separators. We
#' suggest to stick to unigrams, as the remainder of the sentiment computation (in \code{compute_sentiment()}) and built-in
#' lexicons assume the same.
#'
#' @return A list encapsulating the control parameters.
#'
#' @seealso \code{\link{fill_measures}}, \code{\link{compute_sentiment}}
#'
#' @examples
#' # simple control function
#' ctr1 <- ctr_agg(howTime = "linear", by = "year", lag = 3)
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
#' # set up control function with one linear, two Almon and two exponential weighting schemes
#' a <- almons(n = 70, orders = 1:3, do.inverse = TRUE, do.normalize = TRUE)
#' e <- exponentials(n = 70, alphas = c(0.4, 0.8))
#' ctr3 <- ctr_agg(howTime = c("linear", "own"), by = "year", lag = 70,
#'                 weights = data.frame(a1 = a[, 1], a2 = a[, 3], e1 = e[, 1], e2 = e[, 2]))
#'
#' @export
ctr_agg <- function(howWithin = "proportional", howDocs = "equal_weight", howTime = "equal_weight",
                    do.ignoreZeros = FALSE, by = "day", lag = 1, fill = "zero", alphasExp = seq(0.1, 0.5, by = 0.1),
                    ordersAlm = 1:3, do.inverseAlm = TRUE, do.normalizeAlm = TRUE, weights = NULL, dfm = NULL) {

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
  if (!("own" %in% howTime) & is.data.frame(weights)) {
    howTime <- c(howTime, "own")
    warning(paste0("The option 'own' is added to 'howTime' since a valid (not NULL) 'weights' data.frame was supplied."))
  }
  if ("own" %in% howTime) {
    if (lag != nrow(weights)) {
      lag <- nrow(weights)
      warning("Argument 'lag' is set equal to the number of rows in the 'weights' data.frame.")
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
  if (!(fill %in% c("zero", "latest", "none"))) {
    warning(paste0(fill, " is no current 'fill' option."))
    warned <- warned + 1
  }
  if (!is.null(dfm) & !quanteda::is.dfm(dfm)) {
    warning("The 'dfm' argument should pass quanteda::is.dfm(dfm) when it is not equal to NULL.")
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
              fill = fill,
              other = other)

  return(ctr)
}

#' One-way road towards a sentomeasures object
#'
#' @author Samuel Borms, Keven Bluteau
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
#' \item{sentiment}{the sentiment scores \code{data.table} with a \code{date} and lexicon--feature sentiment scores columns.}
#' \item{howWithin}{a \code{character} vector to remind how sentiment within documents was aggregated.}
#' \item{howDocs}{a \code{character} vector to remind how sentiment across documents was aggregated.}
#' \item{fill}{a single \code{character} vector that specifies if and how missing dates have been added before
#' aggregation across time was carried out.}
#' \item{attribWeights}{a \code{list} of document weights and time weights that are extracted in a call to
#' \code{retrieve_attributions()}. Serves further no direct purpose.}
#'
#' @seealso \code{\link{compute_sentiment}}, \code{\link{perform_agg}}
#'
#' @examples
#' data("usnews")
#' data("lexicons")
#' data("valence")
#'
#' # construct a sentomeasures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
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
#' summary(sentomeasures)
#'
#' @import data.table
#' @export
sento_measures<- function(sentocorpus, lexicons, ctr) {
  check_class(sentocorpus, "sentocorpus")
  toAgg <- compute_sentiment(sentocorpus, lexicons, how = ctr$howWithin)
  sentomeasures <- perform_agg(toAgg, ctr)
  return(sentomeasures)
}

#' @export
summary.sentomeasures <- function(object, ...) {
  sentomeasures <- object
  freq <- c("daily", "weekly", "monthly", "yearly")[c("day", "week", "month", "year") %in% sentomeasures$by]
  cat("This sentomeasures object contains", dim(sentomeasures$measures)[2] - 1, "textual sentiment time series",
      "with", dim(sentomeasures$measures)[1], "observations each,", "at a", freq, "frequency.", "\n")
  cat("The corpus has following features:", sentomeasures$features, "\n")
  cat("\n")
  cat("Following lexicons were used for the sentiment calculation:", sentomeasures$lexicons, "\n")
  cat("To aggregate sentiment within documents, following scheme was applied:", sentomeasures$howWithin, "\n")
  cat("To aggregate sentiment across documents, following scheme was applied:", sentomeasures$howDocs, "\n")
  cat("To aggregate sentiment across time, following schemes were applied:", sentomeasures$time, "\n")
  cat("\n")
  cat("Aggregate statistics:", "\n")
  print(rowMeans(sentomeasures$stats))
}

#' @export
print.sentomeasures <- function(x, ...) {
  sentomeasures <- x
  n <- dim(sentomeasures$measures)[2] - 1
  m <- dim(sentomeasures$measures)[1]
  cat("A sentomeasures object that carries with it", n, "distinct textual sentiment time series of", m, "observations each.")
}

#' Set up lexicons (and valence word list) for use in sentiment analysis
#'
#' @author Samuel Borms
#'
#' @description Structures provided lexicons and potentially integrates valence words. One can also provide (part of) the
#' built-in lexicons from \code{data("lexicons")} or a valence word list from \code{data("valence")} as an argument.
#' Makes use of the \code{as_key()} function from the \pkg{sentimentr} package to make the output coherent and check for
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
#' @return A list with each lexicon as a separate element according to its name, as a \code{data.table}, and a list element
#' named \code{valence} that comprises the valence words. Every \code{x} column contains the words, every \code{y} column
#' contains the polarity score, and for the valence word list, \code{t} contains the word type. If a valence word list is
#' provided, all lexicons are expanded by copying the respective lexicon, and changing the words and scores according to the
#' valence word type: "NOT_" is added for negators, "VERY_" is added for amplifiers and "HARDLY_" is added for deamplifiers.
#' Lexicon scores are multiplied by -1, 2 and 0.5 by default, respectively, or the first value of the scores column of the
#' valence word list.
#'
#' @seealso \code{\link[sentimentr]{as_key}}
#'
#' @examples
#' data("lexicons")
#' data("valence")
#'
#' # sets up output list straight from built-in word lists including valence words
#' l1 <- c(lexicons[c("LM_eng", "HENRY_eng")], valence[["eng"]])
#'
#' # using function, including a self-made lexicon, with and without valence shifters
#' lexIn <- c(list(myLexicon = data.table(w = c("nice", "boring"), s = c(2, -1))),
#'            lexicons[c("GI_eng")])
#' valIn <- valence[["valence_eng"]]
#' l2 <- setup_lexicons(lexIn)
#' l3 <- setup_lexicons(lexIn, valIn)
#' l4 <- setup_lexicons(lexIn, valIn, do.split = TRUE)
#'
#' @export
setup_lexicons <- function(lexiconsIn, valenceIn = NULL, do.split = FALSE) {

  if (!is.list(lexiconsIn))
    stop("The 'lexiconsIn' input should be a list.")
  if (!is.data.frame(valenceIn) & !is.null(valenceIn))
    stop("The 'valenceIn' argument should be a data.table or data.frame if not NULL.")
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
    lexicons <- expand_lexicons(lexicons, types = valTypes, scores = scores)
  }
  # split each lexicon into a positive and a negative polarity words only lexicon
  if (do.split == TRUE) {
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

.compute_sentiment <- function(sentocorpus, lexicons, how = get_hows()$words, dfm = NULL) {
  check_class(sentocorpus, "sentocorpus")

  if (length(how) > 1) how <- how[1]
  if ("valence" %in% names(lexicons)) {
    cat("Modify corpus to account for valence words... ")
    quanteda::texts(sentocorpus) <- include_valence(quanteda::texts(sentocorpus), lexicons[["valence"]])
    cat("Done.", "\n")
  }
  lexNames <- names(lexicons)[names(lexicons) != "valence"]
  features <- names(quanteda::docvars(sentocorpus))[-1] # drop date column

  cat("Compute sentiment... ")
  # frequency-based document-feature matrix (rows are corpus ids, columns are words)
  if (is.null(dfm)) {
      dfm <- quanteda::dfm(quanteda::tokenize(sentocorpus, remove_punct = TRUE, remove_numbers = TRUE,
                                              remove_symbols = TRUE, remove_separators = TRUE), verbose = FALSE)
  } else if (!quanteda::is.dfm(dfm))
    stop("The 'dfm' argument should pass quanteda::is.dfm(dfm).")

  if (how == "counts" || how == "proportional") {
    fdm <- quanteda::t(dfm) # feature-document matrix
  } else if (how == "tf-idf") {
      weights <- quanteda::tfidf(dfm, scheme_tf = "prop")
      fdmWeighted <- quanteda::t(weights)
  } else stop("Please select an appropriate aggregation 'how'.")

  s <- as.data.table(matrix(0, nrow = quanteda::ndoc(sentocorpus), ncol = length(lexNames)))
  names(s) <- lexNames
  allWords <- quanteda::featnames(dfm)
  wCounts <- quanteda::rowSums(dfm, na.rm = TRUE)
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
    } else scores <- quanteda::rowSums(quanteda::t(fdmWeighted * allScores))
    scores[is.na(scores)] <- 0 # set NA/NaN sentiment to 0 (e.g. if document contains no words)
    s[, (lexicon) := scores]
  }
  # structure: date - feature1 - ... - word_count - lexicon1 (sentiment) - ...
  s <- as.data.table(cbind(id = quanteda::docnames(sentocorpus), quanteda::docvars(sentocorpus), word_count = wCounts, s))
  # compute feature-sentiment per document for all lexicons and order by date
  sent <- get_features_sentiment(s, features, lexNames)
  sent <- sent[order(date)]
  cat("Done.", "\n")

  sentOut <- list(corpus = sentocorpus, # not the same as input corpus if accounted for valence shifters
                  sentiment = sent,
                  features = features,
                  lexicons = lexNames,
                  howWithin = how)

  return(sentOut)
}

#' Compute document-level sentiment across features and lexicons
#'
#' @author Samuel Borms
#'
#' @description Given a corpus of texts, computes sentiment per document using the bag-of-words approach,
#' based on the lexicons provided and a choice of aggregation across words per document scheme. Relies partly on the
#' \pkg{quanteda} package. The scores computed are net sentiment (sum of positive minus sum of negative scores).
#'
#' @details
#' For a separate calculation of positive (resp. negative) sentiment, one has to provide distinct positive (resp. negative)
#' lexicons. This can be done using the \code{do.split} option in the \code{setup_lexicons()} function, which splits out the
#' lexicons into a positive and a negative polarity counterpart. \code{NA}s are converted to 0, under the assumption that this
#' is equivalent to no sentiment.
#'
#' @param sentocorpus a \code{sentocorpus} object.
#' @param lexicons output from a \code{setup_lexicons()} call.
#' @param how a single \code{character} vector defining how aggregation within documents should be performed. For currently
#' available options on how aggregation can occer, see \code{get_hows()$words}.
#' @param dfm optional; an output from a \code{quanteda::dfm()} call, such that users can specify their own tokenization
#' scheme (via \code{quanteda::tokenize()}) as well as other parameters related to the construction of a document-feature
#' matrix (dfm). By default, a dfm is created based on a tokenization that removes punctuation, numbers, symbols and
#' separators. We suggest to stick to unigrams, as the remainder of the sentiment computation and built-in lexicons assume
#' the same.
#'
#' @return A list containing:
#' \item{corpus}{the supplied \code{sentocorpus} object; the texts are altered if valence shifters are part of the lexicons.}
#' \item{sentiment}{the sentiment scores \code{data.table} with a \code{date} and lexicon--feature sentiment scores columns.}
#' \item{features}{a \code{character} vector of the different features.}
#' \item{lexicons}{a \code{character} vector of the different lexicons used.}
#' \item{howWithin}{a \code{character} vector to remind how sentiment within documents was aggregated.}
#'
#' @seealso \code{\link[quanteda]{dfm}}, \code{\link[quanteda]{tokenize}}
#'
#' @examples
#' data("usnews")
#' data("lexicons")
#' data("valence")
#'
#' # sentiment computation based on raw frequency counts
#' corpus <- sento_corpus(corpusdf = usnews)
#' l <- setup_lexicons(lexicons[c("LM_eng", "HENRY_eng")], valence[["valence_eng"]])
#' sent <- compute_sentiment(corpus, l, how = "counts")
#'
#' # same sentiment computation based on a user-supplied dfm with default settings
#' dfm <- quanteda::dfm(quanteda::tokenize(corpus), verbose = FALSE)
#' sent <- compute_sentiment(corpus, l, how = "counts", dfm = dfm)
#'
#' @export
compute_sentiment <- compiler::cmpfun(.compute_sentiment)

.get_features_sentiment <- function(sent, features, lexNames) {
  for (lexicon in lexNames) { # multiply lexicons with features to obtain feature-sentiment scores per lexicon
    nms <- paste0(lexicon, "--", features)
    sent[, nms] <- sent[[lexicon]] * sent[, features, with = FALSE]
  }
  sent[, eval(c(lexNames, features)) := NULL][] # remove since replaced by lexicon--feature columns
  return(sent)
}
get_features_sentiment <- compiler::cmpfun(.get_features_sentiment)

#' Aggregate textual sentiment across documents and time
#'
#' @author Samuel Borms, Keven Bluteau
#'
#' @description Condense document-level textual sentiment scores into a panel of textual sentiment
#' measures by aggregating across documents and time.
#'
#' @param toAgg output from a \code{compute_sentiment()} call, a list with as main component a sentiment scores
#' \code{data.table} with dates and lexicon--feature sentiment scores columns.
#' @param ctr output from a \code{ctr_agg()} call.
#'
#' @return A \code{sentomeasures} object.
#'
#' @seealso \code{\link{compute_sentiment}}, \code{\link{ctr_agg}}, \code{\link{sento_measures}}
#'
#' @examples
#' data("usnews")
#' data("lexicons")
#' data("valence")
#'
#' # computation of sentiment and aggregation into sentiment measures
#' corpus <- sento_corpus(corpusdf = usnews)
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
  fill <- ctr$fill
  otherVars <- ctr$other # list or empty
  aggDocs <- agg_documents(toAgg, by = by, how = howDocs, do.ignoreZeros = do.ignoreZeros)
  sentomeasures <- agg_time(aggDocs, lag = lag, fill = fill, how = howTime, otherVars)
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
  if (do.ignoreZeros == TRUE)
    sent[, names(sent)] <- sent[, names(sent), with = FALSE][, lapply(.SD, function(x) replace(x, which(x == 0), NA))]

  # aggregate feature-sentiment per document by date for all lexicon columns
  s <- sent[, -1]
  if (how == "equal_weight") {
    attribWeights[["W"]] <- data.table(id = sent$id, s[, w := 1 / .N, by = date][, c("date", "w")])
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
                        fill = NA,
                        attribWeights = attribWeights)

  class(sentomeasures) <- c("sentomeasures")

  return(sentomeasures)
}

agg_time <- function(sentomeasures, lag, fill, how = get_hows()$time, ...) {
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
  if (!(fill %in% "none")) sentomeasures <- fill_measures(sentomeasures, fill = fill)
  measures <- sentomeasures$measures
  toRoll <- measures[, -1]
  n <- nrow(weights)
  m <- nrow(measures)
  if (n > m)
    stop("Rolling time aggregation window (= ", n, ") is too large for number of observations per measure (= ", m, ")")
  for (i in 1:ncol(weights)) {
    name <- colnames(weights)[i]
    add <- RcppRoll::roll_sum(as.matrix(toRoll), n = n, weights = as.vector(weights[, i]),
                              normalize = FALSE, align = "right", na.rm = TRUE)
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
  sentomeasures$fill <- fill

  return(sentomeasures)
}

#' Set up control for merging sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Sets up control object for the optional merging (additional aggregation) of sentiment measures.
#'
#' @param sentomeasures a \code{sentomeasures} object. This is necessary to check whether the other input arguments
#' make sense.
#' @param lex a list with unique lexicons to merge at given name, e.g. \cr
#' \code{list(lex12 = c("lex1", "lex2"))}. See \code{sentomeasures$lexicons} for the exact names to use.
#' @param feat a list with unique features to merge at given name, e.g. \cr
#' \code{list(feat12 = c("feat1", "feat2"))}. See \code{sentomeasures$features} for the exact names to use.
#' @param time a list with unique time weighting schemes to merge at given name, e.g. \cr
#' \code{list(tw12 = c("tw1", "tw2"))}. See \code{sentomeasures$time} for the exact names to use.
#' @param do.keep a \code{logical} indicating if the original sentiment measures should be kept (i.e. the merged
#' sentiment measures will be added to the current sentiment measures as additional indices if \code{TRUE}).
#'
#' @return A list encapsulating the control parameters.
#'
#' @seealso \code{\link{merge_measures}}
#'
#' @examples
#' data("usnews")
#' data("lexicons")
#' data("valence")
#'
#' # construct a sentomeasures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
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

#' Merge sentiment measures
#'
#' @author Samuel Borms
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
#' data("usnews")
#' data("lexicons")
#' data("valence")
#'
#' # construct a sentomeasures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
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

  if (do.keep == TRUE) {
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
  if (do.keep == TRUE) measures <- cbind(measures, measuresOld[, !(namesOld %in% colnames(measures)), with = FALSE])

  sentomeasures <- update_info(sentomeasures, measures) # update information in sentomeasures object

  return(sentomeasures)
}

#' Merge sentiment measures into one global sentiment measure
#'
#' @author Samuel Borms
#'
#' @description Merges all sentiment measures into one global textual sentiment measure based on a set of weights to
#' indicate the importance of each component in the \code{lexicons}, \code{features} and \code{time} vectors as specified
#' in the input \code{sentomeasures} object. The global measure is composed as the multiplication of the individual weights
#' across the three dimensions times the sentiment value per date observation.
#'
#' @details This function returns no \code{sentomeasures} object, however the global sentiment measure as outputted can
#' be added to regressions as an additional variable using the \code{x} argument in the \code{sento_model()} function.
#'
#' @param sentomeasures a \code{sentomeasures} object.
#' @param lex a \code{numeric} vector of weights, of size \code{length(sentomeasures$lexicons)}, in the same order and
#' summing to one. By default set to 1, which means equally weighted.
#' @param feat a \code{numeric} vector of weights, of size \code{length(sentomeasures$features)}, in the same order and
#' summing to one. By default set to 1, which means equally weighted.
#' @param time a \code{numeric} vector of weights, of size \code{length(sentomeasures$time)}, in the same order and summing
#' to one. By default set to 1, which means equally weighted.
#'
#' @return A \code{data.frame} with the values for the global sentiment measure under the \code{global} column and dates as
#' row names.
#'
#' @seealso \code{\link{sento_model}}
#'
#' @examples
#' data("usnews")
#' data("lexicons")
#' data("valence")
#'
#' # construct a sentomeasures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
#' l <- setup_lexicons(lexicons[c("LM_eng", "HENRY_eng")], valence[["valence_eng"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpus, l, ctr)
#'
#' # merge into one global sentiment measure, with specified weighting for lexicons and features
#' global <- to_global(sentomeasures, lex = c(0.40, 0.60),
#'                                    feat = c(0.10, 0.20, 0.30, 0.40),
#'                                    time = 1)
#'
#' @export
to_global <- function(sentomeasures, lex = 1, feat = 1, time = 1) {
  check_class(sentomeasures, "sentomeasures")

  dims <- list(sentomeasures$lexicons, sentomeasures$features, sentomeasures$time)
  n <- sapply(dims, length)
  weightsInp <- list(lex, feat, time)
  weights <- sapply(1:3, function(i) {
    if (length(weightsInp[[i]]) == 1) w <- as.list(rep(1/n[i], n[i])) # modify weights if equal to default value of 1
    else {
      w <- as.list(weightsInp[[i]])
      names(w) <- dims[[i]] # named weight lists
      if (length(w) != n[i] || sum(unlist(w)) != 1)
        stop("All weights must be equal in length to the respective number of components and sum to one.")
    }
    return(w)
  })
  measures <- sentomeasures$measures
  measuresLong <- to_long(measures) # long format
  # extract different weights based on how measuresLong is ordered and add a global weights (w) column
  wLex <- unlist(weights[[1]][measuresLong[["lexicons"]]])
  wFeat <- unlist(weights[[2]][measuresLong[["features"]]])
  wTime <- unlist(weights[[3]][measuresLong[["time"]]])
  # add a global weights column as the multiplication of the individual weights across the three dimensions per row
  measuresLong[, "w" := wLex * wFeat * wTime]
  global <- as.data.frame(measuresLong[, list(global = sum(value * w)), by = date])
  row.names(global) <- global$date
  global$date <- NULL

  return(global)
}

#' Select a subset of sentiment measures
#'
#' @author Samuel Borms
#'
#' @description Selects the subset of sentiment measures which include either all of the given selection components combined,
#' or those who's name consist of at least one of the selection components. Selecting a subset of dates
#'
#' @param sentomeasures a \code{sentomeasures} object.
#' @param toSelect a \code{"character"} vector of the lexicon, feature and time weighting scheme names, to indicate which
#' measures need to be selected. By default equal to \code{"all"}, which means no selection of the sentiment measures is made;
#' this may be used if one only wants to extract a subset of dates via the \code{dates} argument.
#' @param do.combine a \code{logical} indicating if only measures for wich all (\code{TRUE}) or at least one (\code{FALSE}) of
#' the selection components should occur in each sentiment measure's name in the subset. If \code{do.combine == TRUE}, the
#' \code{toSelect} argument can only consist of one lexicon, one feature and one time weighting scheme at maximum.
#' @param dates any expression, in the form of a \code{character} vector, that would correctly evaluate to a \code{logical}
#' vector, features the variable \code{date} and has dates specified as \code{"yyyy-mm-dd"}, e.g.
#' \code{dates = "date >= '2000-01-15'"}. This argument may also be a vector of class \code{Date} which extracts all dates that
#' show up in that vector. See the examples. By default equal to \code{NA}, meaning no subsetting based on dates is done.
#'
#' @return A modified \code{sentomeasures} object, with only the sentiment measures required, including updated information
#' and statistics, but the original sentiment scores \code{data.table} untouched.
#'
#' @examples
#' data("usnews")
#' data("lexicons")
#' data("valence")
#'
#' # construct a sentomeasures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
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
#' sel5 <- select_measures(sentomeasures, c("linear", "LM_eng"),
#'                         dates = "date >= '1989-12-31' & date <= '2000-12-31'")
#' d <- seq(as.Date("1980-01-01"), as.Date("2013-12-01"), by = "month")
#' sel6 <- select_measures(sentomeasures, c("linear", "LM_eng"), dates = d)
#'
#' @export
select_measures <- function(sentomeasures, toSelect = "all", do.combine = TRUE, dates = NA) {
  check_class(sentomeasures, "sentomeasures")

  allOpts <- c(sentomeasures$features, sentomeasures$lexicons, sentomeasures$time)
  if ("all" %in% toSelect) {
    toSelect <- allOpts
    do.combine = FALSE
  }
  valid <- toSelect %in% allOpts
  if (any(!valid)) {
    stop("Following components make up none of the sentiment measures: ", paste0(toSelect[!valid], collapse = ', '))
  }

  if (all(is.na(dates))) measures <- sentomeasures$measures
  else if (inherits(dates, "Date")) measures <- sentomeasures$measures[date %in% dates, ]
  else measures <- sentomeasures$measures[eval(parse(text = dates)), ]
  namesList <- stringi::stri_split(colnames(measures), regex = "--")
  if (do.combine == TRUE) fun <- all
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
#' @author Samuel Borms
#'
#' @method plot sentomeasures
#'
#' @description Straightforward plotting method that shows all sentiment measures from the provided \code{sentomeasures}
#' object in one plot, or the average along one of the lexicons, features and time weighting dimensions. We suggest to make
#' use of the code{select_measures()} function when you desire to plot only a subset of the sentiment measures.
#'
#' @param x a \code{sentomeasures} object.
#' @param group a value from \code{c("lexicons", "features", "time", "all")}. The first three choices display the average of
#' all measures from the same group, in a different color. The choice \code{"all"} displays every single sentiment measure
#' in a separate color, but this may look visually overwhelming very fast, and can be quite slow.
#' @param ... not used.
#'
#' @return Returns a simple \pkg{ggplot2} plot, which can be added onto (or to alter its default elements) by using the
#' \code{+} operator (see examples). By default, a legend is positioned at the top if there are at maximum twelve line graphs
#' plotted.
#'
#' @examples
#' data("usnews")
#' data("lexicons")
#' data("valence")
#'
#' # construct a sentomeasures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
#' l <- setup_lexicons(lexicons[c("LM_eng", "HENRY_eng")], valence[["valence_eng"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpus, l, ctr)
#'
#' # plot sentiment measures
#' plot(sentomeasures)
#' plot(sentomeasures, group = "lexicons")
#'
#' # adjust appearance of plot
#' p <- plot(sentomeasures)
#' p <- p +
#'   ggthemes::theme_base() +
#'   scale_x_date(name = "date") +
#'   scale_y_continuous(name = "newName")
#' p
#'
#' @import ggplot2
#' @export
plot.sentomeasures <- function(x, group = "all", ...) {

  if (!(group %in% c("lexicons", "features", "time", "all")))
    stop("The 'group' argument should be either 'lexicons', 'features', 'time' or 'all'.")
  # melt sentiment measures for plotting
  sentomeasures <- x
  measures <- sentomeasures$measures
  if (group == "all") {
    measuresMelt <- melt(measures, id.vars = "date")
  } else {
    measuresMelt <- to_long(measures)[, c("date", group, "value"), with = FALSE]
    measuresMelt <- measuresMelt[, list(value = mean(value)), by = list(date, variable = eval(parse(text = group)))]
  }
  legendPos <- ifelse(length(unique(measuresMelt[["variable"]])) <= 12, "top", "none")
  p <- ggplot(data = measuresMelt, aes(x = date, y = value, color = variable)) +
    geom_line() +
    geom_hline(yintercept = 0, size = 0.50, linetype = "dotted") +
    scale_x_date(name = "Date", date_labels = "%m-%Y") +
    scale_y_continuous(name = "Sentiment") +
    ggthemes::theme_tufte() +
    theme(legend.title = element_blank(), legend.position = legendPos, text = element_text(size = 11))

  return(p)
}

#' Add and fill missing dates
#'
#' @author Samuel Borms
#'
#' @description Adds missing dates between earliest and latest date, such that time series is continuous on a period-by-period
#' basis. Fills in these dates with either 0, the respective latest non-missing value or \code{NA}.
#'
#' @param sentomeasures a \code{sentomeasures} object.
#' @param fill an element of \code{c("zero", "latest", NA)}; the first and last assume missing dates represent zero sentiment,
#' the second assumes missing dates represent constant sentiment.
#'
#' @return A modified \code{sentomeasures} object.
#'
#' @examples
#' data("usnews")
#' data("lexicons")
#' data("valence")
#'
#' # construct a sentomeasures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
#' l <- setup_lexicons(lexicons[c("LM_eng", "HENRY_eng")], valence[["valence_eng"]])
#' ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "year", lag = 3)
#' sentomeasures <- sento_measures(corpus, l, ctr)
#'
#' # fill measures
#' f1 <- fill_measures(sentomeasures)
#' f2 <- fill_measures(sentomeasures, fill = "latest")
#' f3 <- fill_measures(sentomeasures, fill = NA)
#'
#' @export
fill_measures <- function(sentomeasures, fill = "zero") {
  check_class(sentomeasures, "sentomeasures")

  by <- sentomeasures$by
  measures <- sentomeasures$measures
  dates <- measures$date
  ts <- seq(dates[1], dates[length(dates)], by = by) # continuous date series
  dt <- data.table(date = ts)

  # join and fill as provided to new measures
  measuresFill <- merge(dt, measures, by = "date", all = TRUE) # fills with NA
  if (is.na(fill)) {
    sentomeasures$measures <- measuresFill
    return(sentomeasures)
  } else if (fill == "zero") {
    measuresFill[is.na(measuresFill)] <- 0
  } else if (fill == "latest") {
    measuresFill <- zoo::na.locf(measuresFill)
  } else stop("Input variable 'fill' should be either 'zero', 'latest' or NA.")
  measuresFill <- data.table(date = ts, measuresFill[, lapply(.SD, as.numeric), .SDcols = colnames(measures)[-1]])
  sentomeasures$measures <- measuresFill

  return(sentomeasures)
}

#' Scaling and centering of sentiment measures
#'
#' @author Samuel Borms
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
#' data("usnews")
#' data("lexicons")
#' data("valence")
#'
#' # construct a sentomeasures object to start with
#' corpus <- sento_corpus(corpusdf = usnews)
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

