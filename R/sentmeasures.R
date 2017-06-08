
sentmeasures <- function(corpus, lexicons, ctrl) { # constructor

  if (!("corpuS" %in% class(corpus))) stop("Provide a corpuS object.")

  sentiment <- compute_sentiment(corpuS, lexicons, ctrl$type)

  sentMatrix <- perform_agg(sentiment, ctrl)

  sm <- list(sentiment = sentiment,
             measures = sentMatrix)

  class(sm) <- c("sentmeasures", class(sm))

  return(sm)

}

setup_lexicons <- function(lexiconsRaw, valenceWords) {

  ### format of lexicons_raw and valenceWords to define

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

  lexicons[["valenceWords"]] <- val[!unique(sames), ]

  return(lexicons)

}

compute_sentiment <- function(corpuS, lexicons, type = c("words", "sentences")) {

  if (length(type) > 1) type <- type[1]

  texts <- texts(corpuS)
  lexNames <- names(lexicons)[names(lexicons) != "valenceWords"]

  if (type == "words") {

    # frequency-based document-feature matrix (rows are corpus ids, columns are words)
    dfm <- quanteda::dfm(quanteda::tokenize(corpuS,
                                            remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                                            ngrams = 1))

    ### ngrams value in function of valence shifters... (e.g. ngrams = 1:2)
    ### add remove = stopwords("english") in dfm... (or other specified stopwords)?

    allWords <- quanteda::featnames(dfm)

    s <- list()
    s[["dfmRaw"]] <- dfm
    for (lexicon in lexNames) {
      lexWords <- lexicons[[lexicon]]$x
      lexScores <- lexicons[[lexicon]]$y

      # locate polarized words and set weights to their polarity or keep at zero
      allScores <- rep(0, length(allWords))
      polInd <- allWords %in% lexWords
      polWords <- allWords[polInd]
      allScores[polInd] <- lexScores[lexWords %in% polWords]
      names(allScores) <- allWords

      # transform dfm to set value to (frequency * score) for polarized words, zero else
      dfmSent <- quanteda::dfm_weight(dfm, weights = allScores)

      # assembling of scored dfms in a lexicons-named list
      s[[lexicon]] <- dfmSent
    }

  } else if (type == "sentences") {

    s <- data.table()
    for (lexicon in lexNames) {

      # sentimentr based sentiment calculation at sentence level
      sent <- sentimentr::sentiment(text.var = texts,
                                    polarity_dt = lexicons[[lexicon]],
                                    valence_shifters_dt = lexicons[["valenceWords"]]) # element_id - sentence_id - word_count - sentiment
      sent$sentence_id <- NULL

      # column-wise binding of scores per lexicon
      if (nrow(s) != 0) s <- cbind(s, sent[, "sentiment"])
      else s <- sent

    }
    names(s)[-(1:2)] <- lexNames

  } else stop("Select either 'words' or 'sentences' as type.")

  sentOut <- list(corpuS = corpuS,
                  sent = s,
                  type = type,
                  features = corpuS$features,
                  lexicons = lexNames)

  return(sentOut)

}

perform_agg <- function(toAgg, ctrl) {

  type <- ctrl$type

  if (type != toAgg$type) stop("Sentiment is not calculated according to the same type as indicated in the ctrl argument.")

  howWithin <- ctrl$within
  howDocs <- ctrl$docs
  howTime <- ctrl$time
  ignoreZeros <- ctrl$ignoreZeros
  lag <- ctrl$lag
  otherVars <- ctrl$other # list

  if (type == "words") agg <- agg_words(toAgg, howWithin)
  else agg <- agg_sentences(sent, howWithin)

  agg <- agg_documents(agg, howDocs, ignoreZeros)

  agg <- agg_time(agg, lag, howTime, otherVars)

  return(agg)

}

agg_words <- function(toAgg, how = get_hows()$words) {

  if (length(how) > 1) how <- how[1]

  if (toAgg$type != "words") stop("Sentiment not calculated at terms/words level.")

  features <- toAgg$features
  lexNames <- toAgg$lexicons
  sent <- toAgg$sent

  aggs <- as.data.table(matrix(0, nrow = quanteda::ndoc(toAgg$corpuS), ncol = length(lexicons)))
  names(aggs) <- lexNames

  # loop over all dfms and bind aggregated scores columns into aggs
  dfmRaw <- sent[["dfmRaw"]]
  wCounts <- rowSums(dfmRaw, na.rm = TRUE)
  for (lexicon in lexNames) {
    dfm <- sent[[lexicon]]

    if (how == "equal-weight") {

      weights <- dfmRaw / wCounts

    } else if (how == "proportional") { # proportional w.r.t. words frequency vs. total words frequency per document

      weights <- quanteda::tf(dfmRaw, scheme = "prop") # weight = (words freq. / total words freq.) per document

    } else if (how == "tf-idf") {

      weights <- quanteda::tfidf(dfmRaw, normalize = TRUE)

    } else stop("Please select an appropriate aggregation 'how'.")

    dfmWeighted <- dfm * weights # zeros represented by . (sparse matrix)
    agg <- rowSums(dfmWeighted, na.rm = TRUE)
    aggs[, lexicon] <- agg

  }

  aggs <- as.data.table(cbind(quanteda::docvars(toAgg$corpuS), word_count = wCounts, aggs)) # id - date - feature1 - ... - word_count - lexicon1 (sentiment) - ...

  aggWords <- list(sent = aggs,
                   features = features,
                   lexicons = lexNames)

  return(aggWords)

}

agg_sentences <- function(toAgg, how = get_hows()$sentences) {

  if (length(how) > 1) how <- how[1]

  if (toAgg$type != "sentences") stop("Sentiment not calculated at sentence level.")

  features <- toAgg$features
  lexNames <- toAgg$lexicons
  sent <- toAgg$sent

  # apply aggregation to lexicon-sentiment columns and word_count column
  if (how == "equal-weight") {

    agg <- sent %>%
      group_by(element_id) %>%
      summarise_all(funs(mean(., na.rm = TRUE)))

  } else if (how == "proportional") { # proportional w.r.t. words in sentence vs. total words in document

    agg <- sent %>%
      group_by(element_id) %>%
      summarise_all(funs(sum(. * word_count/(sum(word_count, na.rm = TRUE)), na.rm = TRUE)))

  } else stop("Please select an appropriate aggregation 'how'.")

  agg$element_id <- NULL
  agg$word_count <- sent[, list(word_count = sum(word_count, na.rm = TRUE)), by = element_id]$word_count

  ### slight differences in word_count in words and sentences aggregation due to different tokenization (sentimentr vs. quanteda)

  aggs <- as.data.table(cbind(quanteda::docvars(toAgg$corpuS), agg)) # id - date - feature1 - ... - word_count - lexicon1 (sentiment) - ...

  aggSents <- list(sent = aggs,
                   features = features,
                   lexicons = lexNames)

  return(aggSents)

}

agg_documents <- function(toAgg, how = get_hows()$docs, ignoreZeros = FALSE) {

  features <- toAgg$features
  lexNames <- toAgg$lexicons
  sent <- toAgg$sent

  # multiply lexicons with features to obtain feature-sentiment scores per lexicon
  combs <- NULL
  for (lexicon in lexNames) {
    for (feature in features) {
      name <- paste0(lexicon, "--", feature)
      sent[, name] <- sent[, lexicon, with = FALSE] * sent[, feature, with = FALSE]
      combs <- c(combs, name)
    }
  }
  sent <- as.data.table(sent)
  sent[, eval(c(lexNames, features)) := NULL] # remove since replaced by lexicon--feature columns

  # ignore documents with zero sentiment in aggregation (if ignoreZeros is TRUE)
  if (ignoreZeros) sent[, combs] <-
    sent[, combs, with = FALSE][, lapply(.SD, function(x) replace(x, which(x == 0), NA))]

  # aggregate feature-sentiment per document by date for all lexicon columns
  if (how == "equal-weight") {

    aggs <- sent %>%
      group_by(date) %>%
      summarise_all(funs(mean(., na.rm = TRUE)))

  } else if (how == "proportional") { # proportional w.r.t. words in document vs. total words in all documents per date

    aggs <- sent %>%
      group_by(date) %>%
      summarise_all(funs(sum(. * word_count/(sum(word_count, na.rm = TRUE)), na.rm = TRUE)))

  }

  aggs$word_count <- NULL

  aggDocs <- list(sent = aggs,
                  features = features,
                  lexicons = lexNames)

  return(aggDocs)

}

agg_time <- function(toAgg, lag, how = get_hows()$time, ...) {

  # construct or pass on weights data.frame and check for duplicated names
  if (how == "own") weights <- ...$weights
  else weights <- setup_time_weights(lag, how, ...)

  if (sum(duplicated(colnames(weights))) > 0) {
    duplics <- unique(colnames(weights)[duplicated(colnames(weights))])
    stop(paste0("Names of weighting schemes are not unique. Following names occur at least twice: ",
                paste0(duplics, collapse = ", ")))
  }

  # convert sentiment measures into xts object and order by date
  sent <- toAgg$sent
  toRoll <- xts::xts(sent[, 2:ncol(sent)], order.by = sent$date) ### date should be in an appropriate format

  # apply rolling time window for every weights column and combine all new measures column-wise
  n <- nrow(weights)
  for (i in 1:ncol(weights)) {
    w <- weights[, i]
    name <- colnames(weights)[i]

    add <- data.frame(zoo::rollapplyr(toRoll, width = n, by.column = TRUE, roll_weights, w = w, align = "right"))
    colnames(add) <- paste0(colnames(toRoll), "--", name)

    if (i == 1) sentMatrix <- add
    else sentMatrix <- cbind(sentMatrix, add)
  }

  aggTime <- list(sent = sentMatrix,
                  features = toAgg$features,
                  lexicons = toAgg$lexicons,
                  time = colnames(weights))

  return(aggTime)
}

merge_matrix <- function(ctrl) {

  agg <- ctrl$agg
  sentMatrix <- agg$sent
  toMerge <- ctrl[2:4]
  keep <- ctrl$keep

  if (keep) {
    sentMatrixOld <- sentMatrix
    namesOld <- colnames(sentMatrix)
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
        locs <- stringr::str_detect(colnames(sentMatrix), paste0(el, "\\b")) # exact match required
        ls[[el]] <- sentMatrix[, locs, drop = FALSE]
        sentMatrix <- sentMatrix[, !locs, drop = FALSE]
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

      # add back merged columns here to allow them to be merged furher if needed
      sentMatrix <- cbind(sentMatrix, merged)
    }

  }

  # add old unmerged measures to merged measures (if keep is TRUE)
  if (keep) sentMatrix <- cbind(sentMatrix, sentMatrixOld[, !(namesOld %in% colnames(sentMatrix))]) # add back previously dropped columns

  return(sentMatrix)

}

ctrl_agg <- function(sentType, howWithin, howDocs, howTime, ignoreZeros, lag, ...) {

  ### sentType is 'words' or 'sentences', and either already used in a separate sentiment calculation or to be used

  # get all supported options for each aggregation level
  hows <- get_hows()

  # check if provided aggregations are supported
  warned <- 0
  if (!(howWithin %in% hows[[sentType]])) {
    warning(paste0(howWithin, " is no current option for aggregation across ", toAgg$type, "."))
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

  ctrl_agg <- list(type = sentType,
                   within = howWithin,
                   docs = howDocs,
                   time = howTime,
                   ignoreZeros = ignoreZeros,
                   lag = lag,
                   other = list(...))

  return(ctrl_agg)

}

ctrl_merge <- function(agg, lex = NA, feat = NA, time = NA, keep = FALSE) {

  missings <- c()
  tooFew <- c()

  # check if columns to merge exist (missings) and if all merges have at least two columns to combine and are all unique (tooFew)
  if (all(!is.na(lex))) {
    missings <- c(missings, unlist(lex)[!(unlist(lex) %in% agg$lexicons)])
    for (i in seq_along(lex)) {
      if (length(lex[[i]]) <= 1 | length(unique(lex[[i]])) != length(lex[[i]])) tooFew <- c(tooFew, names(lex)[i])
    }
  }

  if (all(!is.na(feat))) {
    missings <- c(missings, unlist(feat)[!(unlist(feat) %in% agg$features)])
    for (i in seq_along(feat)) {
      if (length(feat[[i]]) <= 1 | length(unique(feat[[i]])) != length(feat[[i]])) tooFew <- c(tooFew, names(feat)[i])
    }
  }

  if (all(!is.na(time))) {
    missings <- c(missings, unlist(time)[!(unlist(time) %in% agg$time)])
    for (i in seq_along(time)) {
      if (length(time[[i]]) <= 1 | length(unique(time[[i]])) != length(time[[i]])) tooFew <- c(tooFew, names(time)[i])
    }
  }

  msg1 <- c()
  msg2 <- c()
  if (length(missings) > 0) {
    msg1 <- paste0("Following columns to merge are not found: ", paste0(missings, collapse = ", "), ".")
    warning(msg1)
  }
  if (length(tooFew) > 0) {
    msg2 <- paste0("Following merges have less than two or not all unique columns: ", paste0(tooFew, collapse = ", "), ".")
    warning(msg2)
  }
  if (length(msg1) > 0 | length((msg2) > 0)) stop("Wrong input. See warning messages for specifics.")

  ctrl_merge <- list(agg = agg,
                     lex = lex, # e.g. list(lex = list(lex12 = c("lex1", "lex2"), lex34 = c("lex3", "lex4")), feat = ..., time = ...)
                     feat = feat,
                     time = time,
                     keep = keep)

  return(ctrl_merge)

}

