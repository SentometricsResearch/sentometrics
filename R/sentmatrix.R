
sentmatrix <- function(corpus, ...) { # constructor
  
  if (!("corpuS" %in% class(corpus))) stop("Provide a corpuS object.")
  
  # sentiment calculation
  
  # perform_agg()
  
  sm <- list(sentmatrix = sentmatrix,
             sentiment = sentiment)
  
  class(sm) <- c("sentmatrix", class(sm))
  
  return(sm)
  
}

setup_lexicons <- function(lexiconsRaw, names = NULL, ...) {
  
  ### format of lexicons_raw to define (only polarized words, so no 0-value words)
  
  ### lists of valence shifters to add (+ in compute_sentiment() function)
  
  lexicons <- lapply(lexiconsRaw, sentimentr::as_key)
  
  if (is.null(names)) names(lexicons) <- names(lexiconsRaw)
  else names(lexicons) <- names
  
  return(lexicons)
  
}

compute_sentiment <- function(corpuS, lexicons, type = "word", ...) {
  
  texts <- corpuS$documents$texts
  lexNames <- names(lexicons)
  
  if (type == "word") {
    
    dfm <- quanteda::dfm(quanteda::tokenize(corpuS, # frequency-based document-feature matrix (rows are corpus ids, columns are words)
                                            remove_punct = TRUE,
                                            remove_numbers = TRUE,
                                            remove_symbols = TRUE,
                                            remove_separators = TRUE,
                                            ngrams = 1)) # ngrams value in function of valence shifters...
    allWords <- quanteda::featnames(dfm)    
    
    s <- list()
    s[["dfmRaw"]] <- dfm
    for (lexicon in lexNames) {
      lexWords <- lexicons[[lexicon]]$x
      lexScores <- lexicons[[lexicon]]$y
      
      allScores <- rep(0, length(allWords))
      polInd <- allWords %in% lexWords # location of polarized words in all features
      polWords <- allWords[polInd]
      allScores[polInd] <- lexScores[lexWords %in% polWords] # set scores for polarized words to their polarity
      names(allScores) <- allWords
      
      dfmSent <- quanteda::dfm_weight(dfm, weights = allScores) # frequency * score for polarized words, zero else
      
      s[[lexicon]] <- dfmSent
    }
    
  } else if (type == "sentence") {
    
    s <- data.table()
    for (lexicon in lexNames) {
      sent <- sentimentr::sentiment(text.var = texts, 
                                    polarity_dt = lexicons[[lexicon]]) # element_id - sentence_id - word_count - sentiment
      
      if (nrow(s) != 0) s <- cbind(s, sent[, "sentiment"])
      else s <- sent
    }
    names(s)[-(1:3)] <- names(lexicons)
    
  } else stop("Select either 'word' or 'sentence' as type.")
  
  sentOut <- list(corpuS = corpuS,
                  sent = s,
                  type = type,
                  features = corpuS$features,
                  lexicons = lexNames)
  
  return(sentOut)
  
}

perform_agg <- function(toAgg, ...) {
  
  # checks
  
  agg <- agg_words(toAgg, ...)
  # other aggregations
  
  return(agg)
  
}

agg_words <- function(toAgg, how = c("equal-weight","tf-idf", "proportional"), ...) {
  
  if (length(how) > 1) how <- how[1]
  
  if (toAgg$type != "word") stop("Sentiment not calculated at terms/words level.")
  
  features <- toAgg$features
  lexNames <- toAgg$lexicons
  sent <- toAgg$sent
  
  aggs <- as.data.table(matrix(0, nrow = nrow(sent[[1]]), ncol = length(lexicons)))
  names(aggs) <- lexNames
  for (lexicon in lexNames) { # loop over all dfms per lexicon
    dfm <- sent[[lexicon]]
    
    if (how == "equal-weight") { # equal-weight across polarized words or all words, not total words in document
      
      agg <- rowMeans(dfm, na.rm = TRUE)
      aggs[, lexicon] <- agg
      
    } else if (how == "tf-idf") {
      
      dfmRaw <- sent[["dfmRaw"]]
      dfmTfidf <- quanteda::tfidf(dfmRaw, normalize = TRUE)
      dfmWeighted <- dfmRaw * dfmTfidf # zeros represented by . (sparse matrix)
      
      agg <- rowSums(dfmWeighted, na.rm = TRUE)
      aggs[, lexicon] <- agg  
      
    } else if (how == "proportional") {
      
      dfmRaw <- sent[["dfmRaw"]]
      dfmProp <- quanteda::tf(dfmRaw, scheme = "prop") # weight = words freq./sum(words freq.) per document
      dfmWeighted <- dfmRaw * dfmProp # zeros represented by . (sparse matrix)
      
      agg <- rowSums(dfmWeighted, na.rm = TRUE)
      aggs[, lexicon] <- agg 
      
    }
    
  }
  
  aggs <- cbind(date = toAgg$corpuS$documents$date, aggs, toAgg$corpuS$documents[, features])
  
  aggWords <- list(sent = aggs, # date - lexicon1 (sentiment) - ... - feature1 - ...
                   features = features,
                   lexicons = lexNames)
  
  return(aggWords)
  
}

agg_sentences <- function(toAgg, how = c("equal-weight", "proportional"), ...) {
  
  if (length(how) > 1) how <- how[1]
  
  if (toAgg$type != "sentence") stop("Sentiment not calculated at sentence level.")
  
  features <- toAgg$features
  lexNames <- toAgg$lexicons
  sent <- toAgg$sent
  
  if (how == "equal-weight") {
    
    agg <- sent %>%
      group_by(element_id) %>%
      summarise_all(funs(mean(., na.rm = TRUE)))
    
  } else if (how == "proportional") { # proportional w.r.t. words per sentence
    
    agg <- sent %>%
      group_by(element_id) %>%
      summarise_all(funs(sum(. * word_count/(sum(word_count, na.rm = TRUE)), na.rm = TRUE))) 
    
  }
  
  agg$element_id <- agg$sentence_id <- NULL
  agg$word_count <- NULL # keep and use as a weight in aggregation across documents?
  
  aggs <- as.data.table(cbind(date = toAgg$corpuS$documents$date, agg, toAgg$corpuS$documents[, features]))
  
  aggSents <- list(sent = aggs, # date - lexicon1 (sentiment) - ... - feature1 - ...
                   features = features,
                   lexicons = lexNames)
  
  return(aggSents)
  
}

agg_documents <- function(toAgg, how = c("equal-weight"), ignoreZeros = FALSE, ...) {
  
  features <- toAgg$features
  lexNames <- toAgg$lexicons
  sent <- toAgg$sent
  
  combs <- NULL # all lexicon/feature combinations
  for (lexicon in lexNames) {
    for (feature in features) {
      name <- paste0(lexicon, "--", feature)
      sent[, name] <- sent[, lexicon, with = FALSE] * sent[, feature, with = FALSE] # obtain topic-sentiment per lexicon
      combs <- c(combs, name)
    }
  }
  sent <- as.data.table(sent)
  sent[, eval(c(lexNames, features)) := NULL] # can be removed since replaced by lexicon_feature columns
  
  if (ignoreZeros) sent[, combs] <- # documents with zero sentiment ignored in aggregation (also for other aggregations?)
    sent[, combs, with = FALSE][, lapply(.SD, function(x) replace(x, which(x == 0), NA))]
  
  aggs <- sent %>%
    group_by(date) %>%
    summarise_all(funs(mean(., na.rm = TRUE))) # sums sentiment per lexicon over each date
  
  aggDocs <- list(sent = aggs,
                  features = features,
                  lexicons = lexNames)
  
  return(aggDocs)
  
}

setup_time_weights <- function(lag, how = c("equal-weight", "Almon"), ...) {
  
  # more to add (exponential, etc.)
  
  if (how == "equal-weight") {
    
    weights <- data.frame(matrix(1/lag, nrow = lag, ncol = 1))
    names(weights) <- "equal_weight"
    
  } else if (how == "Almon") {
    
    otherVars <- list(...)
    weights <- almons(lag, otherVars$orders, otherVars$inverse, otherVars$normalize)
    
  } else stop("Please select an appropriate weight type.")
  
  return(weights)
}

agg_time <- function(toAgg, weights, ...) {
  
  sent <- toAgg$sent
  
  sMat <- xts::xts(sent[, 2:ncol(sent)], order.by = sent$date) # date should be in an appropriate format!
  
  n <- nrow(weights)
  for (i in 1:ncol(weights)) {
    w <- weights[, i]
    name <- colnames(weights)[i] # documentation: inform users if own weights that column names should be sensible
    
    toRoll <- sMat[, 2:ncol(sMat)]
    add <- data.frame(zoo::rollapplyr(toRoll, width = n, by.column = TRUE, roll_weights, w = w, align = "right"))
    colnames(add) <- paste0(colnames(add), "--", name)
    
    if (i == 1) sentMatrix <- add
    else sentMatrix <- cbind(sentMatrix, add)
  }
  
  aggTime <- list(sent = sentMatrix,
                  features = features,
                  lexicons = lexNames)
  
  return(aggTime)
}

agg_lexicons <- function(...) {
  
  # to do
  
}

