
############################################################
####### COMPARISON OF TEXTUAL SENTIMENT COMPUTATIONS #######
############################################################

remove(list = ls())

info <- sessionInfo()
cat("\n")
cat(info$R.version$version.string, "\n")
cat(info$platform, "\n")
cat(info$locale, "\n \n")

set.seed(505)

########################################### loading of packages, definition of lexicons

# library("sentometrics")
devtools::load_all()
library("lexicon")
library("quanteda")
library("dplyr")
library("tidyr")
library("tibble")
library("tidytext")
library("sentimentr")
library("meanr")
library("syuzhet")
library("SentimentAnalysis")
library("stringr")
library("microbenchmark")

data("usnews", package = "sentometrics")
data("list_lexicons", package = "sentometrics")
data("list_valence_shifters", package = "sentometrics")

lexiconsIn <- c(
  list_lexicons[c("LM_en", "HENRY_en", "GI_en")],
  list(
    NRC = lexicon::hash_sentiment_nrc,
    HULIU = lexicon::hash_sentiment_huliu,
    SENTIWORD = lexicon::hash_sentiment_sentiword,
    JOCKERS = lexicon::hash_sentiment_jockers,
    SENTICNET = lexicon::hash_sentiment_senticnet,
    SOCAL = lexicon::hash_sentiment_socal_google
  )
)
lex <- sento_lexicons(lexiconsIn = lexiconsIn, valenceIn = list_valence_shifters[["en"]])

keep <- sample(1:(nrow(usnews) * 25), 100000)
corpusAll <- quanteda::corpus(do.call(rbind, lapply(1:25, function(j) usnews))[keep, ], text_field = "texts")
nTexts <- c(1, 5, 10, 25, 50, 75, 100) * 1000

########################################### definition of sentiment functions

lexPure <- lex[-length(lex)]
lexClust <- sento_lexicons(lexiconsIn = lexiconsIn, valenceIn = list_valence_shifters[["en"]][, c("x", "t")])

# simple approach
sentoUnigramsFunc <- function(texts) compute_sentiment(texts, lexicons = lex["HULIU"], how = "counts")
sentoUnigramsAllFunc <- function(texts) compute_sentiment(texts, lexicons = lexPure, how = "counts")
sentoUnigramsAllFeaturesFunc <- function(corpus) compute_sentiment(corpus, lexicons = lexPure, how = "counts")

# bigrams approach
sentoBigramsFunc <- function(texts) compute_sentiment(texts, lexicons = lex[c("HULIU", "valence")], how = "counts")
sentoBigramsAllFunc <- function(texts) compute_sentiment(texts, lexicons = lex, how = "counts")

# clusters approach
sentoClustersFunc <- function(texts) compute_sentiment(texts, lexicons = lexClust[c("HULIU", "valence")], how = "counts")
sentoClustersAllFunc <- function(texts) compute_sentiment(texts, lexicons = lexClust, how = "counts")
sentoClustersAllParFunc <- function(texts) compute_sentiment(texts, lexicons = lexClust, how = "counts", nCore = 8)

meanrFunc <- function(texts) meanr::score(texts, nthreads = 1)

tidytextUnigrams <- function(texts, lexicons) {
  tidyTexts <- tibble(text = texts) %>%
    mutate(linenumber = row_number()) %>%
    unnest_tokens(x, text, token = "words", strip_numeric = TRUE) %>%
    group_by(linenumber)
  wCounts <- tidyTexts %>%
    count(linenumber)
  lexiconsTable <- Reduce(function(x, y) full_join(x, y, by = "x"), lapply(lexicons, as.tbl)) %>%
    rename_at(vars(-x), funs(names(lexicons)))
  lexNames <- colnames(lexiconsTable)[-1]
  sentiments <- tidyTexts %>%
    inner_join(lexiconsTable, by = "x") %>%
    group_by(linenumber) %>%
    summarise_at(vars(lexNames), funs(sum(., na.rm = TRUE)))
  N <- length(texts)
  nToAdd <- (1:N)[which(!(1:N %in% sentiments[["linenumber"]]))]
  if (length(nToAdd) != 0) {
    sentiments <- sentiments %>%
      bind_rows(tibble(linenumber = nToAdd))
  }
  sentiments <- sentiments %>%
    replace(is.na(.), 0) %>%
    arrange(linenumber) %>%
    mutate(linenumber = wCounts[["n"]]) %>%
    rename(word_count = linenumber)
  sentiments
}

tidytextBigrams <- function(texts, lexicons) {
  tidyTexts <- tibble(text = texts) %>%
    mutate(linenumber = row_number()) %>%
    unnest_tokens(x, text, token = "words", strip_numeric = TRUE) %>%
    group_by(linenumber) %>%
    mutate(position = row_number())
  wCounts <- tidyTexts %>%
    count(linenumber)
  lexiconsTable <- Reduce(function(x, y) full_join(x, y, by = "x"), lapply(lexicons, as.tbl)) %>%
    rename_at(vars(-x), funs(names(lexicons)))
  lexNames <- colnames(lexiconsTable)[-c(1, ncol(lexiconsTable))] # drop "valence" list element
  sentiments <- tidyTexts %>%
    inner_join(lexiconsTable, by = "x") %>%
    mutate(bigram = c(1, (diff(position) == 1) * head(valence, -1))) %>%
    replace_na(list(bigram = 1)) %>%
    mutate(bigram = replace(bigram, bigram == 0, 1)) %>%
    group_by(linenumber) %>%
    summarise_at(vars(lexNames), funs(sum(. * bigram, na.rm = TRUE)))
  N <- length(texts)
  nToAdd <- (1:N)[which(!(1:N %in% sentiments[["linenumber"]]))]
  if (length(nToAdd) != 0) {
    sentiments <- sentiments %>%
      bind_rows(tibble(linenumber = nToAdd))
  }
  sentiments <- sentiments %>%
    replace(is.na(.), 0) %>%
    arrange(linenumber) %>%
    mutate(linenumber = wCounts[["n"]]) %>%
    rename(word_count = linenumber)
  sentiments
}

tidytextUnigramsFunc <- function(texts) tidytextUnigrams(texts, lex["HULIU"])
tidytextBigramsFunc <- function(texts) tidytextBigrams(texts, lex[c("HULIU", "valence")])
tidytextUnigramsAllFunc <- function(texts) tidytextUnigrams(texts, lexPure)
tidytextBigramsAllFunc <- function(texts) tidytextBigrams(texts, lex)

dictHuliu <- SentimentDictionaryWeighted(words = lex[["HULIU"]][["x"]], scores = lex[["HULIU"]][["y"]])
SentimentAnalysisFunc <- function(texts) { # uses tm as backend
  analyzeSentiment(texts, stemming = FALSE, removeStopwords = FALSE,
                   rules = list("HULIU" = list(ruleLinearModel, dictHuliu)))
}

scores <- unique(lex[["HULIU"]][["y"]])
scoresList <- as.list(scores)
for (i in seq_along(scores)) scoresList[[i]] <- lex[["HULIU"]][y == eval(scores[i]), x]
names(scoresList) <- scores
quantedaDictHuliu <- dictionary(scoresList)
quantedaFunc <- function(texts) {
  dfmOut <- dfm(texts, dictionary = quantedaDictHuliu, what = "word", remove_numbers = TRUE, remove_punct = TRUE)
  weights <- as.numeric(featnames(dfmOut))
  sentiment <- dfmOut %*% weights
  sentiment[, 1]
}

sentimentrFunc <- function(texts) {
  sents <- sentimentr::get_sentences(texts)
  sentimentr::sentiment_by(sents, averaging.function = average_mean,
                           polarity_dt = lex[["HULIU"]],
                           valence_shifters_dt = lexicon::hash_valence_shifters, n.before = 1, n.after = 0)
}

syuzhetFunc <- function(texts) syuzhet::get_sentiment(texts, method = "syuzhet")

########################################### intermediate checks

# # K <- tail(nTexts, 1)
# K <- 5000
#
# system.time(s1 <- sentoUnigramsFunc(corpusAll[1:K]))
# system.time(s2 <- tidytextUnigramsFunc(corpusAll[1:K]))
# all.equal(s1$word_count, s2$word_count) & all.equal(s1$HULIU, s2$HULIU)
#
# system.time(s3 <- sentoUnigramsAllFunc(corpusAll[1:K]))
# all.equal(s1$word_count, s3$word_count) & all.equal(s1$HULIU, s3$HULIU)
#
# system.time(s4 <- sentoBigramsFunc(corpusAll[1:K]))
# system.time(s5 <- sentoBigramsAllFunc(corpusAll[1:K]))
# system.time(s6 <- tidytextBigramsFunc(corpusAll[1:K]))
# all.equal(s4$word_count, s5$word_count) & all.equal(s4$HULIU, s5$HULIU)
# all.equal(s4$word_count, s6$word_count) & all.equal(s4$HULIU, s6$HULIU)
#
# system.time(s7 <- compute_sentiment(corpusAll[1:K], lexicons = lex[c("HULIU")], how = "proportional"))
# system.time(s8 <- compute_sentiment(corpusAll[1:K], lexicons = lexPure, how = "proportional"))
# all.equal(s7$word_count, s8$word_count) & all.equal(s7$HULIU, s8$HULIU)
#
# system.time(s9 <- compute_sentiment(corpusAll[1:K], lexicons = lex[c("HULIU")], how = "proportionalPol"))
# system.time(s10 <- compute_sentiment(corpusAll[1:K], lexicons = lexPure, how = "proportionalPol"))
# all.equal(s9$word_count, s10$word_count) & all.equal(s9$HULIU, s10$HULIU)
#
# system.time(sA <- sentoUnigramsAllFunc(corpusAll[1:K]))
# system.time(sB <- tidytextUnigramsAllFunc(corpusAll[1:K]))
# system.time(sC <- sentoClustersAllParFunc(corpusAll[1:K]))
# sapply(colnames(sA), function(col) all(all.equal(sA[[col]], sB[[col]])))

########################################### timings for one lexicon

timingsFull.single <- lapply(nTexts, function(n) {
  cat("Run timings for texts size of", n, "\n")
  texts <- corpusAll[1:n]
  out <- microbenchmark(
    sentoUnigramsFunc(texts),
    sentoBigramsFunc(texts),
    sentoClustersFunc(texts),
    meanrFunc(texts),
    # SentimentAnalysisFunc(texts),
    syuzhetFunc(texts),
    quantedaFunc(texts),
    tidytextUnigramsFunc(texts),
    # sentimentrFunc(texts), # too slow
    times = 5,
    unit = "s"
  )
  out
})
timingsFull.single <- do.call(rbind, lapply(timingsFull.single, function(timing) summary(timing)[, "mean"]))

timingsSentimentAnalysis <- lapply(head(nTexts, -1), function(n) {
  cat("Run timings for texts size of", n, "\n")
  texts <- corpusAll[1:n]
  out <- microbenchmark(SentimentAnalysisFunc(texts), times = 3, unit = "s")
  out
})
timingsSentimentAnalysis <- c(do.call(rbind,
                                      lapply(timingsSentimentAnalysis, function(timing) summary(timing)[, "mean"])), NA)

timingsAll.single <- cbind(timingsFull.single[, 1:4], timingsSentimentAnalysis, timingsFull.single[, 5:7])
colnames(timingsAll.single) <- c("sento_unigrams", "sento_bigrams", "sento_clusters",
                                 "meanr", "SentimentAnalysis", "syuzhet", "quanteda", "tidytext")
timings.single <- data.table(texts = nTexts, timingsAll.single)
timings.single
save(timings.single, file = "C:/Users/gebruiker/Dropbox/SENTOMETRICS-R-PACKAGE/vignette/timings.single.rda")

########################################### timings for many lexicons

timingsFull.many <- lapply(nTexts, function(n) {
  cat("Run timings for texts size of", n, "\n")
  corpus <-  quanteda::corpus(do.call(rbind, lapply(1:25, function(j) usnews))[keep[1:n], ], text_field = "texts")
  texts <- quanteda::texts(corpus)
  out <- microbenchmark(
    sentoUnigramsAllFunc(texts),
    sentoUnigramsAllFeaturesFunc(corpus),
    sentoBigramsAllFunc(texts),
    sentoClustersAllFunc(texts),
    sentoClustersAllParFunc(texts),
    tidytextUnigramsAllFunc(texts),
    tidytextBigramsAllFunc(texts),
    times = 5,
    unit = "s"
  )
  out
})
timingsFull.many <- do.call(rbind, lapply(timingsFull.many, function(timing) summary(timing)[, "mean"]))
colnames(timingsFull.many) <- c("sento_unigrams_many", "sento_unigrams_many_features", "sento_bigrams_many",
                                "sento_clusters_many", "sento_clusters_many_parallel",
                                "tidytext_unigrams_many", "tidytext_bigrams_many")
timings.many <- data.table(texts = nTexts, timingsFull.many)
timings.many
save(timings.many, file = "C:/Users/gebruiker/Dropbox/SENTOMETRICS-R-PACKAGE/vignette/timings.many.rda")

###########################################

