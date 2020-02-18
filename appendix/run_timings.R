
############################################################
####### COMPARISON OF TEXTUAL SENTIMENT COMPUTATIONS #######
############################################################

###### DESCRIPTION ######

### This code is used for the supplementary appendix to the vignette paper 'The R Package sentometrics
### to Compute, Aggregate and Predict with Textual Sentiment' (Ardia, Bluteau, Borms and Boudt, 2020),
### comparing various textual sentiment computation tools in R.
### Download the package and its dependencies first before you run this script...
### install.packages("sentometrics", dependencies = TRUE) # from CRAN (version 0.8), OR
### install.packages("sentometrics_0.8.tar.gz", repos = NULL, dependencies = TRUE) # from the tar

###### SESSION INFO ######

# R version 3.6.2 (2019-12-12)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 18362)
#
# Matrix products: default
#
# locale:
# [1] LC_COLLATE=English_Belgium.1252  LC_CTYPE=English_Belgium.1252    LC_MONETARY=English_Belgium.1252
# [4] LC_NUMERIC=C                     LC_TIME=English_Belgium.1252
#
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
# [1] microbenchmark_1.4-7    tidyr_1.0.0             dplyr_0.8.3             lexicon_1.2.1
# [5] data.table_1.12.8       SentimentAnalysis_1.3-3 syuzhet_1.0.4           meanr_0.1-2
# [9] tidytext_0.2.2          quanteda_1.5.2          sentometrics_0.8.0
#
# loaded via a namespace (and not attached):
#  [1] NLP_0.2-0          Rcpp_1.0.3         pillar_1.4.3       compiler_3.6.2     tokenizers_0.2.1   iterators_1.0.12
#  [7] tools_3.6.2        stopwords_1.0      zeallot_0.1.0      packrat_0.5.0      lubridate_1.7.4    lifecycle_0.1.0
# [13] tibble_2.1.3       gtable_0.3.0       lattice_0.20-38    pkgconfig_2.0.3    rlang_0.4.2        Matrix_1.2-18
# [19] foreach_1.4.7      fastmatch_1.1-0    rstudioapi_0.10    parallel_3.6.2     xml2_1.2.2         janeaustenr_0.1.5
# [25] stringr_1.4.0      vctrs_0.2.1        generics_0.0.2     grid_3.6.2         tidyselect_0.2.5   glue_1.3.1
# [31] R6_2.4.1           ggplot2_3.2.1      purrr_0.3.3        spacyr_1.2         magrittr_1.5       ellipsis_0.3.0
# [37] backports_1.1.5    SnowballC_0.6.0    scales_1.1.0       codetools_0.2-16   assertthat_0.2.1   colorspace_1.4-1
# [43] stringi_1.4.5      RcppParallel_4.4.4 lazyeval_0.2.2     munsell_0.5.0      slam_0.1-47        tm_0.7-7
# [49] crayon_1.3.4

remove(list = ls())
options(prompt = "R> ", continue = "+  ", width = 120, digits = 4, max.print = 90, useFancyQuotes = FALSE)
sink(file = "output_timings.txt", append = FALSE, split = TRUE) # output printed in .txt file

library("sentometrics")

library("quanteda")
library("tidytext")
library("meanr")
library("syuzhet")
library("SentimentAnalysis")

library("data.table")
library("lexicon")
library("dplyr")
library("tidyr")
library("microbenchmark")

set.seed(505)

########################################### loading of packages, definition of lexicons

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
lexPure <- lex[-length(lex)]
lexClust <- sento_lexicons(lexiconsIn = lexiconsIn, valenceIn = list_valence_shifters[["en"]][, c("x", "t")])

keep <- sample(1:(nrow(usnews) * 25), 100000)
corpusAll <- quanteda::corpus(do.call(rbind, lapply(1:25, function(j) usnews))[keep, ], text_field = "texts")
nTexts <- c(1, 5, 10, 25, 50, 75, 100) * 1000

########################################### definition of sentiment functions

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
    rename_at(vars(-x), list(~names(lexicons)))
  lexNames <- colnames(lexiconsTable)[-1]
  sentiments <- tidyTexts %>%
    inner_join(lexiconsTable, by = "x") %>%
    group_by(linenumber) %>%
    summarise_at(vars(lexNames), list(~sum(., na.rm = TRUE)))
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
    rename_at(vars(-x), list(~names(lexicons)))
  lexNames <- colnames(lexiconsTable)[-c(1, ncol(lexiconsTable))] # drop "valence" list element
  sentiments <- tidyTexts %>%
    inner_join(lexiconsTable, by = "x") %>%
    mutate(bigram = c(1, (diff(position) == 1) * head(valence, -1))) %>%
    replace_na(list(bigram = 1)) %>%
    mutate(bigram = replace(bigram, bigram == 0, 1)) %>%
    group_by(linenumber) %>%
    summarise_at(vars(lexNames), list(~sum(. * bigram, na.rm = TRUE)))
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

syuzhetFunc <- function(texts) syuzhet::get_sentiment(texts, method = "bing")

########################################### sanity check of scores vs. tidytext calculators

if (FALSE) {
  K <- nTexts[2]

  system.time(s1 <- sentoUnigramsFunc(corpusAll[1:K]))
  system.time(s2 <- tidytextUnigramsFunc(corpusAll[1:K]))
  all.equal(s1$word_count, s2$word_count) & all.equal(s1$HULIU, s2$HULIU)

  system.time(s3 <- sentoUnigramsAllFunc(corpusAll[1:K]))
  all.equal(s1$word_count, s3$word_count) & all.equal(s1$HULIU, s3$HULIU)

  system.time(s4 <- sentoBigramsFunc(corpusAll[1:K]))
  system.time(s5 <- sentoBigramsAllFunc(corpusAll[1:K]))
  system.time(s6 <- tidytextBigramsFunc(corpusAll[1:K]))
  all.equal(s4$word_count, s5$word_count) & all.equal(s4$HULIU, s5$HULIU)
  all.equal(s4$word_count, s6$word_count) & all.equal(s4$HULIU, s6$HULIU)

  system.time(s7 <- compute_sentiment(corpusAll[1:K], lexicons = lex[c("HULIU")], how = "proportional"))
  system.time(s8 <- compute_sentiment(corpusAll[1:K], lexicons = lexPure, how = "proportional"))
  all.equal(s7$word_count, s8$word_count) & all.equal(s7$HULIU, s8$HULIU)

  system.time(s9 <- compute_sentiment(corpusAll[1:K], lexicons = lex[c("HULIU")], how = "proportionalPol"))
  system.time(s10 <- compute_sentiment(corpusAll[1:K], lexicons = lexPure, how = "proportionalPol"))
  all.equal(s9$word_count, s10$word_count) & all.equal(s9$HULIU, s10$HULIU)

  system.time(sA <- sentoUnigramsAllFunc(corpusAll[1:K]))
  system.time(sB <- tidytextUnigramsAllFunc(corpusAll[1:K]))
  system.time(sC <- sentoClustersAllParFunc(corpusAll[1:K]))
  sapply(colnames(sB), function(col) all(all.equal(sA[[col]], sB[[col]])))
}

########################################### timings for one lexicon

timingsFull.single <- lapply(nTexts, function(n) {
  cat("Run timings for texts size of", n, "\n")
  texts <- corpusAll[1:n]
  out <- microbenchmark(
    sentoUnigramsFunc(texts),
    sentoBigramsFunc(texts),
    sentoClustersFunc(texts),
    meanrFunc(texts),
    syuzhetFunc(texts),
    quantedaFunc(texts),
    tidytextUnigramsFunc(texts),
    times = 5,
    unit = "s"
  )
  out
})
timingsFull.single <- do.call(rbind, lapply(timingsFull.single, function(timing) summary(timing)[, "mean"]))
cat("\n")

# we run SentimentAnalysis' function separately as it is prone to a memory error for the larger corpus sizes
timingsSentimentAnalysis <- lapply(nTexts, function(n) {
  cat("Run timings for texts size of", n, "\n")
  texts <- corpusAll[1:n]
  out <- microbenchmark(SentimentAnalysisFunc(texts), times = 1, unit = "s")
  out
})
timingsSentimentAnalysis <- c(do.call(rbind, lapply(timingsSentimentAnalysis,
                                                    function(timing) summary(timing)[, "mean"])), NA)
cat("\n")

timingsAll.single <- cbind(timingsFull.single[, 1:4], timingsSentimentAnalysis, timingsFull.single[, 5:7])
colnames(timingsAll.single) <- c("sento_unigrams", "sento_bigrams", "sento_clusters",
                                 "meanr", "SentimentAnalysis", "syuzhet", "quanteda", "tidytext")
timings.single <- data.table(texts = nTexts, timingsAll.single)

########################################### timings for many lexicons

timingsFull.many <- lapply(nTexts, function(n) {
  cat("Run timings for texts size of", n, "\n")
  corpus <-  quanteda::corpus(do.call(rbind, lapply(1:25, function(j) usnews))[keep[1:n], ],
                              text_field = "texts")
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

cat("\n")
cat("PANEL A", "\n")
timings.single
cat("\n")
cat("PANEL B", "\n")
timings.many
cat("\n")

######

cat("############################## \n")
cat("###### SESSION INFO \n \n")
info <- sessionInfo()
print(info)
cat("\n")

######

sink()

