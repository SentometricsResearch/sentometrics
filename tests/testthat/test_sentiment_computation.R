
library("sentometrics")
library("quanteda")
context("Sentiment computation")

set.seed(123)

# corpus and lexicon creation
data("usnews")
corpus <- sento_corpus(corpusdf = usnews[1:250, ])

data("list_lexicons")
lex <- setup_lexicons(list_lexicons[c("GI_en", "LM_en", "HENRY_en")], list_valence_shifters[["en"]])
lexSplit <- setup_lexicons(list_lexicons[c("GI_en", "LM_en", "HENRY_en")], list_valence_shifters[["en"]], do.split = TRUE)

### tests from here ###

sentimentList <- list(
  s1 = compute_sentiment(quanteda::texts(corpus), lex, how = "counts"),
  s2 = compute_sentiment(quanteda::texts(corpus), lex, how = "tf-idf"),
  s3 = compute_sentiment(quanteda::texts(corpus), lex, how = "proportional"),
  s4 = compute_sentiment(quanteda::texts(corpus), lex, how = "proportionalPol"),
  s5 = compute_sentiment(quanteda::corpus(usnews[1:250, "texts"]), lex, how = "counts"),
  s6 = compute_sentiment(quanteda::corpus(usnews[1:250, c("texts", "wsj", "economy")], text_field = "texts"),
                         lex, how = "counts"),
  s7 = compute_sentiment(corpus, lex, how = "counts"),
  s8 = compute_sentiment(quanteda::texts(corpus), lexSplit, how = "counts")
)

# compute_sentiment
test_that("Agreement between sentiment scores across input objects", {
  expect_true(all(unlist(lapply(sentimentList, function(s) nrow(s$sentiment) == 250))))
  expect_true(all(unlist(lapply(sentimentList, function(s) all(s$sentiment$word_count
                                                               == sentimentList$s1$sentiment$word_count)))))
  expect_equivalent(sentimentList$s1$sentiment[, c("GI_en", "LM_en", "HENRY_en")],
                    sentimentList$s5$sentiment[, c("GI_en", "LM_en", "HENRY_en")])
  expect_equivalent(sentimentList$s6$sentiment[, -c(1:2)],
                    sentimentList$s7$sentiment[, colnames(sentimentList$s6$sentiment)[-c(1:2)], with = FALSE])
  expect_true(all(sentimentList$s8$sentiment[, c("GI_en_POS", "LM_en_POS", "HENRY_en_POS")] >= 0))
  expect_true(all(sentimentList$s8$sentiment[, c("GI_en_NEG", "LM_en_NEG", "HENRY_en_NEG")] <= 0))
})

# setup_lexicons
test_that("Proper fail when at least one lexicon name contains a '-'", {
  expect_error(setup_lexicons(list("heart--break--hotel" = list_lexicons[["LM_en"]], "good" = list_lexicons[["GI_en"]])))
})

