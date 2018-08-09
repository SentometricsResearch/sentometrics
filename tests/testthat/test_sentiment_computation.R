
library("sentometrics")
library("quanteda")
context("Sentiment computation")

set.seed(123)

# corpus and lexicon creation
data("usnews")
corpus <- sento_corpus(corpusdf = usnews[1:250, ])

data("list_lexicons")
lex <- setup_lexicons(list_lexicons[c("GI_en", "LM_en", "HENRY_en")],
                      list_valence_shifters[["en"]])

### tests from here ###

sentiment <- list(
  s1 = compute_sentiment(quanteda::texts(corpus), lex, how = "counts"),
  s2 = compute_sentiment(quanteda::texts(corpus), lex, how = "tf-idf"),
  s3 = compute_sentiment(quanteda::texts(corpus), lex, how = "proportional"),
  s4 = compute_sentiment(quanteda::texts(corpus), lex, how = "proportionalPol"),
  s5 = compute_sentiment(quanteda::corpus(usnews[1:250, "texts"]), lex, how = "counts"),
  s6 = compute_sentiment(quanteda::corpus(usnews[1:250, c("texts", "wsj", "economy")], text_field = "texts"),
                         lex, how = "counts"),
  s7 = compute_sentiment(corpus, lex, how = "counts")
)
test_that("Agreement between sentiment scores across input objects", {
  expect_true(all(unlist(lapply(sentiment, function(s) nrow(s[["sentiment"]]) == 250))))
  expect_true(all(unlist(lapply(sentiment, function(s) all(s[["sentiment"]][["word_count"]]
                                == sentiment$s1$sentiment$word_count)))))
  expect_equivalent(sentiment$s1$sentiment[, c("GI_en", "LM_en", "HENRY_en")],
                    sentiment$s5$sentiment[, c("GI_en", "LM_en", "HENRY_en")])
  expect_equivalent(sentiment$s6$sentiment[, -c(1:2)],
                    sentiment$s7$sentiment[, colnames(sentiment$s6$sentiment)[-c(1:2)], with = FALSE])
})

