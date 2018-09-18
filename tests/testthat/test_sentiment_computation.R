
context("Sentiment computation")

library("sentometrics")
library("quanteda")

set.seed(123)

# corpus and lexicon creation
data("usnews")
corpus <- sento_corpus(corpusdf = usnews[1:250, ])

data("list_lexicons")
lex <- sento_lexicons(list_lexicons[c("GI_en", "LM_en", "HENRY_en")], list_valence_shifters[["en"]])
# lexSimple <- sento_lexicons(list_lexicons[c("GI_en", "LM_en", "HENRY_en")]) # same as lex[1:3]
lexSplit <- sento_lexicons(list_lexicons[c("GI_en", "LM_en", "HENRY_en")], do.split = TRUE)
lexClust <- sento_lexicons(list_lexicons[c("GI_en", "LM_en", "HENRY_en")], list_valence_shifters[["en"]][, c("x", "t")])

### tests from here ###

sentimentList <- list(
  s1 = compute_sentiment(quanteda::texts(corpus), lex, how = "counts"),
  s2 = compute_sentiment(quanteda::texts(corpus), lex[1:3], how = "counts"),
  s3 = compute_sentiment(quanteda::texts(corpus), lex, how = "proportional"),
  s4 = compute_sentiment(quanteda::texts(corpus), lex, how = "proportionalPol", nCore = 2),
  s5 = compute_sentiment(quanteda::corpus(usnews[1:250, "texts"]), lex, how = "counts"),
  s6 = compute_sentiment(quanteda::corpus(usnews[1:250, c("texts", "wsj", "economy")], text_field = "texts"),
                         lex, how = "counts"),
  s7 = compute_sentiment(corpus, lex, how = "counts"),
  s8 = compute_sentiment(quanteda::texts(corpus), lexSplit, how = "counts"),
  s9 = compute_sentiment(quanteda::texts(corpus), lex, how = "proportionalPol", nCore = 1),
  s10 = compute_sentiment(quanteda::texts(corpus), lexClust, how = "counts"),
  s11 = compute_sentiment(corpus, lexClust, how = "proportional"),
  s12 = compute_sentiment(quanteda::texts(corpus), lexClust, how = "proportionalPol")
)

# compute_sentiment
test_that("Agreement between sentiment scores across input objects", {
  expect_true(all(unlist(lapply(sentimentList, function(s) nrow(s$sentiment) == 250))))
  expect_true(all(unlist(lapply(sentimentList, function(s) all(s$sentiment$word_count
                                                               == sentimentList$s1$sentiment$word_count)))))
  expect_true(all(sentimentList$s8[, c("GI_en_POS", "LM_en_POS", "HENRY_en_POS")] >= 0))
  expect_true(all(sentimentList$s8[, c("GI_en_NEG", "LM_en_NEG", "HENRY_en_NEG")] <= 0))
  expect_true(all(sentimentList$s4 == sentimentList$s9))
  expect_equivalent(sentimentList$s1[, c("GI_en", "LM_en", "HENRY_en")],
                    sentimentList$s5[, c("GI_en", "LM_en", "HENRY_en")])
  expect_equivalent(sentimentList$s6[, -c(1:2)],
                    sentimentList$s7$sentiment[, colnames(sentimentList$s6)[-c(1:2)], with = FALSE])
  expect_error(compute_sentiment(quanteda::texts(corpus), lex, how = "notAnOption"))
  expect_warning(compute_sentiment(quanteda::texts(corpus), lex, how = "counts", nCore = -1))
  expect_error(compute_sentiment(quanteda::texts(corpus), list_lexicons))
})

# sento_lexicons
test_that("Proper fails when issues with lexicons and valence shifters input", {
  expect_error(sento_lexicons(list("heart--break--hotel" = list_lexicons[["LM_en"]], "good" = list_lexicons[["GI_en"]])))
  expect_error(sento_lexicons(list_lexicons["GI_en"], valenceIn = data.table(x = rep("w", 10))))
  expect_error(sento_lexicons(list_lexicons["GI_en"], valenceIn = data.table(x = "w", wrong = 1:3)))
  expect_error(sento_lexicons(list_lexicons["GI_en"], valenceIn = data.table(x = "w", t = 2:4)))
})

