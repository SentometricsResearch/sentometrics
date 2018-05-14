
library("sentometrics")
library("quanteda")
context("Aggregation")

# corpus, lexicon and ctr creation
data("usnews")
corpus <- quanteda::corpus_sample(sento_corpus(corpusdf = usnews), size = 1000)
data("list_lexicons")
lex <- list_lexicons[c("GI_en", "LM_en")]
ctr <- ctr_agg(howWithin = "tf-idf", howDocs = "proportional", howTime = "almon", by = "month",
               lag = 3, ordersAlm = 1:3, do.inverseAlm = TRUE, do.normalizeAlm = TRUE)
sentMeas <- sento_measures(corpus, lex, ctr)

# tests from here
test_that("Number of columns coincide with provided dimensions", {
  expect_equal(ncol(sentMeas$measures) - 1, length(sentMeas$features) * length(lex) * length(sentMeas$time))
})

