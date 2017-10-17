
library(sentometrics)
context("Aggregation")

library(quanteda)
library(data.table)

# corpus, lexicon and ctr creation
data("useconomynews")
corpus <- corpus_sample(sento_corpus(corpusdf = useconomynews), size = 1000)
data("lexicons")
lex <- lexicons[c("GI_eng", "LM_eng")]
ctr <- ctr_agg(howWithin = "tf-idf", howDocs = "proportional", howTime = "almon", by = "month",
               lag = 3, ordersAlm = 1:3, do.inverseAlm = TRUE, do.normalizeAlm = TRUE)
sentMeas <- sento_measures(corpus, lex, ctr)

# tests from here
test_that("Number of columns coincide with provided dimensions", {
  expect_equal(ncol(sentMeas$measures) - 1, length(sentMeas$features) * length(lex) * length(sentMeas$time))
})

