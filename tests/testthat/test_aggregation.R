
library(Sentometrics)
context("Aggregation")

library(quanteda)
library(data.table)

# corpus and ctr creation
data <- USECONOMYNEWS
data$headline <- NULL

corpus <- corpus_sample(sento_corpus(texts = data), size = 1000)
lexicons <- setup_lexicons(c("LEXICON_GI_ENG", "LEXICON_LM_ENG"))
ctr <- ctr_agg(howWithin = "tf-idf", howDocs = "proportional", howTime = "almon",
               by = "month", lag = 3, orders = 1:3, do.inverse = TRUE, do.normalize = TRUE)

# tests from here
test_that("Number of columns coincide with provided dimensions", {
  expect_equal(ncol(sento_measures(corpus, lexicons, ctr)$measures) - 1, length(corpus$features) * 2 * 6)
})

