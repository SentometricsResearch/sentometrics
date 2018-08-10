
library("sentometrics")
library("quanteda")
context("Aggregation")

# corpus, lexicon and aggregation control creation
data("usnews")
corpus <- quanteda::corpus_sample(sento_corpus(corpusdf = usnews), size = 1000)

data("list_lexicons")
lex <- list_lexicons[c("GI_en", "LM_en")]

### tests from here ###

ctr1 <- ctr_agg(howWithin = "tf-idf", howDocs = "equal_weight", howTime = "almon", by = "month",
               lag = 5, ordersAlm = 1:3, do.inverseAlm = TRUE)
sentMeas1 <- sento_measures(corpus, lex, ctr1)

ctr2 <- ctr_agg(howWithin = "tf-idf", howDocs = "proportional", howTime = c("equal_weight", "linear", "own"), by = "year",
               lag = 2, weights = data.frame(q1 = c(0.25, 0.75), q3 = c(0.75, 0.25)))
sentMeas2 <- sento_measures(corpus, lex, ctr2)

# sento_measures
test_that("Number of columns coincide with provided dimensions", {
  expect_equal(nmeasures(sentMeas1), length(sentMeas1$features) * length(sentMeas1$lexicons) * length(sentMeas1$time))
  expect_equal(nmeasures(sentMeas2), length(sentMeas2$features) * length(sentMeas2$lexicons) * length(sentMeas2$time))
})

