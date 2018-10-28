
context("Aggregation")

library("sentometrics")
library("quanteda")

set.seed(123)

# corpus, lexicon and aggregation control creation
data("usnews")
corpus <- quanteda::corpus_sample(sento_corpus(corpusdf = usnews), size = 1000)

data("list_lexicons")
lex <- sento_lexicons(list_lexicons[c("GI_en", "LM_en")])

### tests from here ###

ctr1 <- ctr_agg(howWithin = "proportionalPol", howDocs = "equal_weight", howTime = "almon", by = "month",
               lag = 5, ordersAlm = 1:3, do.inverseAlm = TRUE)
sentMeas1 <- sento_measures(corpus, lex, ctr1)

ctr2 <- ctr_agg(howWithin = "counts", howDocs = "proportional", howTime = c("equal_weight", "linear", "own"), by = "year",
               lag = 2, weights = data.frame(q1 = c(0.25, 0.75), q3 = c(0.75, 0.25)), do.ignoreZeros = FALSE)
sentMeas2 <- sento_measures(corpus, lex, ctr2)

ctr3 <- ctr_agg(howWithin = "counts", howDocs = "proportional", howTime = c("equal_weight", "linear", "own"), by = "year",
                lag = 3, weights = data.frame(GI_en = c(0.3, 0.6, 0.1)))

# sento_measures
test_that("Number of columns coincide with provided dimensions", {
  expect_equal(nmeasures(sentMeas1), length(sentMeas1$features) * length(sentMeas1$lexicons) * length(sentMeas1$time))
  expect_equal(nmeasures(sentMeas2), length(sentMeas2$features) * length(sentMeas2$lexicons) * length(sentMeas2$time))
})

# ctr_agg
test_that("Aggregation control function breaks when wrong inputs supplied", {
  expect_error(ctr_agg(howWithin = c("oops", "again"), howDocs = c("mistake", "forYou"), howTime = "bad",
                       lag = 42, by = "infinitely", fill = "theMartiniPolice", nCore = c("yes", "man")))
  expect_error(ctr_agg(howTime = c("almon", "beta", "exponential"), lag = 0,
                       ordersAlm = -1:2, aBeta = -2, bBeta = -3, alphasExp = c(-1, -3)))
  expect_warning(ctr_agg(howTime = "linear", lag = 4, weights = data.frame(a = c(1/2, 1/2))))
  expect_error(ctr_agg(howTime = "own", lag = 12, weights = data.frame("dot--hacker" = rep(1/12, 12), check.names = FALSE)))
  expect_warning(ctr_agg(howTime = c("linear", "beta"), lag = 1))
})

# aggregate
s1 <- compute_sentiment(corpus, lex, how = "proportional")
s2 <- compute_sentiment(quanteda::texts(corpus), lex, how = "counts")
test_that("Test input format of sentiment aggregation function", {
  expect_true(inherits(s1, "sentiment"))
  expect_true(inherits(aggregate(s1, ctr1), "sentomeasures"))
  expect_error(aggregate(s2, ctr2))
  expect_error(sento_measures(corpus, lex, ctr3))
})

# peakdocs
test_that("Number of output dates for peak documents extraction in line with input", {
  expect_length(peakdocs(sentMeas1, corpus, n = 15, type = "both")[["dates"]], 15)
  expect_length(peakdocs(sentMeas1, corpus, n = 21, type = "pos")[["dates"]], 21)
  expect_length(peakdocs(sentMeas1, corpus, n = 4, type = "neg")[["dates"]], 4)
  expect_length(peakdocs(sentMeas1, corpus, n = 10, type = "both", do.average = TRUE)[["dates"]], 10)
})

