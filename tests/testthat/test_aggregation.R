
context("Aggregation")

library("sentometrics")
library("quanteda")

set.seed(123)

# corpus, lexicon and aggregation control creation
data("usnews")
corpus <- quanteda::corpus_sample(sento_corpus(corpusdf = usnews), size = 1000)
data.table::setorder(corpus$documents, "date", na.last = FALSE)
data("list_lexicons")
lex <- sento_lexicons(list_lexicons[c("GI_en", "LM_en")])
lexClust <- sento_lexicons(list_lexicons[c("GI_en", "LM_en", "HENRY_en")],
                           list_valence_shifters[["en"]][, c("x", "t")])

### tests from here ###

ctr1 <- ctr_agg(howWithin = "proportionalPol", howDocs = "equal_weight", howTime = "almon", by = "month",
                lag = 5, ordersAlm = 1:3, do.inverseAlm = TRUE)
sentMeas1 <- sento_measures(corpus, lex, ctr1)

ctr2 <- ctr_agg(howWithin = "counts", howDocs = "proportional", howTime = c("equal_weight", "linear", "own"),
                by = "year", lag = 2, weights = data.frame(q1 = c(0.25, 0.75), q3 = c(0.75, 0.25)),
                do.ignoreZeros = FALSE, do.sentence = TRUE)
sentMeas2 <- sento_measures(corpus, lex, ctr2)

ctr3 <- ctr_agg(howWithin = "counts", howDocs = "inverseProportional", howTime = c("equal_weight", "own"),
                by = "year", lag = 3, weights = data.frame(GI_en = c(0.3, 0.6, 0.1)))

ctr4 <- ctr_agg(howWithin = "UShaped", howDocs = "inverseProportional", howTime = "linear", by = "day", lag = 20)

ctr5 <- ctr_agg(howWithin = "counts", howDocs = "exponential", alphaExpDocs = 0.2,
                howTime = "linear", by = "year", lag = 3)

ctr6 <- ctr_agg(howWithin = "augmentedTF", howDocs = "inverseExponential", alphaExpDocs = 0.1,
                howTime = "equal_weight", by = "week", lag = 7)

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

# aggregate.sentiment
s1 <- compute_sentiment(corpus, lex, how = "proportional")
s2 <- compute_sentiment(quanteda::texts(corpus), lex, how = "counts")
s3 <- compute_sentiment(corpus, lexClust, how = "squareRootCounts", do.sentence = TRUE)
sentimentAgg <- aggregate(s3, ctr_agg(lag = 7), do.full = FALSE)
test_that("Test input and output of sentiment aggregation function", {
  expect_true(inherits(s1, "sentiment"))
  expect_true(inherits(s2, "data.table"))
  expect_true(inherits(s3, "sentiment"))
  expect_true(inherits(aggregate(s1, ctr1), "sento_measures"))
  expect_true(inherits(aggregate(s3, ctr1), "sento_measures"))
  expect_true(inherits(aggregate(s3, ctr1, do.full = FALSE), "sentiment"))
  expect_error(aggregate(s2, ctr2))
  expect_error(sento_measures(corpus, lex, ctr3))
  expect_true(inherits(sento_measures(corpus, lex, ctr4), "sento_measures"))
  expect_true(inherits(sento_measures(corpus, lex, ctr5), "sento_measures"))
  expect_true(inherits(sento_measures(corpus, lex, ctr6), "sento_measures"))
  # expect_true(all.equal(sentimentAgg[["word_count"]], s1[["word_count"]]))
})

# peakdocs
test_that("Output for peak documents extraction in line with input", {
  expect_length(peakdocs(s1, n = 7, type = "both"), 7)
  expect_length(peakdocs(s1, n = 11, type = "pos"), 11)
  expect_length(peakdocs(s1, n = 1, type = "neg"), 1)
  expect_length(peakdocs(s1, n = 25, type = "both", do.average = TRUE), 25)
})

# peakdates
test_that("Output for peak dates extraction in line with input", {
  expect_length(peakdates(sentMeas1, n = 15, type = "both"), 15)
  expect_length(peakdates(sentMeas1, n = 21, type = "pos"), 21)
  expect_length(peakdates(sentMeas1, n = 4, type = "neg"), 4)
  expect_length(peakdates(sentMeas1, n = 10, type = "both", do.average = TRUE), 10)
})

