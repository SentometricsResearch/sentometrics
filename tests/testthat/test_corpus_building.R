
library("sentometrics")
library("quanteda")
context("Corpus building")

# load built-in corpus
data("usnews")

### tests from here ###

# sento_corpus
corpus <- sento_corpus(corpusdf = usnews)
test_that("Corpus building works and fails when appropriate", {
  expect_equal(c("texts", "date", "wsj", "wapo", "economy", "noneconomy"), colnames(corpus$documents))
  expect_warning(corpusDummy <- sento_corpus(corpusdf = usnews[, 1:3]))
  expect_equal(colnames(quanteda::docvars(corpusDummy)), c("date", "dummy"))
  expect_warning(sento_corpus(corpusdf = cbind(usnews, "notNumeric")))
  expect_warning(sento_corpus(corpusdf = cbind(usnews[, 1:3], usnews[, -c(1:3)] * 100)))
  expect_error(sento_corpus(corpusdf = cbind(usnews[, 1:3], same = 0.5, same = 0.5, unique = 1.2)))
})

# to_sentocorpus
test_that("Conversion to sentocorpus from quanteda corpus", {
  expect_equal(
    c("texts", "date", "wsj", "wapo", "economy", "noneconomy"),
    colnames(to_sentocorpus(quanteda::corpus(usnews, text_field = "texts", docid_field = "id"), dates = usnews$date)$documents)
  )
  expect_warning(to_sentocorpus(
    quanteda::corpus(cbind(usnews, wrong = "nutNumeric"),  text_field = "texts", docid_field = "id"), dates = usnews$date))
})

# add_features
corpus1 <- add_features(corpus, featuresdf = data.frame(wsj = 1, wapo = 1, economy = 1, noneconomy = 1))
corpus2 <- add_features(corpus, keywords = list(good = "good", cut = c("cut|rates")),
                        do.regex = c(FALSE, TRUE), do.binary = FALSE)
test_that("Multiple additions of features to existing sentocorpus object", {
  expect_false(all(quanteda::docvars(corpus)[, -1] == quanteda::docvars(corpus1)[, -1]))
  expect_warning(add_features(corpus, keywords = list(wrong = "forSureNotHere")))
  expect_equal(c("wsj", "wapo", "economy", "noneconomy", "good", "cut"), colnames(quanteda::docvars(corpus2)[-1]))
  expect_error(add_features(corpus, keywords = list(cut = c("cut", "rates")), do.regex = TRUE))
  expect_true(max(quanteda::docvars(corpus2)[, c("good", "cut")]) <= 1)
  expect_true(min(quanteda::docvars(corpus2)[, c("good", "cut")]) >= 0)
})

