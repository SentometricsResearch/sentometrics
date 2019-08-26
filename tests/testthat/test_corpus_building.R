
context("Corpus building")

library("sentometrics")
library("quanteda")

# load built-in corpus
data("usnews")

### tests from here ###

# sento_corpus
corpus <- sento_corpus(corpusdf = usnews, do.clean = TRUE)
test_that("Corpus building works and fails when appropriate", {
  expect_equal(c("date", "wsj", "wapo", "economy", "noneconomy"),
               names(docvars(corpus)))
  expect_warning(corpusDummy <- sento_corpus(corpusdf = usnews[, 1:3]))
  expect_equal(colnames(quanteda::docvars(corpusDummy)), c("date", "dummyFeature"))
  expect_warning(sento_corpus(corpusdf = cbind(usnews, "notNumeric")))
  expect_warning(sento_corpus(corpusdf = cbind(usnews[, 1:3], usnews[, -c(1:3)] * 100)))
  expect_error(sento_corpus(corpusdf = cbind(usnews[, 1:3], same = 0.5, same = 0.5, unique = 1.2)))
  expect_error(sento_corpus(corpusdf = cbind(usnews, "-doesNotBelong-" = 0.5)))
})

# as.sento_corpus
test_that("Conversion to sento_corpus from quanteda corpus", {
  expect_equal(
    c("date", "wsj", "wapo", "economy", "noneconomy"),
    names(docvars(as.sento_corpus(quanteda::corpus(usnews, text_field = "texts", docid_field = "id"), dates = usnews$date)))
  )
  expect_warning(as.sento_corpus(
    quanteda::corpus(cbind(usnews, wrong = "nutNumeric"),  text_field = "texts", docid_field = "id"), dates = usnews$date))
})

# add_features
corpus1 <- add_features(corpus, featuresdf = data.frame(wsj = 1, wapo = 1, economy = 1, noneconomy = 1))
corpus2 <- add_features(corpus, keywords = list(good = "good", cut = c("cut|rates")),
                        do.regex = c(FALSE, TRUE), do.binary = FALSE)
test_that("Multiple additions of features to existing sento_corpus object", {
  expect_false(all(quanteda::docvars(corpus)[, -1] == quanteda::docvars(corpus1)[, -1]))
  expect_warning(add_features(corpus, keywords = list(wrong = "forSureNotHere")))
  expect_equal(c("wsj", "wapo", "economy", "noneconomy", "good", "cut"), colnames(quanteda::docvars(corpus2)[-1]))
  expect_error(add_features(corpus, keywords = list(cut = c("cut", "rates")), do.regex = TRUE))
  expect_error(add_features(corpus, keywords = list("cut--rates" = c("cut", "rates")), do.regex = TRUE))
  expect_true(max(quanteda::docvars(corpus2)[, c("good", "cut")]) <= 1)
  expect_true(min(quanteda::docvars(corpus2)[, c("good", "cut")]) >= 0)
})

# corpus summarize
summYear <- corpus_summarize(corpus, by = "year")
summMonth <- corpus_summarize(corpus, by = "month")
summWeek <- corpus_summarize(corpus, by = "week")
summDay <- corpus_summarize(corpus, by = "day")
test_that("Summary of sento_corpus object" , {
  expect_true(inherits(summYear$plots$feature_plot, "ggplot"))
  expect_true(inherits(summYear$plots$token_plot, "ggplot"))
  expect_true(length(summDay$stats$date) == 3043)
  expect_true(length(summWeek$stats$date) == 1009)
  expect_true(length(summMonth$stats$date) == 239)
  expect_true(length(summYear$stats$date) == 20)
  expect_true(all(c("date", "wsj", "economy", "noneconomy", "wapo", "minTokens", "maxTokens", "totalTokens", "documents")
                  %in% colnames(summMonth$stats), TRUE))
  expect_true(all(month(summYear$stats$date) == 1 , TRUE ))
})

