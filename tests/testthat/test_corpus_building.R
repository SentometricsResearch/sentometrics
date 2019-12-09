
context("Corpus building")

library("sentometrics")
library("data.table")
library("quanteda")
library("tm")

# load corpus data
data("usnews")
txt <- system.file("texts", "txt", package = "tm")
reuters <- system.file("texts", "crude", package = "tm")

### tests from here ###

corpus <- sento_corpus(corpusdf = usnews, do.clean = TRUE)

# sento_corpus
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

colnames(usnews)[c(1, 3)] <- c("doc_id", "text") # original usnews data not used any further
tmSCdf <- tm::SimpleCorpus(tm::DataframeSource(usnews))
tmSCdir <- tm::SimpleCorpus(tm::DirSource(txt))
tmVCdf <- tm::VCorpus(tm::DataframeSource(usnews))
tmVCdir <- tm::VCorpus(tm::DirSource(reuters), list(reader = tm::readReut21578XMLasPlain))
tmVCdir_decomp <- tm::VCorpus(tm::DirSource(reuters))
test_that("Conversion to sento_corpus from tm corpora", {
  expect_true(inherits(as.sento_corpus(tmSCdf), "sento_corpus"))
  expect_error(as.sento_corpus(tmSCdir))
  expect_true(inherits(suppressWarnings(as.sento_corpus(tmSCdir, dates = usnews$date[1:5])), "sento_corpus"))
  expect_true(inherits(as.sento_corpus(tmVCdf), "sento_corpus"))
  expect_error(as.sento_corpus(tmVCdir))
  expect_true(inherits(suppressWarnings(as.sento_corpus(tmVCdir, dates = usnews$date[1:20])), "sento_corpus"))
  expect_error(as.sento_corpus(tmVCdir_decomp))
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

# as.data.table, as.data.frame
dt <- as.data.table(corpus)
df <- as.data.frame(corpus)
test_that("Proper data.table and data.frame conversion", {
  expect_true(inherits(dt, "data.table"))
  expect_true(all(colnames(dt)[1:3] == c("id", "date", "texts")))
  expect_true(class(df) == "data.frame")
  expect_true(all(colnames(df)[1:2] == c("date", "texts")))
  expect_true(all(colnames(dt)[-1] == colnames(df)))
})

# quanteda corpus_*() functions
res <- quanteda::corpus_reshape(corpus, to = "sentences")
sam <- quanteda::corpus_sample(corpus, 100)
seg <- quanteda::corpus_segment(corpus, pattern = "stock", use_docvars = TRUE)
sub <- quanteda::corpus_subset(corpus, wsj == 1)
tri <- quanteda::corpus_trim(corpus, "documents", min_ntoken = 300)
trs <- quanteda::corpus_trim(corpus, "sentences", min_ntoken = 40)
# trs <- quanteda::corpus_trimsentences(corpus, min_length = 40)
test_that("quanteda corpus_*() functions keep sento_corpus object intact when expected", {
  expect_true(inherits(res, "sento_corpus"))
  expect_true(inherits(sam, "sento_corpus"))
  # expect_true(inherits(as.sento_corpus(sam), "sento_corpus"))
  expect_true(inherits(seg, "sento_corpus"))
  expect_true(inherits(sub, "sento_corpus"))
  expect_true(inherits(tri, "sento_corpus"))
  expect_true(inherits(trs, "sento_corpus"))
})

