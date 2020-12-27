
context("Measures manipulation")

library("data.table")
library("quanteda")

set.seed(123)

# corpus, lexicon and aggregation control creation
data("usnews")
corpus <- quanteda::corpus_sample(sento_corpus(corpusdf = usnews), size = 1000)

data("list_lexicons")
lex <- sento_lexicons(list_lexicons[c("GI_en", "LM_en")])
ctr <- ctr_agg(howWithin = "proportionalPol", howDocs = "proportional", by = "week", lag = 21, do.ignoreZeros = TRUE,
               howTime = c("almon", "beta", "linear"), ordersAlm = 1:3, do.inverseAlm = TRUE, aBeta = 1:3, bBeta = 1:2)

sentMeas <- sento_measures(corpus, lex, ctr)

### tests from here ###

dims <- get_dimensions(sentMeas)

# deletion
sentMeasDelete <- subset(sentMeas, delete = list("wsj", c("linear", "wapo"), "beta31"))
test_that("Consistency of deletion through subset() function", {
  expect_false("wsj" %in% sentMeasDelete$features)
  expect_equal(dims$lexicons, sentMeasDelete$lexicons)
  expect_false("beta31" %in% sentMeasDelete$time)
  expect_false(any(stringi::stri_detect(colnames(sentMeasDelete$measures)[-1], regex = "wapo--linear")))
  expect_warning(subset(sentMeas, delete = dims$features))
  expect_warning(subset(sentMeas, delete = dims$lexicons))
  expect_warning(subset(sentMeas, delete = dims$time))
  expect_false("beta31" %in% colnames(sentMeasDelete$attribWeights$B))
  expect_true(all(unlist(sapply(sentMeasDelete$lexicons, function(l) paste0(l, "--", sentMeasDelete$features))) %in%
                    colnames(sentMeasDelete$attribWeights$W)[-c(1:2)]))
  expect_length(sentMeasDelete$attribWeights$W[[1]], 1000)
})

# fill
dates <- get_dates(sentMeas)
sentMeasFill <- measures_fill(sentMeas, fill = "zero", dateBefore = dates[1] - 24, dateAfter = tail(dates, 1) + 45)
test_that("Consistency of measures_fill() function", {
  expect_true(nobs(sentMeasFill) > nobs(sentMeas))
  expect_true(get_dates(sentMeasFill)[1] < dates[1])
  expect_true(tail(get_dates(sentMeasFill), 1) > tail(dates, 1))
  expect_identical(sentMeas$attribWeights$B, sentMeasFill$attribWeights$B)
  expect_identical(sentMeas$attribWeights$W, sentMeasFill$attribWeights$W)
  expect_length(sentMeasFill$attribWeights$W[[1]], 1000)
})

# aggregate.sento_measures
sentMeasMerge <- aggregate(
  sentMeas,
  features = list(ECO = c("economy", "noneconomy")),
  lexicons = list(LEX = c("GI_en", "LM_en")),
  time = list(B2 = c("beta21", "beta22"), A1 = c("almon1", "almon1_inv"), A3 = c("almon3", "almon3_inv"))
)
dimsMerge <- get_dimensions(sentMeasMerge)
test_that("Consistency of aggregate.sento_measures() function", {
  expect_equal(nmeasures(sentMeasMerge), (length(dims$features) - 1) * (length(dims$lexicons) - 1) * (length(dims$time) - 3))
  expect_true(all(dimsMerge$features %in% c("wsj", "wapo", "ECO")))
  expect_true(all(dimsMerge$lexicons %in% c("LEX")))
  expect_true(all(dimsMerge$time %in%
                    c("linear", "beta11", "beta12", "B2", "beta31", "beta32", "A1", "almon2", "almon2_inv", "A3")))
  expect_error(aggregate(sentMeas, features = list(journals = c("notInHere", "wapo"))))
  expect_error(aggregate(sentMeas, lexicons = list(journals = c("LM_en", "notInHere"))))
  expect_error(aggregate(sentMeas, time = list(journals = c("linear", "notInHere", "beta12"))))
  expect_true(all(colnames(sentMeasMerge$attribWeights$B) %in%
                    c("linear", "beta11", "beta12", "B2", "beta31", "beta32", "A1", "almon2", "almon2_inv", "A3")))
  expect_true(all(colnames(sentMeasMerge$attribWeights$W)[-c(1:2)] %in% c("LEX--wsj", "LEX--wapo", "LEX--ECO")))
  expect_length(sentMeasMerge$attribWeights$W[[1]], 1000)
})

# selection
sentMeasSelect <- subset(sentMeas, select = list(c("wsj", "almon3", "LM_en"), c("wapo", "almon2_inv")))
test_that("Consistency of selection through subset() function", {
  expect_false(all(dims$features %in% sentMeasSelect$features))
  expect_equal(dims$lexicons, sentMeasSelect$lexicons)
  expect_true(all(sentMeasSelect$time %in% c("almon3", "almon2_inv")))
  expect_equal(nmeasures(sentMeasSelect), 1 + length(sentMeasSelect$lexicons))
  expect_equal(nmeasures(subset(sentMeas, select = dims$features)), nmeasures(sentMeas))
  expect_equal(nmeasures(subset(sentMeas, select = dims$lexicons)), nmeasures(sentMeas))
  expect_equal(nmeasures(subset(sentMeas, select = dims$time)), nmeasures(sentMeas))
  expect_true(all(colnames(sentMeasSelect$attribWeights$B) %in% c("almon2_inv", "almon3")))
  expect_true(all(colnames(sentMeasSelect$attribWeights$W)[-c(1:2)] %in%
                    c("LM_en--wsj", "LM_en--wapo", "GI_en--wapo")))
  expect_length(sentMeasSelect$attribWeights$W[[1]], 1000)
})

# subsetting
sentMeasSubset <- subset(sentMeas, date > "2003-05-04" & GI_en--wsj--almon1_inv <= 0)
test_that("Consistency of subsetting through subset() function", {
  expect_true(min(get_dates(sentMeasSubset)) >= "2003-05-04")
  expect_true(max(as.data.table(sentMeasSubset)[["GI_en--wsj--almon1_inv"]]) <= 0)
  expect_true(nobs(sentMeasSubset) < nobs(sentMeas))
  expect_equal(nmeasures(sentMeasSubset), nmeasures(sentMeas))
  expect_warning(subset(sentMeas, date > "2017-08-12"))
  expect_identical(sentMeas$attribWeights$B, sentMeasSubset$attribWeights$B)
  expect_identical(sentMeas$attribWeights$W, sentMeasSubset$attribWeights$W)
  expect_length(sentMeasSubset$attribWeights$W[[1]], 1000)
})

# measures_update
corpus1 <- quanteda::corpus_sample(sento_corpus(corpusdf = usnews[1:100,]))
corpus2 <- quanteda::corpus_sample(sento_corpus(corpusdf = usnews[101:200,]))
sentMeas <- sento_measures(corpus1, lex, ctr)
sentMeasUpd <- measures_update(sento_corpus = corpus2 , sentMeas, lexicon = lex)
test_that("Sentomeasure update works properly", {
  expect_true(length(sentMeas$sentiment$word_count) == 100)
  expect_true(length(sentMeasUpd$sentiment$word_count) == 200)
})

