
context("Sentiment computation")

library("sentometrics")
library("quanteda")
library("tm")

set.seed(123)

# sento_corpus creation
data("usnews")
corpus <- sento_corpus(corpusdf = usnews[1:250, ])

# SimpleCorpus creation
txt <- system.file("texts", "txt", package = "tm")
scorp <- SimpleCorpus(DirSource(txt, encoding = "UTF-8"), control = list(language = "en"))
# scorp$content[1] <- "This is a text for which we want to calculate above average sentiment."
# scorp$content[2] <- "This is a text for which we want to calculate below average sentiment."
scorp$content[3] <- corpus$documents$text[3]

# VCorpus creation
reut21578 <- system.file("texts", "crude", package = "tm")
vcorp <- VCorpus(DirSource(reut21578, mode = "binary"))

# corpus with multiple languages
usnews[["language"]] <- "en"
usnews[["language"]][1:100] <- "fr"
corpusLang <- sento_corpus(corpusdf = usnews[1:250, ])

# lexicons creation
data("list_lexicons")
lex <- sento_lexicons(list_lexicons[c("GI_en", "LM_en", "HENRY_en")],
                      list_valence_shifters[["en"]])
# lexSimple <- sento_lexicons(list_lexicons[c("GI_en", "LM_en", "HENRY_en")]) # same as lex[1:3]
lexSplit <- sento_lexicons(list_lexicons[c("GI_en", "LM_en", "HENRY_en")], do.split = TRUE)
lexClust <- sento_lexicons(list_lexicons[c("GI_en", "LM_en", "HENRY_en")],
                           list_valence_shifters[["en"]][, c("x", "t")])
lEn <- sento_lexicons(list("HENRY_en" = list_lexicons$HENRY_en))
lFr <- sento_lexicons(list("HENRY_fr" = list_lexicons$HENRY_en))
lexLang <- list(en = lEn, fr = lFr)
lexWrong <- list(en = lEn, frr = lFr)

### tests from here ###

test_data <- readRDS(system.file("extdata", "test_data.rds", package = "sentometrics")) # benchmark sentiment scores

sentimentList <- list(
  s1 = compute_sentiment(quanteda::texts(corpus), lex, how = "counts"),
  s2 = compute_sentiment(quanteda::texts(corpus), lex[1:3], how = "counts"),
  s3 = compute_sentiment(quanteda::texts(corpus), lex, how = "proportional"),
  s4 = compute_sentiment(quanteda::texts(corpus), lex, how = "proportionalPol"),
  s5 = compute_sentiment(quanteda::corpus(usnews[1:250, "texts"]), lex, how = "counts"),
  s6 = compute_sentiment(quanteda::corpus(usnews[1:250, c("texts", "wsj", "economy")], text_field = "texts"),
                         lex, how = "counts"),
  s7 = compute_sentiment(corpus, lex, how = "counts"),
  s8 = compute_sentiment(quanteda::texts(corpus), lexSplit, how = "counts"),
  # s9 = compute_sentiment(quanteda::texts(corpus), lex, how = "TF", nCore = 2), # no multicore computation because of CRAN checks
  s10 = compute_sentiment(quanteda::texts(corpus), lexClust, how = "counts"),
  s11 = compute_sentiment(corpus, lexClust, how = "proportional"),
  s12 = compute_sentiment(quanteda::texts(corpus), lexClust, how = "proportionalPol"),
  s13 = compute_sentiment(corpus, lex, how = "exponential"),
  s14 = compute_sentiment(corpus, lex, how = "invertedExponential"),
  s15 = compute_sentiment(corpus, lex, how = "UShaped"),
  s16 = compute_sentiment(corpus, lex, how = "invertedUShaped"),
  s17 = compute_sentiment(corpus, lex, how = "TF"),
  s18 = compute_sentiment(corpus, lex, how = "logarithmicTF"),
  s19 = compute_sentiment(corpus, lex, how = "augmentedTF"),
  s20 = compute_sentiment(corpus, lex, how = "IDF"),
  s21 = compute_sentiment(corpus, lex, how = "TFIDF"),
  s22 = compute_sentiment(corpus, lex, how = "logarithmicTFIDF"),
  s23 = compute_sentiment(corpus, lex, how = "augmentedTFIDF"),
  s24 = compute_sentiment(corpusLang, lexLang, how = "squareRootCounts")
)

# compute_sentiment
test_that("Agreement between sentiment scores on document-level across input objects", {
  expect_true(all(unlist(lapply(sentimentList, function(s) nrow(s) == 250))))
  expect_true(all(unlist(lapply(sentimentList[-1], function(s) all(s$word_count == sentimentList$s1$word_count)))))
  expect_true(all(sentimentList$s8[, c("GI_en_POS", "LM_en_POS", "HENRY_en_POS")] >= 0))
  expect_true(all(sentimentList$s8[, c("GI_en_NEG", "LM_en_NEG", "HENRY_en_NEG")] <= 0))
  expect_equivalent(sentimentList$s1[, c("GI_en", "LM_en", "HENRY_en")],
                    sentimentList$s5[, c("GI_en", "LM_en", "HENRY_en")])
  expect_equivalent(sentimentList$s6[, -c(1:2)],
                    sentimentList$s7[, colnames(sentimentList$s6)[-c(1:2)], with = FALSE])
  expect_error(compute_sentiment(quanteda::texts(corpus), lex, how = "notAnOption"))
  expect_warning(compute_sentiment(quanteda::texts(corpus), lex, how = "counts", nCore = -1))
  expect_error(compute_sentiment(quanteda::texts(corpus), list_lexicons))
  expect_true(all.equal(sentimentList$s3[3, -1],
                        compute_sentiment(scorp[3], lex, how = "proportional")[, -1]))
  # expect_warning(compute_sentiment(vcorp, lex, how = "proportional"))
  expect_error(compute_sentiment(corpusLang, lex, how = "proportional"))
  expect_true("language" %in% colnames(quanteda::docvars(corpusLang)))
  expect_error(compute_sentiment(corpusLang, lexWrong, how = "proportional"))
  expect_true(all.equal(test_data, sentimentList[1:11])) # compare with old sentiment scores
})

sentimentSentenceList <- list(
  s1 = compute_sentiment(quanteda::texts(corpus), lexClust, how = "counts", do.sentence = TRUE),
  s2 = compute_sentiment(quanteda::corpus(usnews[1:250, "texts"]),
                         lexClust, how = "counts", do.sentence = TRUE),
  s3 = compute_sentiment(quanteda::corpus(usnews[1:250, c("texts", "wsj", "economy")], text_field = "texts"),
                         lexClust, how = "counts", do.sentence = TRUE),
  s4 = compute_sentiment(corpus, lexClust, how = "squareRootCounts", do.sentence = TRUE),
  s5 = compute_sentiment(corpusLang, lexLang, how = "proportional", do.sentence = TRUE)
)

test_that("Agreement between sentiment scores on sentence-level across input objects", {
  expect_true(all(unlist(lapply(sentimentSentenceList, function(s) nrow(s) == 2658))))
  expect_true(all(unlist(lapply(sentimentSentenceList[1:4], function(s)
    all(s$word_count == sentimentSentenceList$s1$word_count)))))
  expect_true(all(unlist(lapply(sentimentSentenceList, function(s)
    sum(s$word_count) == sum(sentimentSentenceList$s1$word_count)))))
  expect_true(all(c("GI_en", "LM_en", "HENRY_en") %in%
                    colnames(compute_sentiment(scorp[3], lexClust, how = "counts", do.sentence = TRUE))))
  # expect_warning(compute_sentiment(vcorp, lexClust, how = "proportional", do.sentence = TRUE))
})

# sento_lexicons
test_that("Proper fails when issues with lexicons and valence shifters input", {
  expect_error(sento_lexicons(list("heart--break--hotel" = list_lexicons[["LM_en"]], "good" = list_lexicons[["GI_en"]])))
  expect_error(sento_lexicons(list_lexicons["GI_en"], valenceIn = data.table(x = rep("w", 10))))
  expect_error(sento_lexicons(list_lexicons["GI_en"], valenceIn = data.table(x = "w", wrong = 1:3)))
  expect_error(sento_lexicons(list_lexicons["GI_en"], valenceIn = data.table(x = "w", t = 2:5)))
})

# as.sentiment
sA <- sAw1 <- sAw2 <- sAw3 <- sentimentList[["s7"]]
colnames(sAw1)[1:3] <- letters[1:3]
colnames(sAw2)[5:6] <- letters[1]
sAw3[[7]] <- "notNumeric"
test_that("Correct or failed conversion to a sentiment object", {
  expect_true(inherits(as.sentiment(sA), "sentiment"))
  expect_error(as.sentiment(sAw1))
  expect_error(as.sentiment(sAw2))
  expect_error(as.sentiment(sAw3))
})

# merge.sentiment
sB <- sA
sB$id <- paste0("idNew", 1:nrow(sB))
test_that("Correct binding of several sentiment objects", {
  expect_true(inherits(merge(sentimentList$s1, sentimentList$s2), "data.table"))
  expect_true(nrow(merge(sA, sB, sA)) == (2 * nrow(sA)))
  expect_true(ncol(merge(sentimentList$s7, sentimentList$s11)) == ncol(sentimentList$s7))
})

