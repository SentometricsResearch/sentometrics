
context("Sentiment computation")

library("sentometrics")
library("quanteda")
library("tm")

set.seed(123)

load(system.file("extdata", "testdata.rda", package = "sentometrics")) # data as comparison for testing

# corpus and lexicon creation
data("usnews")
corpus <- sento_corpus(corpusdf = usnews[1:250, ])

data("list_lexicons")
lex <- sento_lexicons(list_lexicons[c("GI_en", "LM_en", "HENRY_en")], list_valence_shifters[["en"]])
# lexSimple <- sento_lexicons(list_lexicons[c("GI_en", "LM_en", "HENRY_en")]) # same as lex[1:3]
lexSplit <- sento_lexicons(list_lexicons[c("GI_en", "LM_en", "HENRY_en")], do.split = TRUE)
lexClust <- sento_lexicons(list_lexicons[c("GI_en", "LM_en", "HENRY_en")], list_valence_shifters[["en"]][, c("x", "t")])

# VCorpus and SimpleCorpus
sp_texts <- system.file("texts", "txt", package = "tm")
simple_corpus <- SimpleCorpus(DirSource(sp_texts, encoding = "UTF-8"), control = list(language = "en"))
simple_corpus$content[1] <- "This is a text for which we want to calculate above average sentiment."
simple_corpus$content[2] <- "This is a text for which we want to calculate below average sentiment."
simple_corpus$content[3] <- corpus$documents$text[3]

reut21578 <- system.file("texts", "crude", package = "tm")
v_corpus <- VCorpus(DirSource(reut21578, mode = "binary"), list(reader = readReut21578XMLasPlain))

# Corpus with multiple languages
usnews[["language"]] <- "en"
usnews$language[1:100] <- "fr"
corpus_lang <- sento_corpus(corpusdf = usnews[1:250, ])

lexIn1 <- list("HENRY_en" = list_lexicons$HENRY_en)
l_en <- sento_lexicons(lexIn1)
lexIn2 <- list("HENRY_fr" = list_lexicons$HENRY_en)
l_fr <- sento_lexicons(lexIn2)
lexicons <- list(en = l_en, fr = l_fr )
lexicons_wrong <- list(en = l_en, frr = l_fr)

#sentence based computation


### tests from here ###



sentimentList <- list( ### TODO: check equality of sentiment scores with mydata.rda
  s1 = compute_sentiment(quanteda::texts(corpus), lex, how = "counts"),
  s2 = compute_sentiment(quanteda::texts(corpus), lex[1:3], how = "counts"),
  s3 = compute_sentiment(quanteda::texts(corpus), lex, how = "proportional"),
  s4 = compute_sentiment(quanteda::texts(corpus), lex, how = "proportionalPol"),
  s5 = compute_sentiment(quanteda::corpus(usnews[1:250, "texts"]), lex, how = "counts"),
  s6 = compute_sentiment(quanteda::corpus(usnews[1:250, c("texts", "wsj", "economy")], text_field = "texts"),
                         lex, how = "counts"),
  s7 = compute_sentiment(corpus, lex, how = "counts"),
  s8 = compute_sentiment(quanteda::texts(corpus), lexSplit, how = "counts"),
  # s9 = compute_sentiment(quanteda::texts(corpus), lex, how = "proportionalPol", nCore = 2),
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
  s24 = compute_sentiment(corpus, lex, how = "squareRootCounts", nCore = 4),
  s25 = compute_sentiment(corpus_lang, lexicons, how ="proportional")
)

# compute_sentiment
test_that("Agreement between sentiment scores across input objects", {
  expect_true(all(unlist(lapply(sentimentList, function(s) nrow(s) == 250))))
  expect_true(all(unlist(lapply(sentimentList[1:23], function(s) all(s$word_count == sentimentList$s1$word_count)))))
  expect_true(all(sentimentList$s8[, c("GI_en_POS", "LM_en_POS", "HENRY_en_POS")] >= 0))
  expect_true(all(sentimentList$s8[, c("GI_en_NEG", "LM_en_NEG", "HENRY_en_NEG")] <= 0))
  expect_equivalent(sentimentList$s1[, c("GI_en", "LM_en", "HENRY_en")],
                    sentimentList$s5[, c("GI_en", "LM_en", "HENRY_en")])
  expect_equivalent(sentimentList$s6[, -c(1:2)],
                    sentimentList$s7[, colnames(sentimentList$s6)[-c(1:2)], with = FALSE])
  expect_error(compute_sentiment(quanteda::texts(corpus), lex, how = "notAnOption"))
  expect_warning(compute_sentiment(quanteda::texts(corpus), lex, how = "counts", nCore = -1))
  expect_error(compute_sentiment(quanteda::texts(corpus), list_lexicons))
  expect_true(all.equal(sentimentList$s3[3],compute_sentiment(simple_corpus[3], lex, how = "proportional")))
  expect_warning(compute_sentiment(v_corpus , lex, how = "proportional"))
  expect_error(compute_sentiment(corpus_lang, lex, how = "proportional"))
  expect_true("language" %in% colnames(quanteda::docvars(corpus_lang)))
  expect_error(compute_sentiment(corpus_lang, lexicons_wrong, how = "proportional"))
})

sentimentSentenceList <- list(
  s1 = compute_sentiment(quanteda::texts(corpus), lexClust, how = "counts", do.sentence = TRUE ),
  s2 = compute_sentiment(quanteda::corpus(usnews[1:250, "texts"]), lexClust, how = "counts", do.sentence = TRUE),
  s3 = compute_sentiment(quanteda::corpus(usnews[1:250, c("texts", "wsj", "economy")], text_field = "texts"),
                         lexClust, how = "counts", do.sentence = TRUE),
  s4 = compute_sentiment(corpus, lexClust, how = "exponential", do.sentence = TRUE),
  s5 = compute_sentiment(corpus_lang, lexicons, how ="proportional", do.sentence = TRUE)
)

test_that("Agreement between sentiment scores on sentence level across input objects", {
  expect_true(all(unlist(lapply(sentimentSentenceList, function(s) nrow(s) == 2658))))
  expect_true(all(unlist(lapply(sentimentSentenceList[1:4], function(s) all(s$word_count == sentimentSentenceList$s1$word_count)))))
  expect_true(all(unlist(lapply(sentimentSentenceList, function(s) sum(s$word_count) == sum(sentimentSentenceList$s1$word_count)))))
  expect_true(all(c("GI_en", "LM_en", "HENRY_en") %in% colnames(compute_sentiment(simple_corpus[3], lexClust, how = "proportional", do.sentence = TRUE)) ))
  expect_warning(compute_sentiment(v_corpus , lexClust, how = "proportional", do.sentence = TRUE))
  })

# sento_lexicons
test_that("Proper fails when issues with lexicons and valence shifters input", {
  expect_error(sento_lexicons(list("heart--break--hotel" = list_lexicons[["LM_en"]], "good" = list_lexicons[["GI_en"]])))
  expect_error(sento_lexicons(list_lexicons["GI_en"], valenceIn = data.table(x = rep("w", 10))))
  expect_error(sento_lexicons(list_lexicons["GI_en"], valenceIn = data.table(x = "w", wrong = 1:3)))
  expect_error(sento_lexicons(list_lexicons["GI_en"], valenceIn = data.table(x = "w", t = 2:5)))
})

# to_sentiment
sA <- sAw1 <- sAw2 <- sAw3 <- sentimentList[["s7"]]
colnames(sAw1)[1:3] <- letters[1:3]
colnames(sAw2)[5:6] <- letters[1]
sAw3[[7]] <- "notNumeric"
test_that("Correct or failed conversion to a sentiment object", {
  expect_true(inherits(to_sentiment(sA), "sentiment"))
  expect_error(to_sentiment(sAw1))
  expect_error(to_sentiment(sAw2))
  expect_error(to_sentiment(sAw3))
})

# sentiment_bind
sB <- sA
sB$id <- paste0("idNew", 1:nrow(sB))
test_that("Correct binding of several sentiment objects", {
  expect_true(nrow(sentiment_bind(sA, sB, sA)) == (2 * nrow(sA)))
  expect_true(ncol(sentiment_bind(sentimentList$s7, sentimentList$s11)) == ncol(sentimentList$s7))
  expect_error(sentiment_bind(sentimentList$s1, sentimentList$s2))
})

#sentence sentiment
sentiment <- compute_sentiment_by_sentence(corpus, lexClust, how = "squareRootCounts")
agg_sentiment <-aggregate_sentences(sentiment)
wc <-cbind(agg_sentiment[,"word_count"], sentimentList$s1[,"word_count"])
test_that("Check word count after aggregation", {
  expect_true(all.equal(wc[,1], wc[,2]))
})

