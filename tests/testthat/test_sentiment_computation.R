
context("Sentiment computation")

library("sentometrics")
library("data.table")
library("quanteda")
library("tm")

set.seed(123)

# sento_corpus creation
data("usnews")
corpus <- sento_corpus(corpusdf = usnews[1:250, ])

# SimpleCorpus creation
txt <- system.file("texts", "txt", package = "tm")
scorp <- tm::SimpleCorpus(tm::DirSource(txt))
# scorp$content[1] <- "A text for which we want to calculate above average sentiment."
# scorp$content[2] <- "A text for which we want to calculate below average sentiment."
scorp$content[3] <- quanteda::texts(corpus)[3]

# VCorpus creation
reuters <- system.file("texts", "crude", package = "tm")
vcorp <- tm::VCorpus(tm::DirSource(reuters))

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
lEn <- sento_lexicons(list("HENRY_en" = list_lexicons$HENRY_en), list_valence_shifters$en)
lFr <- sento_lexicons(list("HENRY_fr" = list_lexicons$HENRY_en, "FEEL" = list_lexicons$FEEL_fr))
lexLang <- lexWrong <- list(en = lEn, fr = lFr)
names(lexWrong)[2] <- "frr"

### tests from here ###

load(system.file("extdata", "test_data.rda", package = "sentometrics")) # benchmark sentiment scores

sanity_sentiment <- function(texts, lexicon, valence = NULL) {
  setkey(lexicon, "x")
  if (!is.null(valence)) setkey(valence, "x")

  out <- rep(NA, length(texts))
  for (i in seq_along(texts)) {
    x <- texts[i]
    tok <- stringi::stri_split_boundaries(
      stringi::stri_trans_tolower(x), type = "word", skip_word_none = TRUE, skip_word_number = TRUE
    )[[1]]
    lo <- which(tok %in% lexicon[["x"]])
    m <- tok[lo]
    sc <- lexicon[m, y]
    before <- sapply(lo - 1, max, 1)
    vals <- rep(1, length(sc))
    if (!is.null(valence)) {
      val <- which(tok[before] %in% valence$x)
      v <- tok[before][val]
      vals[val] <- valence[v, y]
    }
    ss <- sum(sc * vals)
    out[i] <- ss
  }

  out
}

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
  # s9 = compute_sentiment(quanteda::texts(corpus), lex, how = "TF", nCore = 2), # no multicore computation in CRAN checks
  s10 = compute_sentiment(quanteda::texts(corpus), lexClust, how = "counts"),
  s11 = compute_sentiment(corpus, lexClust, how = "proportional"),
  s12 = compute_sentiment(quanteda::texts(corpus), lexClust, how = "proportionalPol"),
  s13 = compute_sentiment(corpus, lex, how = "exponential"),
  s14 = compute_sentiment(corpus, lex, how = "inverseExponential"),
  s15 = compute_sentiment(corpus, lex, how = "UShaped"),
  s16 = compute_sentiment(corpus, lex, how = "inverseUShaped"),
  # s17 = compute_sentiment(corpus, lex, how = "TF"),
  # s18 = compute_sentiment(corpus, lex, how = "logarithmicTF"),
  # s19 = compute_sentiment(corpus, lex, how = "augmentedTF"),
  # s20 = compute_sentiment(corpus, lex, how = "IDF"),
  s21 = compute_sentiment(corpus, lex, how = "TFIDF"),
  # s22 = compute_sentiment(corpus, lex, how = "logarithmicTFIDF"),
  # s23 = compute_sentiment(corpus, lex, how = "augmentedTFIDF"),
  s24 = compute_sentiment(corpusLang, lexLang, how = "proportionalSquareRoot")
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
  expect_true(all.equal(sentimentList$s1$GI_en, sanity_sentiment(quanteda::texts(corpus), lex$GI_en, lex$valence)))
  expect_true(all.equal(sentimentList$s2$GI_en, sanity_sentiment(quanteda::texts(corpus), lex$GI_en)))
})

sentimentSentenceList <- list(
  s1 = compute_sentiment(quanteda::texts(corpus), lexClust, how = "counts", do.sentence = TRUE),
  s2 = compute_sentiment(quanteda::corpus(usnews[1:250, "texts"]),
                         lexClust, how = "counts", do.sentence = TRUE),
  s3 = compute_sentiment(quanteda::corpus(usnews[1:250, c("texts", "wsj", "economy")], text_field = "texts"),
                         lexClust, how = "counts", do.sentence = TRUE),
  s4 = compute_sentiment(corpus, lexClust, how = "proportionalSquareRoot", do.sentence = TRUE),
  s5 = compute_sentiment(corpusLang, lexLang, how = "proportional", do.sentence = TRUE),
  s6 = compute_sentiment(corpus, lex[1:3], how = "TFIDF", do.sentence = TRUE),
  s7 = compute_sentiment(corpus, lex, how = "inverseUShaped", do.sentence = TRUE)
)

# test_that("Agreement between sentiment scores on sentence-level across input objects", {
#   expect_true(all(unlist(lapply(sentimentSentenceList, function(s) nrow(s) == 2658))))
#   expect_true(all(unlist(lapply(sentimentSentenceList[1:4], function(s)
#     all(s$word_count == sentimentSentenceList$s1$word_count)))))
#   expect_true(all(unlist(lapply(sentimentSentenceList, function(s)
#     sum(s$word_count) == sum(sentimentSentenceList$s1$word_count)))))
#   expect_true(all(c("GI_en", "LM_en", "HENRY_en") %in%
#                     colnames(compute_sentiment(scorp[3], lexClust, how = "counts", do.sentence = TRUE))))
# })

# sento_lexicons
test_that("Proper fails when issues with lexicons and valence shifters input", {
  expect_error(sento_lexicons(list("heart--break--hotel" = list_lexicons[["LM_en"]], "good" = list_lexicons[["GI_en"]])))
  expect_error(sento_lexicons(list_lexicons["GI_en"], valenceIn = data.table(x = rep("w", 10))))
  expect_error(sento_lexicons(list_lexicons["GI_en"], valenceIn = data.table(x = "w", wrong = 1:3)))
  expect_error(sento_lexicons(list_lexicons["GI_en"], valenceIn = data.table(x = "w", t = 2:5)))
  expect_error(sento_lexicons(list_lexicons$FEEL_nl_tr))
  expect_error(sento_lexicons(list(list_lexicons$LM_en, list_lexicons$GI_en)))
  expect_error(sento_lexicons(list(a = list_lexicons[[1]], b = list_lexicons[[2]], a = list_lexicons[[3]])))
  expect_error(sento_lexicons(list_lexicons[1:3], valenceIn = letters))
})

test_that("Proper fails when trying to modify a sento_lexicons object", {
  expect_error(lex["valence"])
  expect_error(lex[0])
  expect_error(lex[length(lex) + 1])
  expect_error(lex[1] <- lexSplit[3])
  expect_error(lex[[1]] <- lexSplit[[1]])
  expect_error(lex$HENRY_en <- lexSplit$HENRY_en_POS)
  expect_error(names(lex)[1] <- names(lex)[2])
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

# tf-idf comparison sentometrics vs. quanteda
toks <- stri_split_boundaries(stri_trans_tolower(texts(corpus)), type = "word", skip_word_none = TRUE)
dfmQ <- dfm(as.tokens(toks)) %>% dfm_tfidf(k = 1)
posScores <- rowSums(dfm_select(dfmQ, lex$GI_en[y == 1, x]))
negScores <- rowSums(dfm_select(dfmQ, lex$GI_en[y == -1, x]))
test_that("Same tf-idf scoring for sentometrics and quanteda", {
  expect_equal(compute_sentiment(texts(corpus), lex[-length(lex)], tokens = toks, "TFIDF")[["GI_en"]],
               unname(posScores - negScores))
})

