
### informal testing ###

require(plyr)
require(dplyr)
require(quanteda)
require(data.table)
require(zoo)

load("DATA/test_corpus0.rda")

tester <- test_corpus0$fr # unordered by date

categs <- unique(tester$category)
for (el in categs) {
  if (el != "") {
    ones = rep(0, nrow(tester))
    ones[which(tester$category == el)] <- 1 # make binary columns to check if text belongs to category
    tester[, el] <- ones
  } else next
}

tester2 <- tester[, c("id", "date", "body", categs[categs != ""])] # drop language for simplicity
tester2$date <- as.Date(tester2$date, format = "%d/%m/%Y")
names(tester2)[names(tester2) == "body"] <- "text"

lexs <- c("French_FEEL_lexicon.csv", "French_McDonald_GT_lexicon.csv")
lexicons <- lapply(lexs, function(x) return(read.csv(paste0("LEXICON/", x), sep = ";")))
names(lexicons) <- gsub("_lexicon.csv", "", lexs)

lexicons <- lapply(lexicons, function(x) {x[, 1] <- stringr::str_to_lower(x[, 1]); return(x)})
lexicons <- lapply(lexicons, function(x) {x[, 1] <- stringr::str_trim(x[, 1]); return(x)})
# lexicons <- lapply(lexicons, function(x) {x[!(stri_detect_fixed(l[, 1], " ")), ]}) # keep only single words

valence <- read.csv("LEXICON/valence_French.csv", sep = ";")

###################################################################################################

cAll <- corpuS(tester2)

cPol <- corpus_subset(cAll, POL == 1) # quanteda
cGen <- corpus_subset(cAll, GEN == 1 | GENERAL == 1 | ALG == 1) # quanteda

c <- corpus_sample(c, size = 500) # quanteda
summary(c) # quanteda

lexiconsIn <- setup_lexicons(lexicons, valence)

###

# dfm <- dfm(c) # documents: ids (keys in corpus creation)
# dfm_w1 <- dfm_weight(dfm, type = "tfidf")

# words <- lexiconsIn$French_FEEL$x
# names(words) <- lexiconsIn$French_FEEL$x
# dict <- dictionary(as.list(words))
# dfm2 <- dfm(c, dictionary = dict) # very slow

# weights <- lexiconsIn$French_FEEL$y
# names(weights) <- words
# dfm_w2 <- dfm_weight(dfm, weights = weights) # weight of one for words not in lexicon, so need to be set to 0

# dfm_w2P <- dfm_select(dfm_w2, features = words)
# head(dfm_w2P)
# head(dfm_select(dfm, features = words))

# allFeat <- featnames(dfm)
# polInd <- allFeat %in% words
# polWords <- allFeat[polInd]
# scores <- rep(0, length(allFeat))
# scores[polInd] <- lexiconsIn$French_FEEL$y[words %in% polWords]
# names(scores) <- allFeat # scores are weights with value of zero for non-polar words

# dfm_w3 <- dfm_weight(dfm, weights = scores) # dfm with zeros for non-polar words and frequency * score for polar words
# head(dfm_w3)
# head(dfm_select(dfm_w3, features = words))
# head(dfm_select(dfm, features = words))

# dfm_cleaned <- dfm(tokenize(c, remove_punct = T, remove_numbers = T, remove_symbols = T, remove_separators = T))

###

sent1 <- compute_sentiment(c, lexiconsIn, type = "words")
# head(sent1$sent$dfmRaw)
# head(sent1$sent$French_FEEL)
# head(sent1$sent$French_McDonald_GT)

sent2 <- compute_sentiment(c, lexiconsIn, type = "sentences")

aggWords1 <- agg_words(sent1, how = "equal-weight")
# aggWords1$sent

aggWords2 <- agg_words(sent1, how = "tf-idf")
# aggWords2$sent

aggWords3 <- agg_words(sent1, how = "proportional")
# aggWords3$sent

agg_words(sent2) # produces error as it should

aggSents1 <- agg_sentences(sent2, how = "equal-weight")
# aggSents1$sent

aggSents2 <- agg_sentences(sent2, how = "proportional")
# aggSents2$sent

agg_sentences(sent1) # produces error as it should

aggDocs1 <- agg_documents(aggWords1, how = "equal-weight", ignoreZeros = FALSE)
# aggDocs1$sent

aggDocs2 <- agg_documents(aggWords1, how = "proportional")
# aggDocs2$sent

aggDocs3 <- agg_documents(aggSents1, how = "equal-weight", ignoreZeros = FALSE)
# aggDocs3$sent

aggDocs4 <- agg_documents(aggSents1, how = "proportional", ignoreZeros = TRUE)
# aggDocs4$sent

# al <- almons(n = 3, orders = 1:3, inverse = TRUE); al
# al2 <- almons(n = 30, orders = 1:5, inverse = TRUE); al2

# w1 <- setup_time_weights(3, how = "equal-weight"); w1
# w2 <- setup_time_weights(3, how = "almon", orders = 1:3, inverse = TRUE, normalize = TRUE); w2
# w3 <- setup_time_weights(3, how = "linear"); w3
# w4 <- setup_time_weights(3, how = "exponential", alphas = c(0.01, 0.1, 0.4, 0.7)); w4

sentMat1 <- agg_time(aggDocs1, 3, how = "almon", list(orders = 1:3, inverse = TRUE, normalize = TRUE))
# sentMat1
sentMat2 <- agg_time(aggDocs1, 3, how = "equal-weight")
# sentMat2

ctrlM1 <- ctrl_merge(sentMat1,
                     lex = list(LEX = c("French_FEEL", "French_McDonald_GT")),
                     feat = list(GEN = c("GEN", "GENERAL"), FEAT2 = c("ECO", "POL")),
                     time = list(W1 = c("almon1", "almon1_inv"), W2 = c("almon2", "almon3")),
                     keep = FALSE)

ctrlM2 <- ctrl_merge(sentMat1,
                     lex = list(LEX = c("French_FEEL", "French_McDonald_GT")),
                     feat = list(WRONG3 = c("Oops")),
                     time = list(WRONG1 = c("almon1", "almon1"), WRONG2 = c("almon1_INV")),
                     keep = TRUE) # produces error and warnings as it should

m1 <- merge_matrix(ctrlM1)

ctrlAgg <- ctrl_agg(sentType = "words",
                    howWithin = "equal-weight",
                    howDocs = "equal-weight",
                    howTime = "almon",
                    ignoreZeros = FALSE,
                    lag = 3,
                    orders = 1:3,
                    inverse = TRUE,
                    normalize = TRUE)

sentMat3 <- perform_agg(sent1, ctrlAgg)

sentMat1$sent == sentMat3$sent # should be all TRUE

# test sentmeasures constructor

