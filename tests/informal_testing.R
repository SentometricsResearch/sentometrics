
### informal testing ###

require(plyr)
require(dplyr)
require(quanteda)
require(data.table)
require(zoo)

load("DATA/test_corpus0.rda")

n <- 500
tester <- test_corpus0$fr[sample(nrow(test_corpus0$fr), n), ] # unordered by date

categs <- unique(tester$category)
for (el in categs) {
  if (el != "") {
    ones = rep(0, n)
    ones[which(tester$category == el)] <- 1 # make binary columns to check if text belongs to category
    tester[, el] <- ones
  } else next
}

tester2 <- tester[, c("id", "date", "body", categs[categs != ""])] # drop language for simplicity
tester2$date <- as.Date(tester2$date, format = "%d/%m/%Y")
names(tester2)[names(tester2) == "body"] <- "text"

c <- corpuS(tester2)

lexs <- c("French_FEEL_lexicon.csv", "French_McDonald_GT_lexicon.csv")
lexicons <- lapply(lexs, function(x) return(read.csv(paste0("LEXICON/", x), sep = ";")))
names(lexicons) <- gsub("_lexicon.csv", "", lexs)

lexicons <- lapply(lexicons, function(x) {x[, 1] <- stringr::str_to_lower(x[, 1]); return(x)})
lexicons <- lapply(lexicons, function(x) {x[, 1] <- stringr::str_trim(x[, 1]); return(x)})
# lexicons <- lapply(lexicons, function(x) {x[!(stri_detect_fixed(l[, 1], " ")), ]}) # keep only single words

###################################################################################################

lexiconsIn <- setup_lexicons(lexicons)

###

dfm <- dfm(c) # documents: ids (keys in corpus creation)
dfm_w1 <- dfm_weight(dfm, type = "tfidf")

words <- lexiconsIn$French_FEEL$x
names(words) <- lexiconsIn$French_FEEL$x
dict <- dictionary(as.list(words))

# dfm2 <- dfm(c, dictionary = dict) # very slow

weights <- lexiconsIn$French_FEEL$y
names(weights) <- words

dfm_w2 <- dfm_weight(dfm, weights = weights) # warning: weight of one for words not in lexicon, so need to be set to 0

dfm_w2P <- dfm_select(dfm_w2, features = words)
head(dfm_w2P)
head(dfm_select(dfm, features = words))

allFeat <- featnames(dfm)
polInd <- allFeat %in% words
polWords <- allFeat[polInd]
scores <- rep(0, length(allFeat))
scores[polInd] <- lexiconsIn$French_FEEL$y[words %in% polWords]
names(scores) <- allFeat # scores are weights with value of zero for non-polar words

dfm_w3 <- dfm_weight(dfm, weights = scores) # dfm with zeros for non-polar words and frequency * score for polar words
head(dfm_w3)
head(dfm_select(dfm_w3, features = words))
head(dfm_select(dfm, features = words))

dfm_cleaned <- dfm(tokenize(c, 
                            remove_punct = TRUE,
                            remove_numbers = TRUE,
                            remove_symbols = TRUE,
                            remove_separators = TRUE))

###

sent1 <- compute_sentiment(c, lexiconsIn, type = "word")
head(sent1$sent$dfmRaw)
head(sent1$sent$French_FEEL)
head(sent1$sent$French_McDonald_GT)

sent2 <- compute_sentiment(c, lexiconsIn, type = "sentence")

aggWords1 <- agg_words(sent1, how = "equal-weight")
aggWords1$sent # very low values relative to other two schemes (due to averaging)

aggWords2 <- agg_words(sent1, how = "tf-idf")
aggWords2$sent

aggWords3 <- agg_words(sent1, how = "proportional")
aggWords3$sent

agg_words(sent2) # produces error as it should

aggSents1 <- agg_sentences(sent2, how = "equal-weight")
aggSents1$sent

aggSents2 <- agg_sentences(sent2, how = "proportional")
aggSents2$sent

agg_sentences(sent1) # produces error as it should

aggDocs1 <- agg_documents(aggWords1, ignoreZeros = FALSE)
aggDocs1$sent

aggDocs2 <- agg_documents(aggWords1, ignoreZeros = TRUE)
aggDocs2$sent

aggDocs3 <- agg_documents(aggSents1, ignoreZeros = FALSE)
aggDocs3$sent

aggDocs4 <- agg_documents(aggSents1, ignoreZeros = TRUE)
aggDocs4$sent

al <- almons(n = 3, orders = 1:3, inverse = TRUE)
al2 <- almons(n = 30, orders = 1:5, inverse = TRUE)

w1 <- setup_time_weights(3, how = "equal-weight")
w2 <- setup_time_weights(3, how = "Almon", orders = 1:3, inverse = TRUE, normalize = TRUE)

(sentMat1 <- agg_time(aggDocs1, weights = w1)) # final date-index sentiment matrix
(sentMat2 <- agg_time(aggDocs3, weights = w2))
