
############################################################
##################### Informal testing #####################
############################################################

require(sentometrics)

str <- c("./R/")
sources <- list.files(str, pattern = "*.R$", full.names = TRUE, ignore.case = TRUE)
sapply(sources, source, .GlobalEnv)

load("nonpackage/examples (Sentometrics package)/belga.rda") # BELGA 2016 news data
tester <- belga$fr # unordered by date

categs <- unique(tester$category)
for (el in categs) {
  if (el != "") {
    ones = rep(0, nrow(tester))
    ones[which(tester$category == el)] <- 1 # make binary columns to check if text belongs to category
    tester[, el] <- ones
  } else next
}

testerIn <- tester[, c("id", "date", "body", categs[categs != ""])] # drop language for simplicity
testerIn$date <- as.Date(testerIn$date, format = "%d/%m/%Y")
names(testerIn)[names(testerIn) == "body"] <- "text"

lexNames <- c("FEEL.csv", "LM_fr.csv")
lex <- lapply(lexNames, function(x) return(read.csv(paste0("data-raw/lexicons-raw/", x), sep = ";")))
lex <- lapply(lex, function(x) {x[, 1] <- stringr::str_to_lower(x[, 1]); return(x)})
lex <- lapply(lex, function(x) {x[, 1] <- stringr::str_trim(x[, 1]); return(x)})
# lexicons <- lapply(lexicons, function(x) {x[!(stringi::stri_detect_fixed(l[, 1], " ")), ]}) # keep only single words
names(lex) <- gsub(".csv", "", lexNames)

valence <- read.csv("data-raw/valence-raw/NEGATORS_fr.csv", sep = ";")

###################################################################################################

cAll <- sento_corpus(testerIn)

cPol <- quanteda::corpus_subset(cAll, POL == 1)
cGen <- quanteda::corpus_subset(cAll, GEN == 1 | GENERAL == 1 | ALG == 1)

c <- quanteda::corpus_sample(cAll, size = 500)
summary(c)

data("lexicons")
lexiconsIn <- setup_lexicons(lexicons[c("FEEL_fr", "LM_fr_tr")])
lexiconsIn2 <- setup_lexicons(lex, valence)
lexiconsIn3 <- setup_lexicons(lex, valence, do.split = TRUE)

###

# dfm <- dfm(c) # documents: ids (keys in corpus creation)
# dfm_w1 <- dfm_weight(dfm, type = "tfidf")

# words <- lexiconsIn$French_FEEL$x
# names(words) <- lexiconsIn$French_FEEL$x
# dict <- dictionary(as.list(words))
# dfm2 <- dfm(c, dictionary = dict) # very slow

# weights <- lexiconsIn$French_FEEL$y
# names(weights) <- words
# dfm_w2 <- dfm_weight(dfm, weights = weights) # weight of 1 for words not in lexicon, needs to be 0

# head(dfm_select(dfm, features = words))

###

sent1 <- compute_sentiment(c, lexiconsIn, how = "equal-weight")
sent2 <- compute_sentiment(c, lexiconsIn, how = "proportional")
sent3 <- compute_sentiment(c, lexiconsIn, how = "tf-idf")
sent4 <- compute_sentiment(c, lexiconsIn, how = "counts")

aggDocs1 <- agg_documents(sent1, by = "day", how = "equal-weight", do.ignoreZeros = FALSE)
aggDocs2 <- agg_documents(sent1, by = "week", how = "proportional")
aggDocs3 <- agg_documents(sent1, by = "month", how = "equal-weight", do.ignoreZeros = FALSE)
aggDocs4 <- agg_documents(sent1, by = "year", how = "proportional", do.ignoreZeros = TRUE)

# al <- almons(n = 3, orders = 1:3, inverse = TRUE); al
# al2 <- almons(n = 30, orders = 1:5, inverse = TRUE); al2

# w1 <- setup_time_weights(3, how = "equal-weight"); w1
# w2 <- setup_time_weights(3, how = "almon", orders = 1:3, inverse = TRUE, normalize = TRUE); w2
# w3 <- setup_time_weights(3, how = "linear"); w3
# w4 <- setup_time_weights(3, how = "exponential", alphas = c(0.01, 0.1, 0.4, 0.7)); w4

sentMeas1 <- agg_time(aggDocs1, lag = 10, how = "almon", list(ordersAlm = 1:3, do.inverseAlm = TRUE, do.normalizeAlm = TRUE))
sentMeas2 <- agg_time(aggDocs2, 3, how = "equal-weight")
sentMeas3 <- agg_time(aggDocs3, 3, how = "exponential", list(alphasExp = seq(0.1, 0.5, by = 0.1)))
sentMeas4 <- agg_time(aggDocs4, 1, how = "equal-weight")
sentMeas5 <- agg_time(aggDocs1, lag = 3, how = "own", list(weights = data.frame(w1 = c(0.5, 0.3, 0.2),
                                                                                   w2 = c(0.3, 0.3, 0.4))))

ctrM1 <- ctr_merge(sentMeas1,
                   lex = list(LEX = c("FEEL_fr", "LM_fr_tr")),
                   feat = list(GEN = c("GEN", "GENERAL"), FEAT2 = c("ECO", "POL")),
                   time = list(W1 = c("almon1", "almon1_inv"), W2 = c("almon2", "almon3")),
                   do.keep = FALSE)

ctrM2 <- ctr_merge(sentMeas1,
                   lex = list(LEX = c("FEEL_fr", "LM_fr_tr")),
                   feat = list(WRONG3 = c("Oops")),
                   time = list(WRONG1 = c("almon1", "almon1"), WRONG2 = c("almon1_INV")),
                   do.keep = TRUE) # produces error and warnings as it should

m1 <- merge_measures(ctrM1)

ctrAgg <- ctr_agg(howWithin = "equal-weight",
                  howDocs = "equal-weight",
                  howTime = "almon",
                  do.ignoreZeros = FALSE,
                  lag = 3,
                  by = "month",
                  orders = 1:3,
                  do.inverse = TRUE,
                  do.normalize = TRUE)

ctrAgg2 <- ctr_agg(howWithin = "equal-weight",
                   howDocs = "equal-weight",
                   howTime = "equal-weight", # automatically set to "own"
                   do.ignoreZeros = FALSE,
                   by = "week",
                   orders = 1:3,
                   do.inverse = TRUE,
                   do.normalize = TRUE,
                   weights = data.frame(w1 = c(0.5, 0.3, 0.2),
                                        w2 = c(0.3, 0.3, 0.4)))

sentMeas6 <- perform_agg(sent1, ctrAgg)
sentMeas7 <- sento_measures(c, lexiconsIn, ctrAgg)
sentMeas8 <- perform_agg(sent1, ctrAgg2)

# sentMeas1$measures == sentMeas3$measures  # should be all TRUE
# sentMeas3$measures == sentMeas4$out$measures # should be all TRUE

mSel <- select_measures(m1, c("LEX", "FEAT2"))
# mSel <- select_measures(m1, c("LEX", "not_in_here", "wrong_again")) # produces error as it should

plot(sentMeas2)
plot(m1)
plot(mSel)

fill1 <- fill_measures(sentMeas2)
fill2 <- fill_measures(sentMeas2, fill = c("latest"))

