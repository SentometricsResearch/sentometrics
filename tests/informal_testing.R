
### informal testing ###

require(plyr)
require(dplyr)
require(quanteda)
require(data.table)
require(zoo)

str <- c("./R/")
fileSources <- list.files(str, pattern = "*.R$", full.names = TRUE, ignore.case = TRUE)

sapply(fileSources[fileSources != "./R/_sentiment_calculation_example.R" & fileSources != "./R/sentmodel.R"], source, .GlobalEnv)

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

c <- corpus_sample(cAll, size = 500) # quanteda
summary(c) # quanteda

lexiconsIn <- setup_lexicons(lexicons, valence)
lexiconsIn2 <- setup_lexicons(lexicons, valence, split = TRUE)

###

# dfm <- dfm(c) # documents: ids (keys in corpus creation)
# dfm_w1 <- dfm_weight(dfm, type = "tfidf")

# words <- lexiconsIn$French_FEEL$x
# names(words) <- lexiconsIn$French_FEEL$x
# dict <- dictionary(as.list(words))
# dfm2 <- dfm(c, dictionary = dict) # very slow

# weights <- lexiconsIn$French_FEEL$y
# names(weights) <- words
# dfm_w2 <- dfm_weight(dfm, weights = weights) # weight of 1 for words not in lexicon, so need weights for which these are equal to 0

# head(dfm_select(dfm, features = words))

###

sent1 <- compute_sentiment(c, lexiconsIn, how = "equal-weight")
sent2 <- compute_sentiment(c, lexiconsIn, how = "proportional")
sent3 <- compute_sentiment(c, lexiconsIn, how = "tf-idf")
sent4 <- compute_sentiment(c, lexiconsIn, how = "counts")

aggDocs1 <- agg_documents(sent1, how = "equal-weight", ignoreZeros = FALSE)
# aggDocs1$sent

aggDocs2 <- agg_documents(sent1, how = "proportional")
# aggDocs2$sent

aggDocs3 <- agg_documents(sent1, how = "equal-weight", ignoreZeros = FALSE)
# aggDocs3$sent

aggDocs4 <- agg_documents(sent1, how = "proportional", ignoreZeros = TRUE)
# aggDocs4$sent

# al <- almons(n = 3, orders = 1:3, inverse = TRUE); al
# al2 <- almons(n = 30, orders = 1:5, inverse = TRUE); al2

# w1 <- setup_time_weights(3, how = "equal-weight"); w1
# w2 <- setup_time_weights(3, how = "almon", orders = 1:3, inverse = TRUE, normalize = TRUE); w2
# w3 <- setup_time_weights(3, how = "linear"); w3
# w4 <- setup_time_weights(3, how = "exponential", alphas = c(0.01, 0.1, 0.4, 0.7)); w4

sentMeas1 <- agg_time(aggDocs1, 3, how = "almon", list(orders = 1:3, inverse = TRUE, normalize = TRUE))
# sentMeas1
sentMeas2 <- agg_time(aggDocs1, 3, how = "equal-weight")
# sentMeas2

ctrlM1 <- ctrl_merge(sentMeas1,
                     lex = list(LEX = c("French_FEEL", "French_McDonald_GT")),
                     feat = list(GEN = c("GEN", "GENERAL"), FEAT2 = c("ECO", "POL")),
                     time = list(W1 = c("almon1", "almon1_inv"), W2 = c("almon2", "almon3")),
                     keep = FALSE)

ctrlM2 <- ctrl_merge(sentMeas1,
                     lex = list(LEX = c("French_FEEL", "French_McDonald_GT")),
                     feat = list(WRONG3 = c("Oops")),
                     time = list(WRONG1 = c("almon1", "almon1"), WRONG2 = c("almon1_INV")),
                     keep = TRUE) # produces error and warnings as it should

m1 <- merge_measures(ctrlM1)

ctrlAgg <- ctrl_agg(howWithin = "equal-weight",
                    howDocs = "equal-weight",
                    howTime = "almon",
                    ignoreZeros = FALSE,
                    lag = 3,
                    orders = 1:3,
                    inverse = TRUE,
                    normalize = TRUE)

sentMeas3 <- perform_agg(sent1, ctrlAgg)

sentMeas4 <- sentmeasures(c, lexiconsIn, ctrlAgg)

# sentMeas1$measures == sentMeas3$measures  # should be all TRUE
# sentMeas3$measures == sentMeas4$out$measures # should be all TRUE

mSel <- select_measures(m1, c("LEX", "FEAT2"))
# mSel <- select_measures(m1, c("LEX", "not_in_here", "wrong_again")) # produces error as it should

modeling <- FALSE
if (modeling) {
  nSample <- 350
  start <- 3
  oos <- 4
  h <- 3
  alphas <- seq(0, 1, by = 0.20)
  lambdas <- 10^seq(2, -2, length.out = 50)

  sentmeasures <- sentMeas1
  sentmeasures <- fill_measures(sentmeasures)

  y <- data.frame(y = 0.01 + runif(nrow(sentmeasures$measures), 0.01, 0.04))
  x <- data.frame(xOther = y$y)
  row.names(y) <- row.names(sentmeasures$measures) # row.names (dates) shouldn't be necessary, but assumes nrows are aligned
  trainWindow <- 340

  ctrlModel1 <- ctrl_model(model = "lm", type = "BIC", iter = FALSE, h = h, alphas = alphas, lambdas = lambdas)

  bicTest <- lm_sent(sentmeasures, y, x, ctrlModel1)

  ctrlModel2 <- ctrl_model(model = "lm", type = "cv", iter = FALSE, h = h, alphas = alphas, lambdas = lambdas,
                          oos = oos, trainWindow = trainWindow)

  cvTest <- lm_sent(sentmeasures, y, x, ctrlModel2)

  ctrlModel3 <- ctrl_model(model = "lm", type = "BIC", iter = TRUE, h = h, alphas = alphas, lambdas = lambdas,
                           nSample = nSample, start = start, oos = oos, trainWindow = NULL)

  bicTestIter <- lm_sent(sentmeasures, y, x, ctrlModel3)

  ### test cvTestIter + bicTestIter to check
}

###

example <- FALSE
if (example) {
  cAll <- corpuS(text_df)
  c <- corpus_sample(cAll, size = 500) # quanteda

  lexiconsIn <- setup_lexicons(lexicons, valence)

  # step-by-step
  sent <- compute_sentiment(c, lexiconsIn, how = "tf-idf")

  aggDocs <- agg_documents(sent, how = "equal-weight", ignoreZeros = FALSE)

  sentMeas <- agg_time(aggDocs, lag = 3, how = "almon", list(orders = 1:3, inverse = TRUE, normalize = TRUE))

  # via perform_agg()
  ctrlAgg <- ctrl_agg(howWithin = "tf-idf",
                      howDocs = "equal-weight",
                      howTime = "almon",
                      ignoreZeros = FALSE,
                      lag = 3,
                      orders = 1:3,
                      inverse = TRUE,
                      normalize = TRUE)

  sentMeas2 <- perform_agg(sent, ctrlAgg)

  # compute_sentiment() + perform_agg()
  sentMeas3 <- sentmeasures(c, lexiconsIn, ctrlAgg)

  # merge sentiment measures/indices
  ctrlMerge <- ctrl_merge(sentMeas,
                          lex = list(LEX = c("French_FEEL", "French_McDonald_GT")),
                          feat = list(GEN = c("GEN", "GENERAL"), FEAT2 = c("ECO", "POL")),
                          time = list(W1 = c("almon1", "almon1_inv"), W2 = c("almon2", "almon3")),
                          keep = FALSE)

  sentMeasMerged <- merge_measures(ctrlMerge)

  mSel <- select_measures(sentMeasMerged, c("LEX", "FEAT2"))
}

###

