
############################################################
######################### Examples #########################
############################################################

require(Sentometrics)

###############################
######### AGGREGATION #########
###############################

data <- USECONOMYNEWS
data$headline <- NULL
data <- data[data$date >= "1988-01-01", ]
corpus <- sento_corpus(texts = data)
# corpus <- quanteda::corpus_sample(corpus, size = 5000)

lexicons <- setup_lexicons(c("LEXICON_GI_ENG", "LEXICON_LM_ENG"))

ctr <- ctr_agg(howWithin = "tf-idf", howDocs = "proportional", howTime = "almon", by = "month",
               lag = 3, orders = 1:3, do.inverse = TRUE, do.normalize = TRUE)

# step-by-step
sent <- compute_sentiment(corpus, lexicons, how = "tf-idf")
# aggDocs <- agg_documents(sent, by = "month", how = "proportional", do.ignoreZeros = FALSE) # internal
# sentMeas <- agg_time(aggDocs, lag = 3, how = "equal-weight") # internal

# at once
sentMeas <- sento_measures(corpus, lexicons, ctr)
plot(sentMeas)

ctrMerge <- ctr_merge(sentMeas, time = list(W1 = c("almon1", "almon1_inv"), W2 = c("almon2", "almon3")),
                      lex = list(LEX = c("LEXICON_GI_ENG", "LEXICON_LM_ENG")), feat = list(journals = c("wsj", "wapo")),
                      do.keep = FALSE)

sentMeasMerged <- merge_measures(ctrMerge)
plot(sentMeasMerged)

measSel <- select_measures(sentMeasMerged, c("LEX", "wrong")) # error
measSel <- select_measures(sentMeasMerged, c("LEX", "W1", "W2")) # warning
measSel <- select_measures(sentMeasMerged, c("W1", "W2"), do.all = FALSE)
measSel <- select_measures(sentMeasMerged, c("LEX", "W1"))
plot(measSel)

###############################
########## MODELLING ##########
###############################

### LINEAR ###

y <- as.numeric(SP500) # make sure it is a numeric vector (will be coerced to data.frame)
sentMeas <- fill_measures(sentMeas)
length(y) == nrow(sentMeas$measures) # TRUE

# require(doParallel)
# registerDoParallel(2)

out <- list()
for (ic in c("BIC", "AIC", "Cp")) {
  ctrIC <- ctr_model(model = "lm", type = ic, do.iter = FALSE, h = 0, alphas = seq(0, 1, by = 0.10))
  out[[ic]] <- sento_model(sentMeas, y, ctr = ctrIC)
}

ctrIC <- ctr_model(model = "lm", type = "BIC", do.iter = TRUE, h = 0, nSample = 315, do.progress = TRUE)
out <- sento_model(sentMeas, y, ctr = ctrIC)

ctrCV <- ctr_model(model = "lm", type = "cv", do.iter = FALSE, h = 0, trainWindow = 250, testWindow = 10, oos = 3)
out <- sento_model(sentMeas, y, ctr = ctrCV)

ctrCV <- ctr_model(model = "lm", type = "cv", do.iter = TRUE, h = 0, trainWindow = 250, testWindow = 3, oos = 0, nSample = 320)
out <- sento_model(sentMeas, y, ctr = ctrCV)

### LOGISTIC (binomial) ###

yb <- y
yb <- ifelse(yb >= 0, 1, -1)
yb <- as.factor(yb)
# p <- as.numeric(yb == 1)
# n <- as.numeric(yb == -1)
# yb <- matrix(c(n, p), ncol = 2)

