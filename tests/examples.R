
############################################################
######################### Examples #########################
############################################################

data <- USECONOMYNEWS
data$headline <- NULL
data <- data[data$date >= "1988-01-01"]
corpus <- sento_corpus(texts = data)
# corpus <- corpus_sample(corpus, size = 5000)

lexicons <- setup_lexicons(c("LEXICON_GI_ENG", "LEXICON_LM_ENG"))

ctr <- ctr_agg(howWithin = "tf-idf", howDocs = "proportional", howTime = "almon", by = "month",
               lag = 3, orders = 1:3, do.inverse = TRUE, do.normalize = TRUE)

# step-by-step
sent <- compute_sentiment(corpus, lexicons, how = "tf-idf")
aggDocs <- agg_documents(sent, by = "year", how = "proportional", do.ignoreZeros = FALSE)
sentMeas <- agg_time(aggDocs, lag = 3, how = "equal-weight")

# at once
sentMeas <- sento_measures(corpus, lexicons, ctr)

sentMeas <- fill_measures(sentMeas)
plot(sentMeas)

y <- SP500
nrow(y) == nrow(sentMeas$measures) # TRUE

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

