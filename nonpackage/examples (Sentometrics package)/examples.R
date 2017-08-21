
############################################################
######################### Examples #########################
############################################################

require(sentometrics)

###############################
######### AGGREGATION #########
###############################

data("useconomynews")
useconomynews <- useconomynews[date >= "1988-01-01", ]

corpus <- sento_corpus(corpusdf = useconomynews)
corpusSmall <- quanteda::corpus_sample(corpus, size = 500)
corpusExt <- add_features(corpus, featuresdf = data.frame(random = runif(quanteda::ndoc(corpus))))

data("lexicons")
data("valence")

lexIn <- c(list(myLexicon = data.frame(w = c("good", "nice", "boring"), s = c(1, 2, -1))), lexicons[c("GI_eng")])
valIn <- valence[["valence_eng"]]
lex <- setup_lexicons(lexIn, valIn)
lex <- setup_lexicons(lexIn, valIn, do.split = TRUE)
lex <- lexicons[c("GI_eng", "LM_eng")]

ctr <- ctr_agg(howWithin = "tf-idf", howDocs = "proportional", howTime = c("equal_weight", "linear", "almon"),
               by = "month", lag = 3, ordersAlm = 1:3, do.inverseAlm = TRUE, do.normalizeAlm = TRUE)

# step-by-step
sent <- compute_sentiment(corpusSmall, lex, how = "tf-idf")
sent <- compute_sentiment(corpus, lex, how = "equal_weight")
# aggDocs <- agg_documents(sent, by = "month", how = "equal_weight", do.ignoreZeros = FALSE) # internal
# sentMeas <- agg_time(aggDocs, lag = 3, c("equal_weight", "linear")) # internal

# at once
sentMeas <- sento_measures(corpus, lex, ctr)
p <- plot(sentMeas)

ctrMerge <- ctr_merge(sentMeas, time = list(W1 = c("almon1", "almon1_inv"), W2 = c("almon2", "almon3")),
                      lex = list(LEX = c("GI_eng", "LM_eng")), feat = list(journals = c("wsj", "wapo")),
                      do.keep = FALSE)

sentMeasMerged <- merge_measures(ctrMerge)
plot(sentMeasMerged)

scaled <- scale(sentMeasMerged)
plot(scaled)

measSel <- select_measures(sentMeasMerged, c("LEX", "wrong")) # error
measSel <- select_measures(sentMeasMerged, c("LEX", "W1", "W2")) # warning
measSel <- select_measures(sentMeasMerged, c("W1", "W2"), do.combine = FALSE)
measSel <- select_measures(sentMeasMerged, c("LEX", "W1"))
plot(measSel)

global <- merge_to_global(sentMeas)
plot(global)

global <- merge_to_global(sentMeas, lex = c(0.30, 0.70), feat = c(0.20, 0.30, 0.40, 0.10), time = 1)
plot(global)

###############################
########## MODELLING ##########
###############################

### LINEAR ###

data("sp500")

y <- sp500$return # convert to numeric vector
sentMeas <- fill_measures(sentMeas)
sentMeas <- fill_measures(sentMeas, fill = "latest")
length(y) == nrow(sentMeas$measures) # TRUE

n <- 100
sentMeas2 <- sentMeas
sentMeas2$measures <- tail(sentMeas2$measures, n)
yl <- tail(y, n)
x <- data.frame(runif(length(y)), rnorm(length(y))) # two other (random) x variables
colnames(x) <- c("x1", "x2")
xs <- tail(x, n)

# require(doParallel)
# registerDoParallel(2)

out <- list()
for (ic in c("BIC", "AIC", "Cp")) {
  ctrIC <- ctr_model(model = "lm", type = ic, do.iter = FALSE, h = 0, alphas = seq(0, 1, by = 0.10))
  out[[ic]] <- sento_model(sentMeas, y, x = x, ctr = ctrIC)
}

ctrIC <- ctr_model(model = "lm", type = "BIC", do.iter = TRUE, h = 0, nSample = n - 2, do.progress = TRUE)
out <- sento_model(sentMeas2, yl, x = xs, ctr = ctrIC)

ctrCV <- ctr_model(model = "lm", type = "cv", do.iter = FALSE, h = 0, trainWindow = 250, testWindow = 10, oos = 3)
out <- sento_model(sentMeas, y, x = x, ctr = ctrCV)

ctrCV <- ctr_model(model = "lm", type = "cv", do.iter = TRUE, h = 0, trainWindow = 45, testWindow = 4, oos = 0,
                   nSample = n - 2, do.progress = TRUE)
out <- sento_model(sentMeas2, yl, ctr = ctrCV)

### LOGISTIC (binomial) ###

yb <- tail(sp500$up, n)
ctrCV <- ctr_model(model = "binomial", type = "cv", do.iter = TRUE, h = 0, trainWindow = 45, testWindow = 2, oos = 0,
                   nSample = n - 2, do.progress = TRUE)
out <- sento_model(sentMeas2, yb, x = xs, ctr = ctrCV)

### LOGISTIC (multinomial) ###

ym <- tail(sp500$upMulti, n)
ctrCV <- ctr_model(model = "multinomial", type = "cv", do.iter = TRUE, h = 0, trainWindow = 45, testWindow = 2, oos = 0,
                   nSample = n - 2, do.progress = TRUE)
out <- sento_model(sentMeas2, ym, ctr = ctrCV)

