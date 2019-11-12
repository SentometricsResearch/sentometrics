
#################################################
################# VIGNETTE CODE #################
#################################################

###### DESCRIPTION ######

### This code is used in the vignette paper 'The R Package sentometrics to Compute,
### Aggregate and Predict with Textual Sentiment' (Ardia, Bluteau, Borms and Boudt, 2019).
### See the paper for the results and setup details.
### Download the package and its dependencies first before you run this script...
### install.packages("sentometrics", dependencies = TRUE) # from CRAN (version 0.7.5), OR
### install.packages("sentometrics_0.7.6.tar.gz", repos = NULL, dependencies = TRUE) # from the tar

### Dependencies can be installed separately like this:
### install.packages(
###   c("covr", "doParallel", "e1071", "NLP", "randomForest",
###     "testthat", "tm", "caret", "data.table", "foreach",
###     "ggplot2", "glmnet", "ISOweek", "quanteda", "Rcpp", "RcppRoll",
###     "RcppParallel", "stringi", "RcppArmadillo")
### )

###### SESSION INFO ######

### R version 3.6.1 (2019-07-05)
### Platform: x86_64-w64-mingw32/x64 (64-bit)
### Running under: Windows 10 x64 (build 18362)
###
### Matrix products: default
###
### locale:
### [1] LC_COLLATE=English_Belgium.1252  LC_CTYPE=English_Belgium.1252    LC_MONETARY=English_Belgium.1252
### [4] LC_NUMERIC=C                     LC_TIME=English_Belgium.1252
###
### attached base packages:
### [1] stats     graphics  grDevices utils     datasets  methods   base
###
### other attached packages:
### [1] zoo_1.8-4          lubridate_1.7.4    stm_1.3.3          quanteda_1.5.1     lexicon_1.2.1      gridExtra_2.3
### [7] ggplot2_3.2.1      data.table_1.12.6  sentometrics_0.7.6 testthat_2.2.1
###
### loaded via a namespace (and not attached):
### [1] Rcpp_1.0.2         lattice_0.20-38    prettyunits_1.0.2  class_7.3-15       ps_1.3.0           assertthat_0.2.0
### [7] glmnet_2.0-18      rprojroot_1.3-2    digest_0.6.20      packrat_0.5.0      ipred_0.9-8        foreach_1.4.7
### [13] R6_2.4.0           plyr_1.8.4         backports_1.1.3    stats4_3.6.1       pillar_1.3.1       rlang_0.4.0
### [19] lazyeval_0.2.2     caret_6.0-84       rstudioapi_0.10    callr_3.3.1        rpart_4.1-15       Matrix_1.2-17
### [25] desc_1.2.0         devtools_2.1.0     splines_3.6.1      gower_0.1.2        stringr_1.4.0      ISOweek_0.6-2
### [31] munsell_0.5.0      spacyr_1.0         compiler_3.6.1     pkgconfig_2.0.2    pkgbuild_1.0.3     nnet_7.3-12
### [37] tidyselect_0.2.5   tibble_2.1.3       prodlim_2018.04.18 codetools_0.2-16   RcppRoll_0.3.0     crayon_1.3.4
### [43] dplyr_0.8.3        withr_2.1.2        MASS_7.3-51.4      recipes_0.1.6      ModelMetrics_1.2.2 grid_3.6.1
### [49] nlme_3.1-141       gtable_0.3.0       magrittr_1.5       scales_1.0.0       RcppParallel_4.4.4 cli_1.1.0
### [55] stringi_1.4.3      reshape2_1.4.3     fs_1.3.1           remotes_2.1.0      syuzhet_1.0.4      timeDate_3043.102
### [61] stopwords_0.9.0    generics_0.0.2     fastmatch_1.1-0    lava_1.6.5         iterators_1.0.12   tools_3.6.1
### [67] glue_1.3.0         purrr_0.3.0        processx_3.4.1     pkgload_1.0.2      survival_2.44-1.1  colorspace_1.4-0
### [73] sessioninfo_1.1.1  memoise_1.1.0      usethis_1.5.1

remove(list = ls())
options(prompt = "R> ", continue = "+  ", width = 120, digits = 4, max.print = 90, useFancyQuotes = FALSE)
sink(file = "output_vignette.txt", append = FALSE, split = TRUE) # output printed in .txt file

library("sentometrics")

library("data.table")
library("ggplot2")
library("gridExtra")
library("lexicon")
library("quanteda")
library("stm")
library("lubridate")
library("zoo")

info <- sessionInfo()
print(info)
cat("\n")

set.seed(505)

##################
###### CODE ######
##################

cat("### SECTION 3.1 ####################### \n \n")

library("sentometrics")

data("usnews", package = "sentometrics")
class(usnews)
cat("\n")

head(usnews[, -3])
cat("\n")

usnews[["texts"]][2029]
cat("\n")

uscorpus <- sento_corpus(usnews)
class(uscorpus)
cat("\n")

regex <- c("\\bRepublic[s]?\\b|\\bDemocrat[s]?\\b|\\belection\\b")
uscorpus <- add_features(uscorpus,
                         keywords = list(uncertainty = c("uncertainty", "distrust"), election = regex),
                         do.binary = TRUE,
                         do.regex = c(FALSE, TRUE))
tail(quanteda::docvars(uscorpus))
cat("\n")

summ <- corpus_summarize(uscorpus, by = "year")
pCorp <- summ$plots$feature_plot + guides(color = guide_legend(nrow = 1))
pCorp

cat("### SECTION 3.2 ####################### \n \n")

data("list_lexicons", package = "sentometrics")
data("list_valence_shifters", package = "sentometrics")
lexiconsIn <- c(
  list_lexicons[c("LM_en", "HENRY_en", "GI_en")],
  list(
    NRC = lexicon::hash_sentiment_nrc,
    HULIU = lexicon::hash_sentiment_huliu,
    SENTIWORD = lexicon::hash_sentiment_sentiword,
    JOCKERS = lexicon::hash_sentiment_jockers,
    SENTICNET = lexicon::hash_sentiment_senticnet,
    SOCAL = lexicon::hash_sentiment_socal_google
  )
)
lex <- sento_lexicons(lexiconsIn = lexiconsIn,
                      valenceIn = list_valence_shifters[["en"]])
lex[["HENRY_en"]]
cat("\n")

sentScores <- compute_sentiment(usnews[["texts"]], lexicons = lex, how = "proportional")
head(sentScores[, c("id", "word_count", "GI_en", "SENTIWORD", "SOCAL")])
cat("\n")

reuters <- system.file("texts", "crude", package = "tm")
tmVCorp <- tm::VCorpus(tm::DirSource(reuters),
                       list(reader = tm::readReut21578XMLasPlain))
class(compute_sentiment(tmVCorp, lex))
cat("\n")

sentoSent <- compute_sentiment(
  as.sento_corpus(tmVCorp, dates = as.Date("1993-03-06") + 1:20), lex, "UShaped"
)
tmSentPos <- sapply(tmVCorp, tm::tm_term_score, lex$NRC[y > 0, x])
tmSentNeg <- sapply(tmVCorp, tm::tm_term_score, lex$NRC[y < 0, x])
tmSent <- cbind(sentoSent[, 1:3], "tm_NRC" = tmSentPos - tmSentNeg)
sent <- merge(sentoSent, as.sentiment(tmSent))
sent[6:9, c(1, 11:13)]
cat("\n")

sSentences <- compute_sentiment(uscorpus, lex, do.sentence = TRUE)
sSentences[1:11, 1:6]
aggDocuments <- aggregate(sSentences, ctr_agg(howDocs = "equal_weight"), do.full = FALSE)
aggDocuments[1:2, 1:6]
cat("\n")

usnewsLang <- usnews[1:5, 1:3]
usnewsLang[["language"]] <- c("fr", "en", "en", "fr", "en")
lEn <- sento_lexicons(list("GI_en" = list_lexicons$GI_en))
lFr <- sento_lexicons(list("GI_fr" = list_lexicons$GI_fr_tr))
corpusLang <- sento_corpus(corpusdf = usnewsLang)
sLang <- compute_sentiment(corpusLang,
                           list(en = sento_lexicons(list("GI_en" = list_lexicons$GI_en)),
                                fr = sento_lexicons(list("GI_fr" = list_lexicons$GI_fr_tr))))
head(sLang)
cat("\n")

cat("### SECTION 3.3 ####################### \n \n")

ctrAgg <- ctr_agg(howWithin = "counts",
                  howDocs = "proportional",
                  howTime = c("exponential", "equal_weight"),
                  do.ignoreZeros = TRUE,
                  by = "week",
                  fill = "zero",
                  lag = 30,
                  alphasExp = 0.2)

sentMeas <- sento_measures(uscorpus, lexicons = lex, ctr = ctrAgg)

as.data.table(sentMeas)[, c(1, 23)]
cat("\n")

pT <- plot(sentMeas, group = "time")
pT

cat("### SECTION 3.4 ####################### \n \n")

subset(sentMeas, delete = list(c("LM_en"),
                               c("SENTICNET", "economy", "equal_weight")))
cat("\n")

subset(sentMeas, date %in% get_dates(sentMeas)[50:99])
cat("\n")

sentMeasFill <- measures_fill(sentMeas, fill = "latest", dateBefore = "1995-07-01")
head(as.data.table(sentMeasFill)[, 1:3])
cat("\n")

corpusPlain <- sento_corpus(usnews[, 1:3])
ctrAggLex <- ctr_agg(howWithin = "proportionalPol",
                     howDocs = "equal_weight",
                     howTime = "own",
                     by = "month",
                     fill = "none",
                     lag = 1)
sentMeasLex <- sento_measures(corpusPlain, lexicons = lex[-length(lex)], ctr = ctrAggLex)
mean(as.numeric(sentMeasLex$stats["meanCorr", ]))
cat("\n")

pL <- plot(sentMeasLex, group = "lexicons") +
  guides(colour = guide_legend(nrow = 1))
pL

sentMeasAgg <- aggregate(sentMeas,
                         time = list(W = c("equal_weight", "exponential0.2")),
                         lexicons = list(LEX = c("LM_en", "HENRY_en", "GI_en")),
                         features = list(JOUR = c("wsj", "wapo"),
                                         NEW = c("uncertainty", "election")),
                         do.keep = FALSE)
get_dimensions(sentMeasAgg)

glob <- aggregate(sentMeas,
                  lexicons = c(0.10, 0.40, 0.05, 0.05, 0.08, 0.08, 0.08, 0.08, 0.08),
                  features = c(0.20, 0.20, 0.20, 0.20, 0.10, 0.10),
                  time = c(1/2, 1/2),
                  do.global = TRUE)

peakdates(sentMeas, n = 1, type = "neg")
cat("\n")

cat("### SECTION 3.5 ####################### \n \n")

y <- rnorm(nobs(sentMeas))
dt <- as.data.table(sentMeas)
z <- zoo::zoo(dt[, !"date"], order.by = dt[["date"]])
reg <- lm(y ~ z[, 1:6])

ctrInSample <- ctr_model(model = "gaussian",
                         type = "BIC",
                         h = 0,
                         alphas = 0,
                         do.iter = FALSE, do.progress = FALSE)
fit <- sento_model(sentMeas, y, ctr = ctrInSample)

attrFit <- attributions(fit, sentMeas)
head(attrFit[["features"]])
cat("\n")

X <- as.matrix(as.data.table(sentMeas)[, -1])
yFit <- predict(fit, newx = X)
attrSum <- rowSums(attrFit[["lexicons"]][, -1]) + fit[["reg"]][["a0"]]
all.equal(as.numeric(yFit), attrSum)
cat("\n")

cat("### SECTION 4 ####################### \n \n")

dfm <- quanteda::dfm(uscorpus, tolower = TRUE,
                     remove_punct = TRUE, remove_numbers = TRUE, remove = quanteda::stopwords("en")) %>%
  quanteda::dfm_remove(min_nchar = 3) %>%
  quanteda::dfm_trim(min_termfreq = 0.95, termfreq_type = "quantile") %>%
  quanteda::dfm_trim(max_docfreq = 0.10, docfreq_type = "prop")
dfm <- quanteda::dfm_subset(dfm, quanteda::ntoken(dfm) > 0)
topicModel <- stm::stm(dfm, K = 8, verbose = FALSE)

topTerms <- t(stm::labelTopics(topicModel, n = 5)[["prob"]])
keywords <- lapply(1:ncol(topTerms), function(i) topTerms[, i])
names(keywords) <- paste0("TOPIC_", 1:length(keywords))

uscorpus <- add_features(uscorpus, keywords = keywords, do.binary = FALSE, do.regex = FALSE)
quanteda::docvars(uscorpus, c("uncertainty", "election", "economy", "noneconomy", "wsj", "wapo")) <- NULL
colSums(quanteda::docvars(uscorpus)[, -1] != 0)
cat("\n")

ctrAggPred <- ctr_agg(howWithin = "proportionalPol", howDocs = "equal_weight", howTime = "beta",
                      by = "day", fill = "latest", lag = 270, aBeta = 1:3, bBeta = 1:2)
sentMeasPred <- sento_measures(uscorpus, lexicons = lex, ctr = ctrAggPred)

pF <- plot(sentMeasPred, group = "features") +
  guides(colour = guide_legend(nrow = 1))
pF

load("vix.rda")
data("epu", package = "sentometrics")
sentMeasIn <- subset(sentMeasPred, date %in% vix$date)
datesIn <- get_dates(sentMeasIn)
datesEPU <- floor_date(datesIn, "month") %m-% months(1)
xEPU <- epu[epu$date %in% datesEPU, "index"]
y <- vix[vix$date %in% datesIn, value]
x <- data.frame(lag = y, epu = xEPU)

h <- 6
oos <- h - 1
M <- 60
ctrIter <- ctr_model(model = "gaussian",
                     type = "BIC",
                     h = h,
                     alphas = c(0, 0.1, 0.3, 0.5, 0.7, 0.9, 1),
                     do.iter = TRUE,
                     oos = oos,
                     nSample = M,
                     nCore = 1)
out <- sento_model(sentMeasIn, x = x[, "lag", drop = FALSE], y = y, ctr = ctrIter)
cat("\n")
summary(out)
cat("\n")

preds <- predsAR <- rep(NA, nrow(out[["performance"]]$raw))
yTarget <- y[-(1:h)]
xx <- x[-tail(1:nrow(x), h), ]
for (i in 1:(length(preds))) {
  j <- i + M
  data <- data.frame(y = yTarget[i:(j - 1)], xx[i:(j - 1), , drop = FALSE])

  reg <- lm(y ~ ., data = data)
  preds[i] <- predict(reg, xx[j + oos, ])

  regAR <- lm(y ~ ., data = data[, c("y", "lag")])
  predsAR[i] <- predict(regAR, xx[j + oos, "lag", drop = FALSE])
}

true <- out[["performance"]]$raw$response
benchmark <- data.frame(preds = preds, error = preds - true, errorSq = (preds - true)^2,
                        predsAR = predsAR, errorAR = predsAR - true, errorSqAR = (predsAR - true)^2,
                        stringsAsFactors = FALSE) # main benchmark is EPU-based model

###### TABLE 3 ######
dates  <- names(out$models)
dates1 <- which(dates <= "2007-06-01")
dates2 <- which(dates > "2007-06-01" & dates <= "2009-12-01")
dates3 <- which(dates > "2009-12-01")

rmseTable <- c(
  out$performance$RMSFE, sqrt(mean(benchmark$errorSq)), sqrt(mean(benchmark$errorSqAR)), # full
  sqrt(mean(out$performance$raw[dates1, "errorSq"])), sqrt(mean(benchmark[dates1, "errorSq"])), sqrt(mean(benchmark[dates1, "errorSqAR"])), # pre-crisis (P1)
  sqrt(mean(out$performance$raw[dates2, "errorSq"])), sqrt(mean(benchmark[dates2, "errorSq"])), sqrt(mean(benchmark[dates2, "errorSqAR"])), # crisis (P2)
  sqrt(mean(out$performance$raw[dates3, "errorSq"])), sqrt(mean(benchmark[dates3, "errorSq"])), sqrt(mean(benchmark[dates3, "errorSqAR"])) # post-crisis (P3)
)
madTable <- c(
  out$performance$MAD, mean(abs(benchmark$error)), mean(abs(benchmark$errorAR)),
  mean(abs(out$performance$raw[dates1, "error"])), mean(abs(benchmark[dates1, "error"])), mean(abs(benchmark[dates1, "errorAR"])),
  mean(abs(out$performance$raw[dates2, "error"])), mean(abs(benchmark[dates2, "error"])), mean(abs(benchmark[dates2, "errorAR"])),
  mean(abs(out$performance$raw[dates3, "error"])), mean(abs(benchmark[dates3, "error"])), mean(abs(benchmark[dates3, "errorAR"]))
)
names(rmseTable) <- names(madTable) <-
  paste0(rep(c("Full", "P1", "P2", "P3"), rep(3, 4)), "_", c("M-s", "M-bm", "M-ar"))

cat("RMSE \n")
rmseTable
cat("\n")

cat("MAD \n")
madTable
cat("\n")

######

r <- plot(out) +
  geom_line(data = melt(data.table(date = names(out$models),
                                   "M-epu" = preds, "M-ar" = predsAR, check.names = FALSE),
                        id.vars = "date"))
r

attr <- attributions(out, sentMeasIn, do.lags = FALSE, do.normalize = FALSE)

fe <- plot(attr, group = "features") +
  guides(fill = guide_legend(nrow = 1))
le <- plot(attr, group = "lexicons") +
  guides(fill = guide_legend(nrow = 1))
a <- gridExtra::grid.arrange(fe + theme(axis.title.x = element_blank()),
                             le + theme(axis.title.y = element_blank()),
                             ncol = 1, nrow = 2)

sink()

