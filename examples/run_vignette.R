
#################################################
################# VIGNETTE CODE #################
#################################################

###### DESCRIPTION ######

### This code is used in the vignette paper 'The R Package sentometrics to Compute,
### Aggregate and Predict with Textual Sentiment' (Ardia, Bluteau, Borms and Boudt, 2021).
### See the paper for the results and setup details.

### Download the package and its dependencies first before you run this script...
### install.packages("sentometrics", dependencies = TRUE) # from CRAN (version 0.8.4), OR
### install.packages("sentometrics_0.8.4.tar.gz", repos = NULL, dependencies = TRUE) # from the tar

### Dependencies can be installed separately like this:
### install.packages(
###   c("covr", "doParallel", "e1071", "NLP", "randomForest",
###     "testthat", "tm", "caret", "data.table", "foreach",
###     "ggplot2", "glmnet", "ISOweek", "quanteda", "Rcpp", "RcppRoll",
###     "RcppParallel", "stringi", "RcppArmadillo")
### )

###### SESSION INFO ######

# R version 4.1.0 (2021-05-18)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)
# 
# Matrix products: default
# 
# locale:
# [1] LC_COLLATE=English_Belgium.1252  LC_CTYPE=English_Belgium.1252    LC_MONETARY=English_Belgium.1252
# [4] LC_NUMERIC=C                     LC_TIME=English_Belgium.1252    
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] data.table_1.14.0  sentometrics_0.8.4 zoo_1.8-9          lubridate_1.7.10   stm_1.3.6          quanteda_3.0.0    
# [7] lexicon_1.2.1      gridExtra_2.3      ggplot2_3.3.4     
# 
# loaded via a namespace (and not attached):
#  [1] shape_1.4.6        tidyselect_1.1.1   slam_0.1-48        NLP_0.2-1          purrr_0.3.4        splines_4.1.0     
#  [7] lattice_0.20-44    colorspace_2.0-1   vctrs_0.3.8        generics_0.1.0     survival_3.2-11    utf8_1.2.1        
# [13] rlang_0.4.11       syuzhet_1.0.6      pillar_1.6.1       glue_1.4.2         withr_2.4.2        matrixStats_0.59.0
# [19] foreach_1.5.1      lifecycle_1.0.0    stringr_1.4.0      munsell_0.5.0      gtable_0.3.0       ragg_1.1.3        
# [25] codetools_0.2-18   labeling_0.4.2     tm_0.7-8           parallel_4.1.0     fansi_0.5.0        xts_0.12.1        
# [31] Rcpp_1.0.6         scales_1.1.1       RcppParallel_5.1.4 systemfonts_1.0.2  farver_2.1.0       textshaping_0.3.5 
# [37] fastmatch_1.1-0    stopwords_2.2      digest_0.6.27      stringi_1.6.2      dplyr_1.0.6        RcppRoll_0.3.0    
# [43] rprojroot_2.0.2    grid_4.1.0         here_1.0.1         ISOweek_0.6-2      tools_4.1.0        magrittr_2.0.1    
# [49] glmnet_4.1-1       tibble_3.1.2       crayon_1.4.1       pkgconfig_2.0.3    ellipsis_0.3.2     Matrix_1.3-3      
# [55] xml2_1.3.2         iterators_1.0.13   R6_2.5.0           compiler_4.1.0

remove(list = ls())
options(prompt = "R> ", continue = "+  ", width = 120, digits = 4,
         max.print = 90, useFancyQuotes = FALSE)
sink(file = "output_vignette.txt", append = FALSE, split = TRUE) # output printed in .txt file

library("ggplot2")
library("gridExtra")
library("lexicon")
library("quanteda")
library("stm")
library("lubridate")
library("zoo")

set.seed(505)

##################
###### CODE ######
##################

cat("### SECTION 3.1 ####################### \n \n")

library("sentometrics")
library("data.table")

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
                         keywords = list(uncertainty = c("uncertainty", "distrust"),
                                         election = regex),
                         do.binary = TRUE,
                         do.regex = c(FALSE, TRUE))
tail(quanteda::docvars(uscorpus))
cat("\n")

summ <- corpus_summarize(uscorpus, by = "year")
pCorp <- summ$plots$feature_plot + guides(color = guide_legend(nrow = 1)) + 
  theme(text = element_text(size = 10))
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

sentScores <- compute_sentiment(usnews[["texts"]], lexicons = lex,
                                how = "proportional")
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
cat("\n")

aggDocuments <- aggregate(sSentences, ctr_agg(howDocs = "equal_weight"),
                          do.full = FALSE)
aggDocuments[1:2, 1:6]
cat("\n")

usnewsLang <- usnews[1:5, 1:3]
usnewsLang[["language"]] <- c("fr", "en", "en", "fr", "en")
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

pT <- plot(sentMeas, group = "time") + 
  theme(text = element_text(size = 10))
pT

cat("### SECTION 3.4 ####################### \n \n")

subset(sentMeas, 1:600, delete = list(c("LM_en"), c("SENTICNET", "economy", "equal_weight")))
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
  guides(colour = guide_legend(nrow = 1)) +
  theme(text = element_text(size = 10))
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

dfm <- quanteda::tokens(uscorpus, what = "word",
                        remove_punct = TRUE, remove_numbers = TRUE) %>%
  quanteda::dfm(tolower = TRUE) %>%
  quanteda::dfm_remove(quanteda::stopwords("en"), min_nchar = 3) %>%
  quanteda::dfm_trim(min_termfreq = 0.95, termfreq_type = "quantile") %>%
  quanteda::dfm_trim(max_docfreq = 0.10, docfreq_type = "prop") %>% 
  quanteda::dfm_subset(quanteda::ntoken(.) > 0)
topicModel <- stm::stm(dfm, K = 8, verbose = FALSE)

topTerms <- t(stm::labelTopics(topicModel, n = 5)[["prob"]])
keywords <- lapply(1:ncol(topTerms), function(i) topTerms[, i])
names(keywords) <- paste0("TOPIC_", 1:length(keywords))

uscorpus <- add_features(uscorpus, keywords = keywords, do.binary = FALSE,
                         do.regex = FALSE)
quanteda::docvars(uscorpus, c("uncertainty", "election", "economy", "noneconomy", "wsj", "wapo")) <- NULL
colSums(quanteda::docvars(uscorpus)[, -1] != 0)
cat("\n")

ctrAggPred <- ctr_agg(howWithin = "proportionalPol", howDocs = "equal_weight", howTime = "beta",
                      by = "day", fill = "latest", lag = 270, aBeta = 1:3, bBeta = 1:2)
sentMeasPred <- sento_measures(uscorpus, lexicons = lex, ctr = ctrAggPred)

pF <- plot(sentMeasPred, group = "features") +
  guides(colour = guide_legend(nrow = 1)) +
  theme(text = element_text(size = 10))
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

###### TABLE 2 ######
true <- out[["performance"]]$raw$response
benchmark <- data.frame(preds = preds, error = preds - true, errorSq = (preds - true)^2,
                        predsAR = predsAR, errorAR = predsAR - true, errorSqAR = (predsAR - true)^2,
                        stringsAsFactors = FALSE) # main benchmark is EPU-based model

dates  <- names(out$models)
dates1 <- which(dates <= "2007-06-01")
dates2 <- which(dates > "2007-06-01" & dates <= "2009-12-01")
dates3 <- which(dates > "2009-12-01")

rmseTable <- c(
  out$performance$RMSFE, sqrt(mean(benchmark$errorSq)),
  sqrt(mean(benchmark$errorSqAR)), # full
  sqrt(mean(out$performance$raw[dates1, "errorSq"])),
  sqrt(mean(benchmark[dates1, "errorSq"])),
  sqrt(mean(benchmark[dates1, "errorSqAR"])), # pre-crisis (P1)
  sqrt(mean(out$performance$raw[dates2, "errorSq"])),
  sqrt(mean(benchmark[dates2, "errorSq"])),
  sqrt(mean(benchmark[dates2, "errorSqAR"])), # crisis (P2)
  sqrt(mean(out$performance$raw[dates3, "errorSq"])),
  sqrt(mean(benchmark[dates3, "errorSq"])),
  sqrt(mean(benchmark[dates3, "errorSqAR"])) # post-crisis (P3)
)
madTable <- c(
  out$performance$MAD, mean(abs(benchmark$error)),
  mean(abs(benchmark$errorAR)),
  mean(abs(out$performance$raw[dates1, "error"])),
  mean(abs(benchmark[dates1, "error"])),
  mean(abs(benchmark[dates1, "errorAR"])),
  mean(abs(out$performance$raw[dates2, "error"])),
  mean(abs(benchmark[dates2, "error"])),
  mean(abs(benchmark[dates2, "errorAR"])),
  mean(abs(out$performance$raw[dates3, "error"])),
  mean(abs(benchmark[dates3, "error"])),
  mean(abs(benchmark[dates3, "errorAR"]))
)
names(rmseTable) <- names(madTable) <-
  paste0(rep(c("Full", "P1", "P2", "P3"), rep(3, 4)), "_", c("M-s", "M-bm", "M-ar"))

cat("RMSE \n")
rmseTable
cat("\n")

cat("MAD \n")
madTable
cat("\n")
#####################

r <- plot(out) +
  geom_line(data = melt(data.table(date = names(out$models),
                                   "M-epu" = preds, "M-ar" = predsAR,
                                   check.names = FALSE),
                        id.vars = "date")) + 
  theme(text = element_text(size = 10))
r

attr <- attributions(out, sentMeasIn, do.lags = FALSE, do.normalize = FALSE)

fe <- plot(attr, group = "features") +
  guides(fill = guide_legend(nrow = 1)) +
  theme(text = element_text(size = 10))
le <- plot(attr, group = "lexicons") +
  guides(fill = guide_legend(nrow = 1)) + 
  theme(text = element_text(size = 10))
a <- grid.arrange(fe + theme(axis.title.x = element_blank()),
                  le + theme(axis.title.y = element_blank()),
                  ncol = 1, nrow = 2)

######

cat("############################## \n")
cat("###### SESSION INFO \n \n")
info <- sessionInfo()
print(info)
cat("\n")

######

sink()

if (!dir.exists("Figures")) dir.create("Figures")
ggsave("Figures/corpsumm.pdf", pCorp)
ggsave("Figures/sentmeasT.pdf", pT)
ggsave("Figures/sentmeasL.pdf", pL)
ggsave("Figures/sentmeasF.pdf", pF)
ggsave("Figures/for.pdf", r)
ggsave("Figures/attr.pdf", a)
