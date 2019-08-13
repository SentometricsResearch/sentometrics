
###############################
#######  VIGNETTE CODE  #######
###############################

###### DESCRIPTION ######

### This code is used in the vignette paper 'The R Package sentometrics to Compute,
### Aggregate and Predict with Textual Sentiment' (Ardia, Bluteau, Borms and Boudt, 2018)
### See the paper for the results and setup details
### Download the package first before you run this script...
### install.packages("sentometrics") # from CRAN (version 0.5.6), OR
### install.packages("sentometrics_0.5.6.tar.gz", repos = NULL) # from the tar

###### SESSION INFO ######

### R version 3.5.1 (2018-07-02)
### Platform: x86_64-w64-mingw32/x64 (64-bit)
### Running under: Windows >= 8 x64 (build 9200)
###
### Matrix products: default
###
### locale:
### [1] LC_COLLATE=Dutch_Belgium.1252  LC_CTYPE=Dutch_Belgium.1252    LC_MONETARY=Dutch_Belgium.1252
### [4] LC_NUMERIC=C                   LC_TIME=Dutch_Belgium.1252
###
### attached base packages:
### [1] stats     graphics  grDevices utils     datasets  methods   base
###
### other attached packages:
### [1] lubridate_1.7.4    stm_1.3.3          quanteda_1.3.14    lexicon_1.1.3      gridExtra_2.3      ggplot2_3.1.0
### [7] sentometrics_0.5.6 data.table_1.11.8
###
### loaded via a namespace (and not attached):
### [1] Rcpp_1.0.0         pillar_1.3.1       compiler_3.5.1     plyr_1.8.4         bindr_0.1.1        iterators_1.0.10
### [7] tools_3.5.1        stopwords_0.9.0    tibble_1.4.2       gtable_0.2.0       lattice_0.20-38    pkgconfig_2.0.2
### [13] rlang_0.3.0.1      Matrix_1.2-15      foreach_1.4.4      fastmatch_1.1-0    yaml_2.2.0         bindrcpp_0.2.2
### [19] withr_2.1.2        dplyr_0.7.8        stringr_1.3.1      syuzhet_1.0.4      grid_3.5.1         glmnet_2.0-16
### [25] tidyselect_0.2.5   glue_1.3.0         R6_2.3.0           purrr_0.2.5        spacyr_1.0         magrittr_1.5
### [31] scales_1.0.0       codetools_0.2-15   assertthat_0.2.0   colorspace_1.3-2   stringi_1.2.4      RcppParallel_4.4.2
### [37] lazyeval_0.2.1     munsell_0.5.0      crayon_1.3.4

remove(list = ls())
options(prompt = "R> ", continue = "+  ", width = 120, digits = 4, max.print = 80, useFancyQuotes = FALSE)
sink(file = "output_vignette.txt", append = FALSE, split = TRUE) # output printed in .txt file

library("sentometrics")

library("ggplot2")
library("gridExtra")
library("lexicon")
library("quanteda")
library("stm")
library("lubridate")

info <- sessionInfo()
cat(info$locale, "\n \n")
print(info)
cat("\n")

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
head(sentScores[, c("word_count", "GI_en", "SENTIWORD", "SOCAL")])
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

get_measures(sentMeas)[, c(1, 23)]
cat("\n")

pT <- plot(sentMeas, group = "time")
pT

cat("### SECTION 3.4 ####################### \n \n")

measures_delete(sentMeas, list(c("LM_en"),
                               c("SENTICNET", "economy", "equal_weight")))
cat("\n")

measures_subset(sentMeas, date %in% get_dates(sentMeas)[50:149])
cat("\n")

sentMeasFill <- measures_fill(sentMeas, fill = "latest", dateBefore = "1995-07-01")
head(get_measures(sentMeasFill)[, 1:3])
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

sentMeasMerged <- measures_merge(sentMeas,
                                 time = list(W = c("equal_weight", "exponential_0.2")),
                                 lexicons = list(LEX = c("LM_en", "HENRY_en", "GI_en")),
                                 features = list(JOUR = c("wsj", "wapo"),
                                                 NEW = c("uncertainty", "election")),
                                 do.keep = FALSE)
get_dimensions(sentMeasMerged)

glob <- measures_global(sentMeas,
                        lexicons = c(0.10, 0.40, 0.05, 0.05, 0.08, 0.08, 0.08, 0.08, 0.08),
                        features = c(0.20, 0.20, 0.20, 0.20, 0.10, 0.10),
                        time = c(1/2, 1/2))

peakdates(sentMeas, n = 1, type = "neg")
cat("\n")

cat("### SECTION 3.5 ####################### \n \n")

set.seed(505)
y <- rnorm(nobs(sentMeas))
ctrInSample <- ctr_model(model = "gaussian",
                         type = "BIC",
                         h = 0,
                         alphas = 0,
                         do.iter = FALSE, do.progress = FALSE)
fit <- sento_model(sentMeas, y, ctr = ctrInSample)

attrFit <- attributions(fit, sentMeas)
head(attrFit[["features"]])
cat("\n")

X <- as.matrix(get_measures(sentMeas)[, -1])
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
sentMeasIn <- measures_subset(sentMeasPred, date %in% vix$date)
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
                        stringsAsFactors = FALSE)

###### TABLE 3 ######
dates  <- names(out$models)
dates1 <- which(dates <= "2007-06-01")
dates2 <- which(dates > "2007-06-01" & dates <= "2009-12-01")
dates3 <- which(dates > "2009-12-01")

rmseTable <- c(out$performance$RMSFE,
  sqrt(mean(benchmark$errorSq)), sqrt(mean(benchmark$errorSqAR)), # full
  sqrt(mean(out$performance$raw[dates1, "errorSq"])),
  sqrt(mean(benchmark[dates1, "errorSq"])), sqrt(mean(benchmark[dates1, "errorSqAR"])), # pre-crisis (P1)
  sqrt(mean(out$performance$raw[dates2, "errorSq"])),
  sqrt(mean(benchmark[dates2, "errorSq"])), sqrt(mean(benchmark[dates2, "errorSqAR"])), # crisis (P2)
  sqrt(mean(out$performance$raw[dates3, "errorSq"])),
  sqrt(mean(benchmark[dates3, "errorSq"])), sqrt(mean(benchmark[dates3, "errorSqAR"])) # post-crisis (P3)
)
madTable <- c(out$performance$MAD,
  mean(abs(benchmark$error)), mean(abs(benchmark$errorAR)),
  mean(abs(out$performance$raw[dates1, "error"])),
  mean(abs(benchmark[dates1, "error"])), mean(abs(benchmark[dates1, "errorAR"])),
  mean(abs(out$performance$raw[dates2, "error"])),
  mean(abs(benchmark[dates2, "error"])), mean(abs(benchmark[dates2, "errorAR"])),
  mean(abs(out$performance$raw[dates3, "error"])),
  mean(abs(benchmark[dates3, "error"])), mean(abs(benchmark[dates3, "errorAR"]))
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
                                   "M-epu" = preds, "M-ar" = predsAR, check.names = FALSE), id.vars = "date"))
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

###### FIGURES ######

ggsave("sentmeasT.pdf", pT) # Figure 1
ggsave("sentmeasL.pdf", pL) # Figure 2
ggsave("sentmeasF.pdf", pF) # Figure 3
ggsave("for.pdf", r) # Figure 4
ggsave("attr.pdf", a) # Figure 5

