
###########################################
############## VIGNETTE CODE ##############
###########################################

###########################################

th <- function() { # plotting specifications (border and grid)
  theme(legend.title = element_blank(),
        legend.position = "top",
        panel.background = element_rect(colour = "black", size = 0.35),
        panel.grid.major = element_line(colour = "grey95", size = 0.10),
        panel.grid.minor = element_line(colour = "grey95", size = 0.10))
}

###########################################

require("sentometrics")

data("usnews", package = "sentometrics")
class(usnews)

head(usnews[, -3])

usnews[["text"]][2029]

corpusAll <- sento_corpus(usnews)
class(corpusAll)

corpus <- quanteda::corpus_subset(corpusAll, date < "2014-10-01")
corpus

regex <- c("\\bRepublic[s]?\\b|\\bDemocrat[s]?\\b|\\belection\\b|\\b[US|U.S.] [p|P]resident\\b|\\bwar\\b")
corpus <- add_features(corpus,
                       keywords = list(uncert = "uncertainty", uselect = regex),
                       do.binary = TRUE,
                       do.regex = c(FALSE, TRUE))
c(sum(corpus$documents[["uncert"]]), sum(corpus$documents[["uselect"]]))

data("lexicons", package = "sentometrics")
data("valence", package = "sentometrics")
mine <- data.frame(w = c("uncertainty", "anxiety", "concern", "distrust", "worries"),
                   s = c(-2, -2, -2, -2, -2))
lexiconsIn <- c(list(myLexicon = mine), lexicons[c("LM_eng", "HENRY_eng")])
lex <- setup_lexicons(lexiconsIn = lexiconsIn,
                      valenceIn = valence[["valence_eng"]])
lex[["myLexicon"]]

ctrAgg <- ctr_agg(howWithin = "tf-idf",
                  howDocs = "proportional",
                  howTime = c("equal_weight", "linear", "almon"),
                  do.ignoreZeros = TRUE,
                  by = "month",
                  fill = "zero",
                  lag = 12,
                  ordersAlm = 1:3,
                  do.inverseAlm = TRUE,
                  do.normalizeAlm = TRUE)

sentMeas <- sento_measures(corpus, lexicons = lex, ctr = ctrAgg)
summary(sentMeas)

sentMeas$measures[, c(1, 23, 135)]

p <- plot(sentMeas, group = "features") +
  guides(colour = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date",
               date_labels = "%m-%Y",
               limits = as.Date(c("1995-01-01", "2015-01-01"))) +
  th()
p

data("epu", package = "sentometrics")
y <- epu[epu[["date"]] >= sentMeas[["measures"]][["date"]][1], "index"]
length(y) == nrow(sentMeas[["measures"]])

ctrIter <- ctr_model(model = "gaussian",
                     type = "BIC",
                     h = 1,
                     alphas = c(0.3, 0.5, 0.7),
                     do.iter = TRUE,
                     nSample = 36,
                     do.parallel = TRUE)

require("doParallel")
cl <- makeCluster(4)
registerDoParallel(cl)
start <- proc.time()
out <- sento_model(sentMeas, y, ctr = ctrIter)
stop <- proc.time() - start
stopCluster(cl)
summary(out)

r <- plot(out) + th()
r

attr <- retrieve_attributions(out, sentMeas, do.normalize = FALSE)
names(attr)

a <- plot_attributions(attr, group = "features") +
  guides(fill = guide_legend(nrow = 1)) +
  th()
a

ctrInSample <- ctr_model(model = "gaussian",
                         type = "BIC",
                         h = 0,
                         alphas = c(0.3, 0.5, 0.7),
                         do.iter = FALSE)
inSample <- sento_model(sentMeas, y, ctr = ctrInSample)
attrInSample <- retrieve_attributions(inSample, sentMeas)

yFit <- predict(inSample[["reg"]], newx = inSample[["x"]])
attrSum <- rowSums(attrInSample[["lexicons"]][, -1]) + inSample[["reg"]][["a0"]]
all(sapply(1:length(yFit), function(i) return(all.equal(yFit[i] - attrSum[i], 0))))

ctrMerge <- ctr_merge(sentMeas,
                      time = list(W1 = c("almon1", "almon2", "linear", "almon3_inv"),
                                  W2 = c("almon1_inv", "almon2_inv", "almon3")),
                      lexicons = list(LEX = c("LM_eng", "HENRY_eng")),
                      features = list(JOUR = c("wsj", "wapo"),
                                      UNC = c("uncert", "uselect")),
                      do.keep = FALSE)
sentMeasMerged <- merge_measures(ctrMerge)
sentMeasMerged[c("features", "lexicons", "time")]

globAdHoc <- to_global(sentMeas,
                       lexicons = c(0.10, 0.70, 0.20),
                       features = c(0.20, 0.20, 0.20, 0.20, 0.10, 0.10),
                       time = c(1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8))

cols <- names(inSample[["discarded"]][which(!inSample[["discarded"]])])
nms <- stringi::stri_split(cols, regex = "--")
coeffs <- coef(inSample[["reg"]])[cols, ]

######### the code commented out determines weights based on the average attribution

# sign_coeffs <- function(sentomeasures, names, coeffs, dimension) {
#   signCoeffs <- sign(
#   t(sapply(sentomeasures[[dimension]], function(n)
#     return(sapply(names, function(nn) return(n %in% nn))))) %*% sign(coeffs)
# )
# signCoeffs[signCoeffs == 0] <- 1
# return(signCoeffs)
# }

# sLex <- sign_coeffs(sentMeas, nms, coeffs, "lexicons")
# sFeat <- sign_coeffs(sentMeas, nms, coeffs, "features")
# sTime <- sign_coeffs(sentMeas, nms, coeffs, "time")

# lexW <- colMeans(attrInSample[["lexicons"]][, -1]/rowSums(attrInSample[["lexicons"]][, -1]))/sLex
# featW <- colMeans(attrInSample[["features"]][, -1]/rowSums(attrInSample[["features"]][, -1]))/sFeat
# timeW <- colMeans(attrInSample[["time"]][, -1]/rowSums(attrInSample[["time"]][, -1]))/sTime

weight_coeffs <- function(sentomeasures, names, coeffs, dimension) {
  allSum <- t(sapply(sentomeasures[[dimension]], function(n)
    return(sapply(names, function(nn) return(n %in% nn))))) %*% coeffs
  return(allSum/sum(abs(allSum)))
}
lexW <- weight_coeffs(sentMeas, nms, coeffs, "lexicons")
featW <- weight_coeffs(sentMeas, nms, coeffs, "features")
timeW <- weight_coeffs(sentMeas, nms, coeffs, "time")

globC <- to_global(sentMeas,
                   lexicons = lexW,
                   features = featW,
                   time = timeW)

scaled <- scaled <- scale(cbind(globAdHoc[["global"]], globC[["global"]], yFit, y))
dt <- data.table(as.Date(row.names(globAdHoc)), scaled)
colnames(dt) <- c("date", "ad-hoc", "coefficient-based", "fit", "EPU")
g <- ggplot(data = melt(dt, id.vars = "date"), aes(x = date, y = value, color = variable)) +
  geom_line() +
  scale_x_date(name = "Date",
               date_labels = "%m-%Y",
               limits = as.Date(c("1995-01-01", "2015-01-01"))) +
  scale_y_continuous(name = "Sentiment/Response") +
  ggthemes::theme_tufte(base_size = 12) +
  th()
g

