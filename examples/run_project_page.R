
###########################################
############ PROJECT PAGE CODE ############
###########################################

####

require(sentometrics)
require(ggplot2)
require(gridExtra)

####

data("usnews")
colnames(usnews) # id, date, text, wsj, wapo, economy, noneconomy

####

corpusAll <- sento_corpus(usnews)
quanteda::ndoc(corpusAll) # 4145

corpusSample <- quanteda::corpus_sample(corpusAll, size = 1000)
quanteda::ndoc(corpusSample) # 1000

corpus <- quanteda::corpus_subset(corpusAll, date >= "1988-01-01" & date < "2014-10-01")
quanteda::ndoc(corpus) # 4097

####

corpus <- add_features(corpus,
                       keywords = c(war = "war", election = "election", president = "president", crisis = "crisis"))
sum(corpus$documents$war) # 1099
sum(corpus$documents$election) # 187
sum(corpus$documents$president) # 490
sum(corpus$documents$crisis) # 381

####

data("lexicons")
names(lexicons)

data("valence")
names(valence)

####

lexNoVal <- setup_lexicons(lexiconsIn = lexicons[c("LM_eng", "HENRY_eng")])

lexVal <- setup_lexicons(lexiconsIn = lexicons[c("LM_eng", "HENRY_eng")],
                         valenceIn = valence[["valence_eng"]])

lexIn <- setup_lexicons(lexiconsIn = lexicons[c("LM_eng", "HENRY_eng")],
                        valenceIn = valence[["valence_eng"]],
                        do.split = TRUE)

###

ctrIn <- ctr_agg(howWithin = "tf-idf",
                 howDocs = "proportional",
                 howTime = c("equal_weight", "linear", "almon"),
                 do.ignoreZeros = FALSE,
                 by = "month",
                 fill = "zero",
                 lag = 12,
                 ordersAlm = 1:3,
                 do.inverseAlm = TRUE,
                 do.normalizeAlm = TRUE)

###

lag <- 30
a <- almons(n = lag, orders = 1:3, do.inverse = TRUE, do.normalize = TRUE)
e <- exponentials(n = lag, alphas = c(0.1))
lin <- (1:lag)/sum(1:lag)
weights <- data.frame(id = 1:lag, almon = a[, 6], exponential = e[, 1],
                      linear = lin, equal_weight = 1/lag)

p <- ggplot(data = melt(weights, id.vars = "id"), aes(x = id, y = value, color = variable)) +
  geom_line() +
  scale_x_continuous(name = "Lag") +
  scale_y_continuous(trans = "reverse", name = "Weight") +
  ggthemes::theme_tufte(base_size = 11) +
  theme(legend.title = element_blank(), legend.position = "top")
p

####

sentMeas <- sento_measures(corpus, lexicons = lexIn, ctr = ctrIn)
sentMeas
sentMeas$measures
sentMeas$stats
summary(sentMeas)

sent <- compute_sentiment(corpus, lexicons = lexIn, how = ctrIn$howWithin)
sentMeasAlt <- perform_agg(sent, ctr = ctrIn)

###

sentMeasScaled <- scale(sentMeas, center = TRUE, scale = TRUE)

sentMeasWSJ <- select_measures(sentMeas, toSelect = "wsj")
colnames(sentMeasWSJ$measures)[-1]

sentMeasLinAlm1 <- select_measures(sentMeas, toSelect = c("linear", "equal_weight"), do.combine = FALSE)
colnames(sentMeasLinAlm1$measures)[-1]

sentMeasSel <- select_measures(sentMeas, toSelect = "all", dates = "date <= '2005-04-01'")
max(sentMeasSel$measures$date)

###

p1 <- plot(sentMeasScaled, group = "features")
p2 <- plot(sentMeasWSJ, group = "lexicons") +
  guides(colour = guide_legend(nrow = 2))
p3 <- plot(sentMeasLinAlm1, group = "time")
p4 <- plot(sentMeasSel)

grid.arrange(p1, p2, p3, p4, ncol = 2)

####

ctrMerge <- ctr_merge(sentMeas,
                      time = list(W = c("equal_weight", "linear")),
                      lexicons = list(LEX = c("LM_eng_POS", "HENRY_eng_POS")),
                      features = list(journals = c("wsj", "wapo")),
                      do.keep = FALSE)
sentMeasMerged <- merge_measures(ctrMerge)
sentMeasMerged[c("features", "lexicons", "time")]

####

glob <- to_global(sentMeas,
                  lexicons = c(0.50, 0, 0.50, 0),
                  features = c(0.10, 0.10, 0.10, 0.10, 0.15, 0.15, 0.15, 0.15),
                  time = 1)

g <- ggplot(data = glob, aes(x = as.Date(row.names(glob)), y = global)) +
  geom_line() +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Sentiment") +
  ggthemes::theme_tufte(base_size = 11) +
  theme(legend.title = element_blank())
g

###

data("epu")
y <- epu[epu$date >= sentMeas$measures$date[1], ]$index
length(y) == nrow(sentMeas$measures)

ctrIC <- ctr_model(model = "gaussian",
                   type = "BIC",
                   h = 1,
                   alphas = seq(0, 1, by = 0.10),
                   do.iter = FALSE)
outIC <- sento_model(sentMeas, y, ctr = ctrIC)
summary(outIC)

###

yb <- epu[epu$date >= sentMeas$measures$date[1], ]$above
length(yb) == nrow(sentMeas$measures)
ctrCVBi <- ctr_model(model = "binomial",
                     type = "cv",
                     h = 1,
                     do.iter = FALSE,
                     trainWindow = 200,
                     testWindow = 20)
outBi <- sento_model(sentMeas, yb, ctr = ctrCVBi)
summary(outBi)

###

x <- data.frame(lag = y[-length(y)])
y <- y[-1]
datesIn <- sentMeas$measures$date[-1]
sentMeasShift <- select_measures(sentMeas, dates = datesIn)
all(c(all.equal(length(y), nrow(x)), all.equal(nrow(x), nrow(sentMeasShift$measures)))) # TRUE

ctrIter <- ctr_model(model = "gaussian",
                     type = "BIC",
                     h = 1,
                     alphas = seq(0.10, 0.90, by = 0.20),
                     do.iter = TRUE,
                     nSample = 60,
                     start = 115)
outIter <- sento_model(sentMeasShift, y, x = NULL, ctr = ctrIter)
summary(outIter)
outIter$performance

###

r <- plot(outIter)
r

###

attributions <- retrieve_attributions(outIter, sentMeasShift, do.normalize = FALSE)
attributions$features
attributions$lexicons
attributions$time

f <- plot_attributions(attributions, group = "features") +
  guides(fill = guide_legend(nrow = 1))
l <- plot_attributions(attributions, group = "lexicons")
t <- plot_attributions(attributions, group = "time") +
  guides(fill = guide_legend(nrow = 1))

grid.arrange(f, l, t, ncol = 1, nrow = 3)

### save plots to project page directory ###

ggsave("docs/plots/timeWeights.png", arrangeGrob(p))
ggsave("docs/plots/selects.png", arrangeGrob(p1, p2, p3, p4, ncol = 2))
ggsave("docs/plots/glob.png", arrangeGrob(g))
ggsave("docs/plots/forecasts.png", arrangeGrob(r))
ggsave("docs/plots/attribs.png", arrangeGrob(f, l, t, ncol = 1, nrow = 3))

