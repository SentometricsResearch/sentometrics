
###########################################
############ PROJECT PAGE CODE ############
###########################################

####

require(sentometrics)
require(ggplot2)
require(gridExtra)

####

data("usnews")
colnames(usnews)

####

corpusAll <- sento_corpus(usnews)
corpus <- quanteda::corpus_subset(corpusAll, date >= "1988-01-01" & date < "2014-10-01")

####

corpus <- add_features(corpus,
                       keywords = list(war = "war", pol = c("election", "president"), crisis = "crisis"))
sum(corpus$documents$pol)

####

data("lexicons")
data("valence")

####

lexIn <- setup_lexicons(lexiconsIn = lexicons[c("LM_eng", "HENRY_eng")],
                        valenceIn = valence[["valence_eng"]],
                        do.split = TRUE)

###

ctrIn <- ctr_agg(howWithin = "tf-idf",
                 howDocs = "proportional",
                 howTime = c("equal_weight", "linear", "almon"),
                 do.ignoreZeros = TRUE,
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
summary(sentMeas)

sent <- compute_sentiment(corpus, lexicons = lexIn, how = ctrIn$howWithin)
sentMeasAlt <- perform_agg(sent, ctr = ctrIn)

###

sentMeasWSJ <- select_measures(sentMeas, toSelect = "wsj")
sentMeasLinEw <- select_measures(sentMeas, toSelect = c("linear", "equal_weight"),
                                   do.combine = FALSE)

###

p1 <- plot(sentMeasWSJ, group = "lexicons")
p2 <- plot(scale(sentMeasLinEw), group = "time")

grid.arrange(p1, p2, nrow = 2, ncol = 1)

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
                  features = c(0.10, 0.10, 0.10, 0.10, 0.20, 0.20, 0.20),
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

###

y <- epu[epu$date >= sentMeas$measures$date[1], ]$index
length(y) == nrow(sentMeas$measures)

ctrIC <- ctr_model(model = "gaussian",
                   type = "BIC",
                   h = 1,
                   alphas = seq(0.2, 0.8, by = 0.1),
                   do.iter = FALSE)
outIC <- sento_model(sentMeas, y, ctr = ctrIC)
summary(outIC)

###

yb <- epu[epu$date >= sentMeas$measures$date[1], ]$above

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
                     alphas = seq(0.1, 0.9, by = 0.2),
                     do.iter = TRUE,
                     nSample = 60,
                     start = 115)
outIter <- sento_model(sentMeasShift, y, x = x, ctr = ctrIter)
summary(outIter)

###

r <- plot(outIter)
r

###

attributions <- retrieve_attributions(outIter, sentMeasShift, do.normalize = FALSE)

f <- plot_attributions(attributions, group = "features") +
  guides(fill = guide_legend(nrow = 1))
l <- plot_attributions(attributions, group = "lexicons")
t <- plot_attributions(attributions, group = "time") +
  guides(fill = guide_legend(nrow = 1))

grid.arrange(f, l, t, ncol = 1, nrow = 3)

### save plots to project page directory ###

ggsave("docs/plots/time.png", arrangeGrob(p))
ggsave("docs/plots/examples.png", arrangeGrob(p1, p2, nrow = 2))
ggsave("docs/plots/global.png", arrangeGrob(g))
ggsave("docs/plots/for.png", arrangeGrob(r))
ggsave("docs/plots/attr.png", arrangeGrob(f, l, t, ncol = 1, nrow = 3))

