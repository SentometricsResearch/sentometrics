
###########################################
############## VIGNETTE CODE ##############
###########################################

require(sentometrics)

data("usnews")
class(usnews)

colnames(usnews)

corpusAll <- sento_corpus(usnews)
class(corpusAll)

corpus <- quanteda::corpus_subset(corpusAll, date < "2014-10-1")
corpus

corpus <- add_features(corpus, keywords = list(el = "election", war = "war"))
c(sum(corpus$documents$el), sum(corpus$documents$war))

data("lexicons")
data("valence")
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

p <- plot(sentMeas, group = "features") +
  guides(colour = guide_legend(nrow = 1))
p

data("epu")
y <- epu[epu[["date"]] >= sentMeas[["measures"]]$date[1], ]$index
length(y) == nrow(sentMeas[["measures"]])

ctrIter <- ctr_model(model = "gaussian",
                     type = "BIC",
                     h = 1,
                     alphas = c(0.3, 0.5, 0.7),
                     do.iter = TRUE,
                     nSample = 36,
                     do.parallel = TRUE)

require("doParallel")
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
out <- sento_model(sentMeas, y, ctr = ctrIter)
stopCluster(cl)
summary(out)

r <- plot(out)
r

attributions <- retrieve_attributions(out, sentMeas, do.normalize = FALSE)
names(attributions)

a <- plot_attributions(attributions, group = "features") +
  guides(fill = guide_legend(nrow = 1))
a

ctrMerge <- ctr_merge(sentMeas,
                      time = list(W = c("equal_weight", "linear")),
                      lexicons = list(LEX = c("LM_eng", "HENRY_eng")),
                      features = list(JOUR = c("wsj", "wapo")),
                      do.keep = FALSE)
sentMeasMerged <- merge_measures(ctrMerge)
sentMeasMerged[c("features", "lexicons", "time")]

glob <- to_global(sentMeas,
                  lexicons = c(0.10, 0.70, 0.20),
                  features = c(0.20, 0.20, 0.20, 0.20, 0.10, 0.10),
                  time = 1)

g <- ggplot(data = glob, aes(x = as.Date(row.names(glob)), y = global)) +
  geom_line() +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Sentiment") +
  ggthemes::theme_tufte(base_size = 12) +
  theme(legend.title = element_blank())
g

###

ggsave("C:/Users/gebruiker/Dropbox/SENTOMETRICS-R-PACKAGE/vignette/plots/sentmeas.pdf", p)
ggsave("C:/Users/gebruiker/Dropbox/SENTOMETRICS-R-PACKAGE/vignette/plots/for.pdf", r)
ggsave("C:/Users/gebruiker/Dropbox/SENTOMETRICS-R-PACKAGE/vignette/plots/attr.pdf", a)
ggsave("C:/Users/gebruiker/Dropbox/SENTOMETRICS-R-PACKAGE/vignette/plots/glob.pdf", g)

