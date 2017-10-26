
library(sentometrics)
context("Attribution")

library(quanteda)
library(data.table)

# corpus, lexicon and ctr creation
data("usnews")
corpus <- quanteda::corpus_subset(sento_corpus(corpusdf = usnews), date >= "1990-01-01" & date < "2000-10-01")
data("lexicons")
lex <- lexicons[c("GI_eng", "LM_eng")]
ctr <- ctr_agg(howWithin = "tf-idf", howDocs = "proportional", howTime = "almon", by = "month",
               lag = 3, ordersAlm = 1:3, do.inverseAlm = TRUE, do.normalizeAlm = TRUE)
sentomeasures <- sento_measures(corpus, lex, ctr)

y <- epu[epu$date %in% sentomeasures$measures$date, ]$index
x <- data.frame(runif(length(y)), rnorm(length(y))) # two other (random) x variables
colnames(x) <- c("x1", "x2")
ctr <- ctr_model(model = "gaussian", type = "Cp", do.iter = TRUE, h = 3,
                 nSample = floor(0.95 * (length(y) - 3)), alphas = c(0.2, 0.7, 0.9))
out <- sento_model(sentomeasures, y, x = x, ctr = ctr)

# tests from here
attributions <- retrieve_attributions(out, sentomeasures, do.normalize = FALSE)

l <- rowSums(attributions$lexicons[, -1])
f <- rowSums(attributions$features[, -1])
t <- rowSums(attributions$time[, -1])
d <- as.vector(sapply(attributions$documents, function(x) return(sum(x$attrib))))

test_that("Attributions across all dimensions should be the same across rows", {
  expect_equal(l, f)
  expect_equal(l, t)
  expect_equal(l, d)
  expect_equal(f, t)
  expect_equal(f, d)
  expect_equal(t, d)
})

