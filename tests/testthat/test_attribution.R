
context("Attribution")

library("sentometrics")
library("quanteda")

set.seed(123)

# corpus, lexicon and aggregation control creation
data("usnews")
corpus <- quanteda::corpus_sample(
  quanteda::corpus_subset(sento_corpus(corpusdf = usnews), date >= "1990-01-01" & date < "2000-10-01"),
  1000
)

data("list_lexicons")
lex <- setup_lexicons(list_lexicons[c("GI_en", "LM_en")])
ctrA <- ctr_agg(howWithin = "counts", howDocs = "proportional", howTime = "almon", by = "month",
                lag = 7, ordersAlm = 1:3, do.inverseAlm = TRUE, do.ignoreZeros = FALSE, fill = "latest")

sentomeasures <- sento_measures(corpus, lex, ctrA)

# preparation of estimation data
data("epu")
y <- epu[epu$date %in% get_dates(sentomeasures), "index"]
x <- data.frame(runif(length(y)), rnorm(length(y))) # two other (random) x variables
colnames(x) <- c("x1", "x2")

# model run
ctrM <- ctr_model(model = "gaussian", type = "Cp", do.iter = TRUE, h = 3, lambdas = NULL,
                  nSample = floor(0.90 * (length(y) - 3)), alphas = c(0.2, 0.7))
out <- sento_model(sentomeasures, y, x = x, ctr = ctrM)

### tests from here ###

attributions <- retrieve_attributions(out, sentomeasures, do.normalize = FALSE)

l <- rowSums(attributions$lexicons[, -1], na.rm = TRUE)
f <- rowSums(attributions$features[, -1], na.rm = TRUE)
t <- rowSums(attributions$time[, -1], na.rm = TRUE)
la <- rowSums(attributions$lags[, -1], na.rm = TRUE)
# d <- as.vector(sapply(attributions$documents, function(x) return(sum(x$attrib, na.rm = TRUE))))

TOL <- 1e-04

# retrieve_attributions
test_that("Attributions across all dimensions should be the same across rows", {
  expect_equal(l, f)
  expect_equal(l, t)
  expect_equal(l, la, tolerance = TOL)
  # expect_equal(l, d)
  expect_equal(f, t)
  expect_equal(f, la, tolerance = TOL)
  # expect_equal(f, d)
  expect_equal(t, la, tolerance = TOL)
  # expect_equal(t, d)
  # expect_equal(la, d)
})

# plot_attributions
p <- plot_attributions(attributions, group = sample(c("features", "lexicons", "time", "lags"), 1))
test_that("Plot is a ggplot object", {
  expect_true(inherits(p, "ggplot"))
})

