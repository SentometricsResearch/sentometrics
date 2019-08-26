
context("Attribution")

library("sentometrics")
library("quanteda")

set.seed(123)

# corpus, lexicon and aggregation control creation
data("usnews")
corpus <- quanteda::corpus_sample(
  quanteda::corpus_subset(sento_corpus(corpusdf = usnews), date >= "1997-01-01" & date <= "2000-12-01"),
  500
)

data("list_lexicons")
lex <- sento_lexicons(list_lexicons[c("GI_en", "LM_en")])
ctrA <- ctr_agg(howWithin = "counts", howDocs = "proportional", howTime = "almon", by = "day",
                lag = 24, ordersAlm = 1:3, do.inverseAlm = TRUE, do.ignoreZeros = FALSE, fill = "latest")

sento_measures <- sento_measures(corpus, lex, ctrA)

# preparation of estimation data
N <- nobs(sento_measures)
y <- rnorm(N) # random y variable
x <- data.frame(runif(N), rnorm(N)) # two additional random x variables
colnames(x) <- c("x1", "x2")

# model run
ctrM <- ctr_model(model = "gaussian", type = "Cp", do.iter = TRUE, h = 3, lambdas = NULL,
                  nSample = N - 12, do.shrinkage.x = TRUE, alphas = 0)
out <- sento_model(sento_measures, y, x = x, ctr = ctrM)

### tests from here ###

attributions <- attributions(out, sento_measures, do.normalize = FALSE)

l <- rowSums(attributions$lexicons[, -1], na.rm = TRUE)
f <- rowSums(attributions$features[, -1], na.rm = TRUE)
t <- rowSums(attributions$time[, -1], na.rm = TRUE)
la <- rowSums(attributions$lags[, -1], na.rm = TRUE)
# d <- as.vector(sapply(attributions$documents, function(x) return(sum(x$attrib, na.rm = TRUE))))

TOL <- 1e-04

# attributions
test_that("Attributions across all dimensions should be the same across rows", {
  expect_equal(l, f)
  expect_equal(l, t)
  expect_equal(l, la, tolerance = TOL)
  # expect_equal(l, d) # does not hold because fill = "latest"
  expect_equal(f, t)
  expect_equal(f, la, tolerance = TOL)
  # expect_equal(f, d)
  expect_equal(t, la, tolerance = TOL)
  # expect_equal(t, d)
  # expect_equal(la, d)
})

# plot.attributions
p <- plot(attributions, group = sample(c("features", "lexicons", "time", "lags"), 1))
test_that("Plot is a ggplot object", {
  expect_true(inherits(p, "ggplot"))
})

