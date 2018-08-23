
context("Model specification")

library("sentometrics")
library("quanteda")

set.seed(123)

# corpus, lexicon and aggregation control creation
data("usnews")
corpus <- quanteda::corpus_sample(quanteda::corpus_subset(sento_corpus(corpusdf = usnews)), 750)

data("list_lexicons")
lex <- list_lexicons[c("GI_en", "LM_en")]
ctrA <- ctr_agg(howWithin = "proportional", howDocs = "proportional", howTime = "almon", by = "month",
                lag = 7, ordersAlm = 1:3, do.inverseAlm = TRUE, do.ignoreZeros = FALSE, fill = "latest")

sentomeasures <- sento_measures(corpus, lex, ctrA)

# preparation of estimation data
data("epu")
idx <- sample(1:nrow(epu), nobs(sentomeasures), replace = TRUE)
y <- epu[idx, "index"]
yb <- epu[idx, "above"]
ym <- epu[idx, "aboveMulti"]
x <- data.frame(runif(length(y)), rnorm(length(y))) # two other (random) x variables
colnames(x) <- c("x1", "x2")

### tests from here ###

N <- nrow(x)
nSample <- floor(0.95 * N)

ctrM1 <- ctr_model(model = "gaussian", type = "Cp", h = 8, alphas = c(0.2, 0.7))
out1 <- sento_model(sentomeasures, y, x = x, ctr = ctrM1)

ctrM2 <- ctr_model(model = "gaussian", type = "AIC", h = -1, alphas = c(0.2, 0.7))
out2 <- sento_model(sentomeasures, y, x = x, ctr = ctrM2)

ctrM3 <- ctr_model(model = "gaussian", type = "BIC", h = 0, alphas = c(0.2, 0.7))
out3 <- sento_model(sentomeasures, y, x = x, ctr = ctrM3)

ctrM4 <- ctr_model(model = "gaussian", type = "cv", h = 3, trainWindow = nrow(x) - 30, testWindow = 7, alphas = c(0.2, 0.7))
out4 <- sento_model(sentomeasures, y, x = x, ctr = ctrM4)

ctrM5 <- ctr_model(model = "binomial", type = "cv", h = 1, trainWindow = nrow(x) - 15, testWindow = 10, alphas = c(0.2, 0.7))
out5 <- sento_model(sentomeasures, yb, x = x, ctr = ctrM5)

ctrM6 <- ctr_model(model = "multinomial", type = "cv", h = 5, trainWindow = nrow(x) - 21, testWindow = 6, alphas = c(0.2, 0.7))
out6 <- sento_model(sentomeasures, ym, x = x, ctr = ctrM6)

ctrM7 <- ctr_model(model = "gaussian", type = "AIC", do.difference = TRUE, h = 1, alphas = c(0.2, 0.7), lambdas = 50:1)
out7 <- sento_model(sentomeasures, y, x = x, ctr = ctrM7)

ctrM8 <- ctr_model(model = "gaussian", type = "Cp", h = 1, alphas = c(0, 0.4, 1),
                   nSample = nSample, do.iter = TRUE, start = 2)
out8 <- sento_model(sentomeasures, y, x = x, ctr = ctrM8)

ctrM9 <- ctrM8
ctrM9$nSample <- N - 1 - 2 + 1

ctrM10 <- ctr_model(model = "gaussian", type = "Cp", h = 1, alphas = 0,
                    nSample = nSample, do.iter = TRUE, start = 2, nCore = 4)
out10 <- sento_model(sentomeasures, y, x = x, ctr = ctrM10)

ctrM11 <- ctr_model(model = "gaussian", type = "Cp", h = 1, alphas = 1,
                    nSample = nSample, do.iter = TRUE, start = 2)
out11 <- sento_model(sentomeasures, y, x = x, ctr = ctrM11)

ctrM12 <- ctr_model(model = "gaussian", type = "Cp", h = 1, alphas = c(0, 0.4, 1),
                    nSample = nSample, do.iter = TRUE, start = 2)
out12 <- sento_model(measures_select(sentomeasures, c("LM_en", "wsj", "almon1")),
                     y, x = cbind(x, measures_global(sentomeasures)), ctr = ctrM12)

# sento_model
test_that("Different model specifications give specified output", {
  expect_equal(N - 8, nrow(out1$x))
  expect_equal(N - 1, nrow(out2$x))
  expect_equal(N - 3, nrow(out4$x))
  expect_equal(N - 1, nrow(out5$x))
  expect_equal(N - 5, nrow(out6$x))
  expect_equal(N - 1, nrow(out7$x))
  expect_equal(nSample, nrow(out8$models[[1]]$x))
  expect_true(all(c(out1$alpha, out2$alpha, out3$alpha, out4$alpha, out5$alpha, out6$alpha, out7$alpha) %in% c(0.2, 0.7)))
  expect_true(all(out8$alphas %in% c(0, 0.4, 1)))
  expect_equal(out7$lambda, (50:1)[which(out7$ic$matrix == min(out7$ic$matrix, na.rm = TRUE), arr.ind = TRUE)[1, 2]])
  expect_equal(N - 1 - nSample - 2 + 1, length(out8$models))
  expect_true(all(sapply(c(list(out1, out2, out3, out4, out5, out7), out8$models),
                         function(out) stats::coef(out$reg)[c("x1", "x2"), ]) != 0))
  expect_error(sento_model(sentomeasures, y, x = x, ctr = ctrM9))
  expect_null(summary(out1))
  expect_null(summary(out5))
  expect_null(summary(out6))
  expect_null(summary(out8))
})

# perform_MCS
models <- list(elnet = out8, ridge = out10, lasso = out11, elnetLM = out12)
test_that("Model confidence set procedure works and fails if needed", {
  expect_true(inherits(suppressWarnings(perform_MCS(models, loss = "DA")), "SSM"))
  expect_true(inherits(suppressWarnings(perform_MCS(models, loss = "errorSq")), "SSM"))
  expect_true(inherits(suppressWarnings(perform_MCS(models, loss = "AD")), "SSM"))
  expect_error(perform_MCS(models, loss = "accuracy"))
  expect_error(perform_MCS(list(wrong = out1, elnet = out8), loss = "errorSq"))
})

# summary.sentomodel, summary.sentomodeliter, print.sentomodel, print.sentomodeliter
test_that("No output returned when object summarized or printed", {
  expect_null(summary(out1))
  expect_null(summary(out5))
  expect_null(summary(out6))
  expect_null(summary(out8))
  expect_null(print(out1))
  expect_null(print(out5))
  expect_null(print(out6))
  expect_null(print(out8))
})

# plot.sentomodeliter
p <- plot(out8)
test_that("Plot is a ggplot object", {
  expect_true(inherits(p, "ggplot"))
})

# ctr_model
test_that("Modelling control function breaks when wrong inputs supplied", {
  expect_error(ctr_model(model = "stuck", type = "puzzle", do.intercept = "yes", do.difference = "yep", nCore = "yip"))
  expect_error(ctr_model(alphas = c(-1, 0.4, 0.7, 1.2), lambdas = seq(-10, 100, by = 2), oos = -4, start = 0))
  expect_error(ctr_model(type = "cv"))
  expect_error(ctr_model(type = "cv", trainWindow = 0, testWindow = 0))
  expect_error(ctr_model(do.iter = TRUE))
  expect_error(ctr_model(model = "multinomial", type = "cv", do.iter = TRUE,
                         trainWindow = 7, testWindow = 12, nSample = 10))
})

