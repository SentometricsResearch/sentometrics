
context("Methods sentomeasures")

library("data.table")
library("quanteda")

set.seed(123)

# corpus, lexicon and aggregation control creation
data("usnews")
corpus <- quanteda::corpus_sample(sento_corpus(corpusdf = usnews), size = 600)

data("list_lexicons")
lex <- sento_lexicons(list_lexicons[c("HENRY_en", "LM_en")])
ctr <- ctr_agg(howWithin = "counts", howDocs = "proportional", howTime = c("linear", "exponential"), by = "day",
               lag = 60, alphasExp = c(0.1, 0.6))

sentMeas <- sento_measures(corpus, lex, ctr)

### tests from here ###

# diff
N <- nobs(sentMeas)
M <- nmeasures(sentMeas)
test_that("Differencing is properly done", {
  expect_equal(nobs(diff(sentMeas, lag = 1)), N - 1)
  expect_equal(nobs(diff(sentMeas, lag = 2, differences = 3)), N - 2 * 3)
})

# scale
s1 <- scale(sentMeas)
s2 <- suppressWarnings(scale(sentMeas, center = -as.matrix(as.data.table(sentMeas)[, -1]), scale = FALSE))
s3 <- scale(sentMeas, center = as.numeric(sentMeas$stats["mean", ]), scale = as.numeric(sentMeas$stats["sd", ]))
s4 <- scale(sentMeas,
            center = -matrix(as.numeric(sentMeas$stats["mean", ]), nrow = N, ncol = M, byrow = TRUE),
            scale = matrix(as.numeric(sentMeas$stats["sd", ]), nrow = N, ncol = M, byrow = TRUE))
test_that("Scaling is properly done", {
  expect_equal(rowMeans(s1$stats["mean", ], na.rm = TRUE), c(mean = 0))
  expect_equal(rowMeans(s1$stats["sd", ], na.rm = TRUE), c(sd = 1))
  expect_equal(rowMeans(s2$stats["mean", ], na.rm = TRUE), c(mean = 0))
  expect_equal(rowMeans(s2$stats["sd", ], na.rm = TRUE), c(sd = 0))
  expect_equal(s1$stats["mean", ], s3$stats["mean", ])
  expect_equal(s1$stats["sd", ], s3$stats["sd", ])
  expect_equal(s1$stats["mean", ], s4$stats["mean", ])
  expect_equal(s1$stats["sd", ], s4$stats["sd", ])
})

# summary.sentomeasures, print.sentomeasures
cat("\n")
test_that("No output returned when object summarized or printed", {
  expect_null(summary(sentMeas))
  expect_null(print(sentMeas))
})

# plot.sentomeasures
p <- plot(sentMeas, group = sample(c("features", "lexicons", "time"), 1))
test_that("Plot is a ggplot object", {
  expect_true(inherits(p, "ggplot"))
})

# as.data.table, measures_to_long
measuresLong <- as.data.table(sentMeas, format = "long")
test_that("Proper long formatting of sentiment measures", {
  expect_true(nrow(measuresLong) == nobs(sentMeas) * nmeasures(sentMeas))
  expect_true(all(sentMeas$lexicons %in% unique(measuresLong[["lexicons"]])))
  expect_true(all(sentMeas$features %in% unique(measuresLong[["features"]])))
  expect_true(all(sentMeas$time %in% unique(measuresLong[["time"]])))
  expect_true(all(as.data.table(sentMeas)[["date"]] %in% unique(measuresLong[["date"]])))
})

# as.data.frame
test_that("Proper data.frame conversion", {
  expect_true(class(as.data.frame(sentMeas)) == "data.frame")
})

