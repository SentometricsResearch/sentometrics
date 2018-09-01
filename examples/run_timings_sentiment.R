
###########################################
#### COMPARISON SENTIMENT COMPUTATIONS ####
###########################################

remove(list = ls())
options(digits = 2, scipen = 999)

info <- sessionInfo()
cat("\n")
cat(info$R.version$version.string, "\n")
cat(info$platform, "\n")
cat(info$locale, "\n \n")

###########################################

# library("sentometrics")
devtools::load_all()
library("lexicon")
library("dplyr")
library("tibble")
library("tidytext")
library("stringr")
library("sentimentr")
library("meanr")
library("syuzhet")
library("microbenchmark")

data("usnews", package = "sentometrics")

data("list_lexicons", package = "sentometrics")
data("list_valence_shifters", package = "sentometrics")
mine <- data.frame(w = c("uncertainty", "anxiety", "concern", "distrust", "worries"),
                   s = c(-2, -2, -2, -2, -2))
lexiconsIn <- c(
  list(myLexicon = mine),
  list_lexicons[c("LM_en", "HENRY_en")],
  list(
    nrc = lexicon::hash_sentiment_nrc,
    huliu = lexicon::hash_sentiment_huliu,
    sentiword = lexicon::hash_sentiment_sentiword,
    jockers = lexicon::hash_sentiment_jockers,
    senticnet = lexicon::hash_sentiment_senticnet
  )
)

lex <- setup_lexicons(lexiconsIn = lexiconsIn,
                      valenceIn = list_valence_shifters[["en"]])
lexPure <- lex[names(lex) != "valence"]

usnewsLarge <- sample(rep(usnews$texts, 25), 100000)
nTexts <- c(1, 5, 10, 25, 50, 75, 100) * 1000

########################################### definition of sentiment functions

sentoSimpleFunc <- function(texts) compute_sentiment(texts, lexicons = lex["huliu"], how = "counts")
sentoSimpleAllFunc <- function(texts) compute_sentiment(texts, lexicons = lexPure, how = "counts")
sentoValenceFunc <- function(texts) compute_sentiment(texts, lexicons = lex, how = "counts")
sentoValenceAllFunc <- function(texts) compute_sentiment(texts, lexicons = lex, how = "counts")
sentoValenceAllParFunc <- function(texts) compute_sentiment(texts, lexicons = lex, how = "counts", nCore = 8)

meanrFunc <- function(texts) meanr::score(texts, nthreads = 2)

tidytextFunc <- function(texts, lexicon) {
  tidyTexts <- tibble(text = texts) %>%
    mutate(linenumber = row_number()) %>%
    unnest_tokens(x, text, token = "words", strip_numeric = TRUE)
  wCounts <- tidyTexts %>%
    count(linenumber)
  sentiment <- tidyTexts %>%
    inner_join(tibble::as.tibble(lexicon), by = "x") %>%
    count(linenumber, y) %>%
    mutate(counts = y * n) %>%
    group_by(linenumber) %>%
    summarise(sentiment = sum(counts))
  N <- length(texts)
  sentiment <- sentiment %>%
    bind_rows(tibble(linenumber = (1:N)[which(!(1:N %in% sentiment[["linenumber"]]))], sentiment = 0)) %>%
    arrange(linenumber) %>%
    mutate(linenumber = wCounts[["n"]]) %>%
    rename(word_count = linenumber)
  sentiment
}

tidytextFuncHuliu <- function(texts) tidytextFunc(texts, lex$huliu)
tidytextFuncSenticNet <- function(texts) tidytextFunc(texts, tibble::as.tibble(lexicon::hash_sentiment_senticnet))

sentimentrFunc <- function(texts) {
  sents <- sentimentr::get_sentences(texts)
  sentimentr::sentiment_by(sents, averaging.function = average_mean,
                           polarity_dt = lex[["huliu"]],
                           valence_shifters_dt = lexicon::hash_valence_shifters, n.before = 1, n.after = 0)
}

syuzhetFunc <- function(texts) syuzhet::get_sentiment(texts, method = "syuzhet")

###########################################

########################################### intermediate checks

# K <- tail(nTexts, 1)
#
# system.time(s1 <- sentoSimpleFunc(usnewsLarge[1:K]))
# system.time(s2 <- tidytextFuncHuliu(usnewsLarge[1:K]))
# sum(s1$word_count == s2$word_count) == K & sum(s1$huliu == s2$sentiment) == K
#
# system.time(s3 <- sentoSimpleAllFunc(usnewsLarge[1:K]))
# sum(s1$word_count == s3$word_count) == K & sum(s1$huliu == s3$huliu) == K
#
# system.time(s4 <- sentoValenceFunc(usnewsLarge[1:K]))
# system.time(s5 <- sentoValenceAllFunc(usnewsLarge[1:K]))
# sum(s4$word_count == s5$word_count) == K & sum(s4$huliu == s5$huliu) == K
#
# system.time(s6 <- compute_sentiment(usnewsLarge[1:K], lexicons = lex[c("huliu")], how = "proportional"))
# system.time(s7 <- compute_sentiment(usnewsLarge[1:K], lexicons = lexPure, how = "proportional"))
# sum(s6$word_count == s7$word_count) == K & sum(s6$huliu == s7$huliu) == K
#
# system.time(s8 <- compute_sentiment(usnewsLarge[1:K], lexicons = lex[c("huliu")], how = "proportionalPol"))
# system.time(s9 <- compute_sentiment(usnewsLarge[1:K], lexicons = lexPure, how = "proportionalPol"))
# sum(s8$word_count == s9$word_count) == K & sum(s8$huliu == s9$huliu) == K

###########################################

timingsFull <- lapply(nTexts, function(n) {
  cat("Run timings for texts size of", n, "\n")
  texts <- usnewsLarge[1:n]
  out <- microbenchmark(
    sentoSimpleFunc(texts),
    sentoValenceFunc(texts),
    sentoValenceAllFunc(texts),
    # sentoValenceAllParFunc(texts),
    meanrFunc(texts),
    tidytextFuncHuliu(texts),
    tidytextFuncSenticNet(texts),
    syuzhetFunc(texts),
    # sentimentrFunc(texts),
    times = 5,
    unit = "s"
  )
  out
})

timingsAll <- do.call(rbind,
                      lapply(timingsFull, function(timing) summary(timing)[, "mean"])
)
colnames(timingsAll) <- c("sentometrics", "sentometrics_valence", "sentometrics_valence_all_lexicons",
                          "meanr", "tidytext_huliu", "tidytext_senticnet", "syuzhet")
timings <- data.table(texts = nTexts, timingsAll)
timings

########################################### tabular LaTeX output

output <- NULL
for (row in 1:nrow(timings)) {
  output <- paste0(
    output,
    timings[row, 1], " & ",
    paste0(paste0(format(round(timings[row, -1], 2), nsmall = 2), collapse = " & "), " \\\\", "\n")
  )
}
cat(output)

###########################################

