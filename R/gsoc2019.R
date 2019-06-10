#'
#' Summarize the sentocorpus object
#'
#' @author Andres Algaba, Sam Borms, Jeroen Van Pelt
#'
#' @description Summarizes the sentocorpus object and returns insights in features, tokens and the combination of the two over time.
#'
#'
#' @details This function summarizes the sentocorpus object and gives the user insight in the statistics of features and tokens over time.
#' Additional arguments can be passed to narrow down the insights. The \code{by} argument specifies the frequency intervals of the calculated statistics.
#' The \code{filter} argument allows to select a subset of the data (e.g. filter on specific features being equal to 1). The
#' \code{features} argument allows to select a subset of the features for which statistics are returned. All the statistics are also plotted. These plots
#' are all returned so the user can plot them seperately. We use the same tokenizer as in the sentiment
#' calculation in \code{\link{compute_sentiment}}.
#'
#' @param x is a \code{sentocorpus} object created with \code{\link{sento_corpus}}
#' @param filter a \code{character} vector that can be used to filter on the data in the \code{sentocorpus} object. As the sentocorpus is
#' translated to a \code{data.table}, the filter will be applied on the rows.
#' @param features a \code{character} vector that can be used to select a subset of the features to be analysed.
#' @param by a single \code{character} vector to specify the frequency interval on which the (over time) statistics need to be calculated
#'
#' @return returns a \code{list} containing:
#' \item{total_features} {a \code{data.table} with total counts for each feature in the corpus.}
#' \item{mean_featuers} {a \code{data.table} with the average for each feature in the corpus }
#' \item{features} {a \code{data.table} with the number of texts and feature counts per time interval, specified in the \code{by} argument }
#' \item{tokens} {a \code{data.table} with the total, mean, median, minimum and maximum of tokens per time interal specified in the \code{by} argument}
#' \item{plots} {a \code{list} with plots representing the above statistics }
#'
#' @examples
#'
#' data("usnews", package = "sentometrics")
#'
#' # Summary of entire corpus
#' corpus <- sento_corpus(usnews)
#' summary1 <- corpus_summarize(corpus)
#'
#' #Summary of economic texts
#' corpus <- sento_corpus(usnews)
#' summary2 <-corpus_summarize(corpus, filter="economy == 1")
#'
#' #Summary  economic and noneconomic features after the year 2000 where wsj equals 1
#' corpus <- sento_corpus(usnews)
#' summary3 <- corpus_summarize(corpus, filter="date > '2000-01-01' & wsj ==1 ", features =c("economy", "noneconomy"), by="week")
#'
#' @export
corpus_summarize <- function(x, filter = NULL, by = "day", features = NULL) {
  check_class(x, "sentocorpus")
  if (!(by %in% c("year", "month", "week", "day"))) {
    stop( paste0(by, " is no current 'by' option."))
  }
  dt <- data.table(x$documents, nTokens = as.numeric(sapply(tokenize_texts(quanteda::texts(x)), length)))[, !"texts"]

  if (!is.null(filter)) {
    dt <- dt[eval(parse(text = filter))]
  }
  if (!is.null(features)) {
    dt <- dt[, c(features, "date", "nTokens"), with = FALSE]
  }

  if (by == "year") {
    dt[, "date" := as.Date(paste0(format(as.Date(date), "%Y"), "-01-01"), "%Y-%m-%d")]
  } else if (by == "month") {
    dt[,"date":= as.Date(paste0(format(as.Date(date), "%Y-%m"), "-01"), "%Y-%m-%d")]
  } else if (by == "week") {
    dt[, "date" := as.Date(paste0(format(as.Date(date), "%Y-%W"), "1"), "%Y-%W%w")] # get first day of week based on ISO standard
  }

  features_dt <- dt[, !"nTokens"]
  tokens_dt <- dt[, .(nTokens, date)]

  freq_texts <- features_dt[, .("documents" = as.numeric(.N)), by=.(date)]
  freq_features <-features_dt[, lapply(.SD, function(x){ sum(ifelse(x > 0, 1, 0), na.rm = TRUE)}), by = date]
  freq_all <- merge(freq_texts, freq_features,  by="date")

  tokens_dt <- setnames(tokens_dt[,.(sum(nTokens), mean(nTokens), min(nTokens), max(nTokens)), by = date],c( "date", "totalTokens", "meanTokens","minTokens","maxTokens"))

  stats <- merge(tokens_dt, freq_all,  by = "date")
  setcolorder(stats, c("date", "documents"))

  df_feat <- melt(freq_all, id = "date", all = T)
  feat_plot <- ggplot(melt(freq_all, id = "date", all = T)) +
    geom_line(aes(x = date, y = value, color = variable, group = variable)) +
    ggtitle("Features over time") +
    theme_bw() +
    scale_x_date(name = "Date", date_labels = "%m-%Y") +
    scale_y_continuous(name = "Count")

  df_tok <- melt(tokens_dt[, !"totalTokens"], id = "date", all = T)
  tok_plot <- ggplot(df_tok) +
    geom_line(aes(x = date, y = value, color = variable, group = variable)) +
    ggtitle("Tokens over time") +
    theme_bw() +
    scale_x_date(name = "Date", date_labels = "%m-%Y") +
    scale_y_continuous(name = "Count")



  # p <- ggplot(data = measuresMelt, aes(x = date, y = value, color = variable)) +
  #   geom_line() +
  #   geom_hline(yintercept = 0, size = 0.50, linetype = "dotted") +
  #   scale_x_date(name = "Date", date_labels = "%m-%Y") +
  #   scale_y_continuous(name = "Sentiment") +
  #   theme_bw() +
  #   plot_theme(legendPos)
  # p


  summary <- list(
    stats = stats,
    plots_list = list(feature_plot = feat_plot,
                      token_plot = tok_plot
    )
  )
  return(summary)
}
#' Partial recomputation of sentomeasures
#'
#' @author Jeroen Van Pelt
#'
#' @description Partially recalculates the sentomeasures, based on the input parameters and/or changes in these parameters.
#'
#' @param sentocorpus a \code{sentocorpus} object created with \code{\link{sento_corpus}}.
#' @param sentomeasures \code{sentomeasures} object created with \code{\link{sento_measures}}
#' @param ctr output from a \code{\link{ctr_agg}} call.
#' @param sentiment the sentiment scores \code{data.table} with \code{"date"}, \code{"word_count"} and lexicon--feature
#' sentiment scores columns. The \code{"date"} column has the dates converted at the frequency for
#' across-document aggregation. All zeros are replaced by \code{NA} if \code{ctr$docs$weightingParam$do.ignoreZeros = TRUE}.
#' @param lexicons a \code{sentolexicons} object created with \code{\link{sento_lexicons}}.

#sentocorpus = NULL, sentomeasures = NULL, ctr = NULL, sentiment = NULL, lexicons = NULL
#' @return An updated \code{sentomeasures} object.
#'
#' @seealso \code{\link{sento_measures}}, \code{\link{compute_sentiment}}
#'
#' @examples
#' set.seed(505)
#'
#' #Update of across document weighting
#' data("usnews", package = "sentometrics")
#'  corpus <- sento_corpus( usnews)
#'  ctr <- ctr_agg(howTime = "linear", by = "year", lag = 3)
#'  l <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#'  sentomeasures <- sento_measures(corpus, l, ctr)
#'  ctr$docs$howDocs <- "exponential"
#'  ctr$docs$weightingParam$alphaExpDocs <- 0.3
#'  sentomeasures_updated <-measures_update(sentocorpus = corpus, lexicons = l, ctr = ctr)
#'
#'  #Update of across document weighting. Control variables and sentiment reused from sentomeasures object
#'  data("usnews", package = "sentometrics")
#'  corpus <- sento_corpus( usnews)
#'  ctr <- ctr_agg(howTime = "linear", by = "year", lag = 3)
#'  l <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#'  sentomeasures <- sento_measures(corpus, l, ctr)
#'  sentomeasures$ctr$docs$howDocs <- "inverseProportional"
#'  sentomeasures_updated <-measures_update(sentomeasures = sentomeasures)
#'
#'  #New texts in corpus with control variables (ctr) from old sentomeasures object. Sentiment only recalculated for new texts.
#'  data("usnews", package = "sentometrics")
#'  corpus1 <- sento_corpus(usnews[1:500,])
#'  ctr <- ctr_agg(howTime = "linear", by = "year", lag = 3)
#'  l <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#'  sentomeasures <- sento_measures(corpus1, l, ctr)
#'  corpus2 <- sento_corpus(usnews[1:2000,])
#'  sentomeasures_updated <- measures_update(sentocorpus = corpus2, lexicons = l, sentomeasures = sentomeasures)
#'
#'  #New texts in corpus with new control variables object (ctr). Sentiment only recalculated for new texts.
#'  data("usnews", package = "sentometrics")
#'  corpus1 <- sento_corpus(usnews[1:500,])
#'  ctr1 <- ctr_agg(howTime = "linear", by = "year", lag = 3)
#'  l <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#'  sentomeasures <- sento_measures(corpus1, l, ctr1)
#'  corpus2 <- sento_corpus(usnews[1:2000,])
#'  ctr2 <- ctr_agg(howTime = "linear", by = "year", lag = 3, howDocs = "inverseProportional")
#'  sentomeasures_updated <- measures_update(sentocorpus = corpus2, lexicons = l, ctr = ctr2)
#'
#'  #Update sentomeasures starting from sentiment and control variables without sentocorpus or sentomeasures object
#'  data("usnews", package = "sentometrics")
#'  corpus <- sento_corpus( usnews)
#'  ctr <- ctr_agg(howTime = "linear", by = "year", lag = 3)
#'  l <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#'  sentomeasures <- sento_measures(corpus, l, ctr)
#'  ctr$docs$howDocs <- "inverseProportional"
#'  sentiment <- sentomeasures$sentiment
#'  sentomeasures_updated <-measures_update(sentiment = sentiment, ctr = ctr)
#'
#' @export
measures_update <- function(sentocorpus = NULL, sentomeasures = NULL, lexicons = NULL) {
  if (is.null(sentocorpus)) {
    stop(paste0("Please provide a sentocorpus object."))
  }
  if (is.null(sentomeasures)) {
    stop(paste0("Please provide a sentomeasures object."))
  }
  if (is.null(lexicons)) {
    stop(paste0("Lexicon required for docs without sentiment"))
  } else {
    if(!setequal(get_dimensions(sentomeasures)$lexicons, names(lexicons)[names(lexicons)!="valence"])){
      stop(paste0("Provided lexicon names are not the same as lexicons used in sentomeasures object"))
    }
  }
  check_class(sentocorpus, "sentocorpus")

  ctr <- sentomeasures$ctr
  sentiment <- sentomeasures$sentiment

  partialCorpus <- quanteda::corpus_subset(sentocorpus, !quanteda::docnames(sentocorpus) %in% sentiment$id)
  if (length(partialCorpus$documents$texts) > 0) {
    partialSentiment <-  compute_sentiment(partialCorpus, lexicons, how = ctr$within$howWithin, nCore = ctr$nCore)
    sentiment <-  sentiment_bind(sentiment, partialSentiment)
    class(sentiment) <- c("sentiment", class(sentiment))
  }

  sentomeasures_updated <- aggregate(sentiment, ctr)
  return(sentomeasures_updated)
}


.compute_sentiment.VCorpus <- function(x, lexicons, how, tokens=NULL, nCore=1){
  x<-sento_corpus(data.table("id" = unname(unlist(meta(vcorp,"id"))), "date" = as.POSIXct(do.call("c", meta(vcorp,"datetimestamp"))),"texts" = as.character(vcorp$content)))
  compute_sentiment(x,lexicons,how,tokens,nCore)


}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.VCorpus <- compiler::cmpfun(.compute_sentiment.VCorpus)

.compute_sentiment.SimpleCorpus <- function(x, lexicons, how, tokens=NULL, nCore=1){
  #' Only language available in metadata so no transformation to sentocorpus
  nCore <- check_nCore(nCore)
  tok <- tokenize_texts(as.character(x$content), tokens)
  s <- compute_sentiment_lexicons(tok, lexicons, how, nCore) # compute sentiment per document for all lexicons
  s


}

#' @importFrom compiler cmpfun
#' @export
compute_sentiment.SimpleCorpus <- compiler::cmpfun(.compute_sentiment.SimpleCorpus)

