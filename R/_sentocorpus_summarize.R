#'
#' Summarize the sentocorpus object
#'
#' @author Jeroen Van Pelt
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
  feature_token_dt <- dt[, lapply(.SD * nTokens, mean), by = date][, !"nTokens"]

  freq_texts <- features_dt[, .("nTexts" = as.numeric(.N)), by=.(date)]
  freq_features <-features_dt[, lapply(.SD, function(x) sum(as.numeric(x), na.rm = TRUE)), by = date]
  freq_all <- merge(freq_texts, freq_features, by="date")

  tokens_dt <- setnames(tokens_dt[,.(sum(nTokens), mean(nTokens), median(nTokens), min(nTokens), max(nTokens)), by = date],c( "date", "nTokens", "mean","median","min","max"))

  mean_dt <- dt[, !"nTokens"][, ! "date"][, lapply(.SD, mean)][,row := as.numeric( .I)]
  sum_dt <- dt[, !"nTokens"][, ! "date"][, lapply(.SD, sum)][,row := as.numeric( .I)]

  df_feat <- melt(freq_all, id = "date", all = T)
  feat_plot <- ggplot(melt(freq_all, id = "date", all = T)) + geom_line(aes(x = date, y = value, color = variable, group = variable)) + ggtitle("Feature counts over time") + theme_minimal()
  df_tok <- melt(tokens_dt[, !"nTokens"], id = "date", all = T)
  tok_plot <- ggplot(df_tok) + geom_line(aes(x = date, y = value, color = variable, group = variable)) + ggtitle("Tokens stats over time") + theme_minimal()
  df_feat_token <- melt(feature_token_dt, id = "date", all = T)
  feat_token_plot <- ggplot(df_feat_token) + geom_line(aes(x = date, y = value, color = variable, group = variable)) + ggtitle("Average token by feature over time") + theme_minimal()
  df_mean <- melt(mean_dt, id = "row")
  mean_feat_plot <- ggplot(df_mean, aes(variable, value )) + geom_col() + ggtitle("Feature average in Corpus") + theme_minimal()
  df_sum <- melt(sum_dt, id = "row")
  total_feat_plot <- ggplot(df_sum, aes(variable, value )) + geom_col() + ggtitle("Feature count in Corpus") + theme_minimal()

  summary <- list(
    total_features = sum_dt[,!"row"],
    mean_features = mean_dt[,!"row"],
    features = freq_all,
    tokens = tokens_dt,
    features_tokens = feature_token_dt,
    plots_list = list(feature_plot = feat_plot,
                      token_plot = tok_plot,
                      feature_token_plot = feat_token_plot,
                      mean_feature_plot =  mean_feat_plot,
                      total_feature_plot = total_feat_plot)
  )
  return(summary)
}
