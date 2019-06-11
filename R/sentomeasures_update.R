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
