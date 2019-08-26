
####################
## DEPRECATED ######
####################

#' Deprecated functions
#'
#' Functions deprecated due to changed naming or because functionality is discarded. Deprecated functions are made defunct
#' every 1 major or every 2 minor package updates. See the NEWS file for more information about since when or why functions
#' have been deprecated.
#'
#' @name sentometrics-deprecated
#' @keywords internal
NULL

####################
## DEFUNCT #########
####################

#' Datasets with defunct names
#'
#' These are datasets that have been renamed and the old names removed. Please change your code to use the new names.
#'
#' @docType data
#'
#' @name data-defunct
#' @keywords internal
NULL

#' @rdname data-defunct
#' @name lexicons
#' @details The dataset \code{lexicons} is defunct, use \code{list_lexicons} instead.
NULL

#' @rdname data-defunct
#' @name valence
#' @details The dataset \code{valence} is defunct, use \code{list_valence_shifters} instead.
NULL

#' Defunct functions
#'
#' Functions defunct due to changed naming or because functionality is discarded. See the NEWS file for more information
#' about since when or why functions have been defunct.
#'
#' @name sentometrics-defunct
#' @keywords internal
NULL

#' @rdname sentometrics-defunct
#'
#' @param ... arguments not documented since function declared defunct.
#'
#' @export
ctr_merge <- function(...) {
  .Defunct(package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @export
perform_MCS <- function(...) {
  .Defunct(package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param sentomeasures an appropriate \code{sentomeasures} object created using \code{\link{sento_measures}}.
#' @param fill an element of \code{c("zero", "latest", NA)}; the first and last assume missing dates represent zero sentiment,
#' the second assumes missing dates represent constant sentiment.
#'
#' @seealso \code{\link{measures_fill}}
#'
#' @export
fill_measures <- function(sentomeasures, fill) {
  .Defunct("measures_fill", package = "sentometrics")
  # measures_fill(sentomeasures, fill = fill)
}

#' @rdname sentometrics-defunct
#'
#' @param ... (other) allowed input arguments.
#'
#' @seealso \code{\link{measures_merge}}
#'
#' @export
merge_measures <- function(...) {
  .Defunct("measures_merge", package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param lexicons a \code{numeric} vector of weights, of size \code{length(sentomeasures$lexicons)}, in the same order.
#' By default set to 1, which means equally weighted.
#' @param features a \code{numeric} vector of weights, of size \code{length(sentomeasures$features)}, in the same order.
#' By default set to 1, which means equally weighted.
#' @param time a \code{numeric} vector of weights, of size \code{length(sentomeasures$time)}, in the same order. By default
#' set to 1, which means equally weighted.
#'
#' @seealso \code{\link{measures_global}}
#'
#' @export
to_global <- function(sentomeasures, lexicons, features, time) {
  .Defunct("measures_global", package = "sentometrics")
  # measures_global(sentomeasures, lexicons = lexicons, features = features, time = time)
}

#' @rdname sentometrics-defunct
#'
#' @param subset a logical expression indicating the rows to keep.
#'
#' @seealso \code{\link{measures_subset}}
#'
#' @export
subset_measures <- function(sentomeasures, subset) {
  .Defunct("measures_subset", package = "sentometrics")
  # measures_subset(sentomeasures, subset = subset)
}

#' @rdname sentometrics-defunct
#'
#' @param toSelect a \code{character} vector of the lexicon, feature and time weighting scheme names, to indicate which
#' measures need to be selected, or as a \code{list} of \code{character} vectors, possibly with separately specified
#' combinations (only consisting of one lexicon, one feature, and one time weighting scheme at maximum).
#'
#' @seealso \code{\link{measures_select}}
#'
#' @export
select_measures <- function(sentomeasures, toSelect) {
  .Defunct("measures_select", package = "sentometrics")
  # measures_select(sentomeasures, toSelect = toSelect)
}

#' @rdname sentometrics-defunct
#'
#' @param lexiconsIn a named \code{list} of (raw) lexicons, each element as a \code{data.table} or a \code{data.frame} with
#' respectively a words column and a polarity score column. A subset of the already formatted built-in lexicons
#' accessible via \code{list_lexicons} should be passed here first.
#' @param valenceIn a single valence word list as a \code{data.table} or a \code{data.frame} with respectively a \code{"x"}
#' and a \code{"y"} or \code{"t"} column. The first column has the words, \code{"y"} has the values for bigram
#' shifting, and \code{"t"} has the types of the valence shifter for a clustered approach to sentiment calculation
#' (supported types: \code{1} = negators, \code{2} = amplifiers, \code{3} = deamplifiers). If three columns
#' are provided, the first two will be considered only. This argument can be one of the already formatted
#' built-in valence word lists accessible via \code{list_valence_shifters}. A word that appears in both a lexicon
#' and the valence word list is prioritized as a lexical entry during sentiment calculation. If \code{NULL}, no valence word
#' list is part of this function's output, and is thus not applied in the sentiment analysis.
#' @param do.split a \code{logical} that if \code{TRUE} splits every lexicon into a separate positive polarity and negative
#' polarity lexicon.
#'
#' @seealso \code{\link{sento_lexicons}}
#'
#' @export
setup_lexicons <- function(lexiconsIn, valenceIn, do.split) {
  .Defunct("sento_lexicons", package = "sentometrics")
  # sento_lexicons(lexiconsIn = lexiconsIn, valenceIn = valenceIn, do.split = do.split)
}

#' @rdname sentometrics-defunct
#'
#' @param model a \code{sentomodel} or \code{sentomodeliter} object created with \code{\link{sento_model}}.
#' @param do.normalize a \code{logical}, \code{TRUE} divides each element of every attribution vector at a given date by its
#' L2-norm at that date, normalizing the values between -1 and 1. The document attributions are not normalized. Or, for
#' \code{\link{almons}}, if \code{TRUE}, then polynomials should be normalized to unity.
#' @param refDates the dates (as \code{"yyyy-mm-dd"}) at which attribution is to be performed. These should be between the latest
#' date available in the input \code{sentomeasures} object and the first estimation sample date (that is, \code{model$dates[1]}
#' if \code{model} is a \code{sentomodel} object). All dates should also be in \code{get_dates(sentomeasures)}. If
#' \code{NULL} (default), attribution is calculated for all in-sample dates. Ignored if \code{model} is a \code{sentomodeliter}
#' object, for which attribution is calculated for all out-of-sample prediction dates.
#' @param factor the factor level as a single \code{character} vector for which attribution has to be calculated in
#' case of (a) multinomial model(s). Ignored for linear and binomial models.
#'
#' @seealso \code{\link{attributions}}
#'
#' @export
retrieve_attributions <- function(model, sentomeasures, do.normalize, refDates, factor) {
  .Defunct("attributions", package = "sentometrics")
  # attributions(model, sentomeasures, do.normalize = do.normalize, refDates = refDates, factor = factor)
}

#' @rdname sentometrics-defunct
#'
#' @param sentiment output from a \code{\link{compute_sentiment}} call, computed from a \code{sentocorpus} object.
#' @param ctr output from a \code{\link{ctr_agg}} call. The \code{howWithin} and \code{nCore} elements are ignored.
#'
#' @seealso \code{\link{aggregate}}
#'
#' @export
perform_agg <- function(sentiment, ctr) {
  .Defunct("aggregate", package = "sentometrics")
  # aggregate(sentiment = sentiment, ctr = ctr)
}

#' @rdname sentometrics-defunct
#'
#' @param attributions an \code{attributions} object created with \code{\link{attributions}}.
#' @param group a value from \code{c("lags", "lexicons", "features", "time")}.
#'
#' @seealso \code{\link{plot.attributions}}
#'
#' @export
plot_attributions <- function(attributions, group, ...) {
  .Defunct("plot.attributions", package = "sentometrics")
  # plot.attributions(x = attributions, group = group, ...)
}

#' @rdname sentometrics-defunct
#'
#' @param orders a \code{numeric} vector as the sequence of the Almon orders (cf., \emph{b}). The maximum value
#' corresponds to \emph{B}.
#' @param do.inverse \code{TRUE} if the inverse Almon polynomials should be calculated as well.
#'
#' @seealso \code{\link{weights_almon}}
#'
#' @export
almons <- function(n, orders, do.inverse, do.normalize) {
  .Defunct("weights_almon", package = "sentometrics")
  # weights_almon(n = n, orders = orders, do.inverse = do.inverse, do.normalize = do.normalize)
}

#' @rdname sentometrics-defunct
#'
#' @param alphas a \code{numeric} vector of decay factors.
#'
#' @seealso \code{\link{weights_exponential}}
#'
#' @export
exponentials <- function(n, alphas) {
  .Defunct("weights_exponential", package = "sentometrics")
  # weights_exponential(n = n, alphas = alphas)
}

