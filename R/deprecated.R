
#' Datasets with defunct names
#'
#' These are datasets that have been renamed and the old names removed. Please change your code to use the new names.
#'
#' @docType data
#'
#' @name data-defunct
NULL

#' @rdname data-defunct
#' @name lexicons
#' @details The dataset \code{lexicons} is defunct, use \code{list_lexicons} instead.
NULL

#' @rdname data-defunct
#' @name valence
#' @details The dataset \code{valence} is defunct, use \code{list_valence_shifters} instead.
NULL

#' Deprecated functions
#'
#' Functions deprecated due to changed naming or because functionality is discarded. Deprecated functions are made defunct
#' every 1 major or every 2 minor package updates. See the NEWS file for more information about since when or why functions
#' have been deprecated.
#'
#' @name sentometrics-deprecated
NULL

#' @rdname sentometrics-deprecated
#'
#' @param sentomeasures a \code{sentomeasures} object created using \code{\link{sento_measures}}.
#' @param fill an element of \code{c("zero", "latest", NA)}; the first and last assume missing dates represent zero sentiment,
#' the second assumes missing dates represent constant sentiment.
#'
#' @seealso \code{\link{measures_fill}}
#'
#' @export
fill_measures <- function(sentomeasures, fill = "zero") {
  .Deprecated("measures_fill", package = "sentometrics")
  measures_fill(sentomeasures, fill = fill)
}

#' @rdname sentometrics-deprecated
#'
#' @param ... (other) allowed input arguments.
#'
#' @seealso \code{\link{measures_merge}}
#'
#' @export
merge_measures <- function(...) {
  .Deprecated("measures_merge", package = "sentometrics")
}

#' @rdname sentometrics-deprecated
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
  .Deprecated("measures_global", package = "sentometrics")
  measures_global(sentomeasures, lexicons = lexicons, features = features, time = time)
}

#' @rdname sentometrics-deprecated
#'
#' @export
ctr_merge <- function(...) {
  .Deprecated(package = "sentometrics")
}

#' @rdname sentometrics-deprecated
#'
#' @param subset a logical expression indicating the rows to keep.
#'
#' @seealso \code{\link{measures_subset}}
#'
#' @export
subset_measures <- function(sentomeasures, subset) {
  .Deprecated("measures_subset", package = "sentometrics")
  measures_subset(sentomeasures, subset = subset)
}

#' @rdname sentometrics-deprecated
#'
#' @param toSelect a \code{character} vector of the lexicon, feature and time weighting scheme names, to indicate which
#' measures need to be selected. One can also supply a \code{list} of such \code{character} vectors, in which case
#' \code{do.combine = TRUE} is set automatically, such that the separately specified combinations are selected.
#' @param do.combine a \code{logical} indicating if only measures for which all (\code{do.combine = TRUE}) or at least one
#' (\code{do.combine = FALSE}) of the selection components should occur in each sentiment measure's name in the selection. If
#' \code{do.combine = TRUE}, the \code{toSelect} argument can only consist of one lexicon, one feature, and one time weighting
#' scheme at maximum.
#'
#' @seealso \code{\link{measures_select}}
#'
#' @export
select_measures <- function(sentomeasures, toSelect, do.combine = TRUE) {
  .Deprecated("measures_select", package = "sentometrics")
  measures_select(sentomeasures, toSelect = toSelect, do.combine = do.combine)
}

#' @rdname sentometrics-deprecated
#'
#' @param sentocorpus the \code{sentocorpus} object created with \code{\link{sento_corpus}}, used for the construction
#' of the input \code{sentomeasures} object.
#' @param n a \code{numeric} value to indicate the number of dates associated to sentiment peaks to extract.
#' @param type a \code{character} value, either \code{"pos"}, \code{"neg"} or \code{"both"}, respectively to look
#' for the \code{n} dates related to the most positive, most negative or most extreme (in absolute terms) sentiment
#' occurrences.
#' @param do.average a \code{logical} to indicate whether peaks should be selected based on the average sentiment
#' value per date.
#'
#' @seealso \code{\link{peakdocs}}
#'
#' @export
extract_peakdocs <- function(sentomeasures, sentocorpus, n = 10, type = "both", do.average = FALSE) {
  .Deprecated("peakdocs")
  peakdocs(sentomeasures, sentocorpus, n = n, type = type, do.average = do.average)
}

