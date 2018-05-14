
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
  .Deprecated("measures_fill")
  measures_fill(sentomeasures, fill = fill)
}

#' @rdname sentometrics-deprecated
#'
#' @param ctr output from a \code{\link{ctr_merge}} call.
#'
#' @seealso \code{\link{measures_merge}}
#'
#' @export
merge_measures <- function(ctr){
  .Deprecated("measures_merge")
  measures_merge(ctr)
}

#' @rdname sentometrics-deprecated
#'
#' @param subset a logical expression indicating the rows to keep.
#'
#' @seealso \code{\link{measures_subset}}
#'
#' @export
subset_measures <- function(sentomeasures, subset) {
  .Deprecated("measures_subset")
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
  .Deprecated("measures_select")
  measures_select(sentomeasures, toSelect = toSelect, do.combine = do.combine)
}

