
####################
## DEPRECATED ######
####################

#' Deprecated functions
#'
#' Functions deprecated due to changed naming or because functionality is discarded. The general (but not
#' blindly followed) rule is that deprecated functions are made defunct every 1 major or every 2 minor
#' package updates. See the NEWS file for more information about since when or why functions have been
#' deprecated.
#'
#' @name sentometrics-deprecated
#' @keywords internal
NULL

####################
## DEFUNCT #########
####################

#' Datasets with defunct names
#'
#' These are datasets that have been renamed and removed.
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
#' @param ... allowed input arguments.
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
#' @param ... allowed input arguments.
#'
#' @export
fill_measures <- function(...) {
  .Defunct("measures_fill", package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param ... allowed input arguments.
#'
#' @export
merge_measures <- function(...) {
  .Defunct("aggregate.sento_measures", package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param ... allowed input arguments.
#'
#' @export
to_global <- function(...) {
  .Defunct("aggregate.sento_measures", package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param ... allowed input arguments.
#'
#' @export
subset_measures <- function(...) {
  .Defunct("subset", package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param ... allowed input arguments.
#'
#' @export
select_measures <- function(...) {
  .Defunct("subset", package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param ... allowed input arguments.
#'
#' @export
setup_lexicons <- function(...) {
  .Defunct("sento_lexicons", package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param ... allowed input arguments.
#'
#' @export
retrieve_attributions <- function(...) {
  .Defunct("attributions", package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param ... allowed input arguments.
#'
#' @export
perform_agg <- function(...) {
  .Defunct("aggregate.sentiment", package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param ... allowed input arguments.
#'
#' @export
plot_attributions <- function(...) {
  .Defunct("plot.attributions", package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param ... allowed input arguments.
#'
#' @export
almons <- function(...) {
  .Defunct("weights_almon", package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param ... allowed input arguments.
#'
#' @export
exponentials <- function(...) {
  .Defunct("weights_exponential", package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param ... allowed input arguments.
#'
#' @export
to_sentocorpus <- function(...) {
  .Defunct("as.sento_corpus", package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param ... allowed input arguments.
#'
#' @export
to_sentiment <- function(...) {
  .Defunct("as.sentiment", package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param ... allowed input arguments.
#'
#' @export
get_measures <- function(...) {
  .Defunct("as.data.table.sento_measures", package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param ... allowed input arguments.
#'
#' @export
measures_subset <- function(...) {
  .Defunct("subset", package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param ... allowed input arguments.
#'
#' @export
measures_select <- function(...) {
  .Defunct("subset", package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param ... allowed input arguments.
#'
#' @export
measures_delete <- function(...) {
  .Defunct("subset", package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param ... allowed input arguments.
#'
#' @export
sentiment_bind <- function(...) {
  .Defunct("merge", package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param ... allowed input arguments.
#'
#' @export
measures_merge <- function(...) {
  .Defunct("aggregate.sento_measures", package = "sentometrics")
}

#' @rdname sentometrics-defunct
#'
#' @param ... allowed input arguments.
#'
#' @export
measures_global <- function(...) {
  .Defunct("aggregate.sento_measures", package = "sentometrics")
}

