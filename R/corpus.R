
#' Create a corpus
#'
#' @description Assembles a collection of texts (corpus), by calling the \code{corpus()} instructor from the \pkg{quanteda}
#' package and prepare it for further analysis.
#'
#' @param texts a \code{data.frame} (or \code{data.table} for that matter) with as named columns: a document \code{id} column,
#' a \code{date} column, a \code{text} column (i.e. the columns where all texts to analyze reside), and a series of
#' \code{feature} columns, with values pointing to the applicability of a particular feature to a particular text. The latter
#' columns are often binary (1 means the feature is applicable to the document in the same row) or as a percentage to specify
#' the degree of connectedness of a feature to a document. Features could be for example topics (e.g. economic, political or
#' legal), but also article sources (e.g. online or printed press), amongst many more possibilities. Please provide the
#' \code{date} column with character values formatted as \code{"yyyy-mm-dd"}.
#'
#' @return A \code{corpuS} object.
#'
#' @seealso \code{\link[quanteda]{corpus}}
#'
#' @export
sento_corpus <- function(texts) {

  # construct corpus as a quanteda corpus
  c <- quanteda::corpus(x = texts,
                        docid_field = "id",
                        text_field = "text",
                        metacorpus = list(info = "This is a Sentometrics corpuS directly based on the quanteda corpus."))

  # check for duplicated feature names, if no issues add to output list
  features <- names(quanteda::docvars(c))[names(quanteda::docvars(c)) != "date"]
  if (sum(duplicated(features)) > 0) {
    duplics <- unique(features[duplicated(features)])
    stop(paste0("Names of features are not unique. Following names occur at least twice: ",
            paste0(duplics, collapse = ", "), "."))
  }

  c$tokens <- NULL
  c$features <- features

  class(c) <- c("corpuS", class(c))

  return(c)
}

#' Add feature columns to corpuS
#'
#' @description Adds new named feature columns to provided \code{corpuS} object.
#'
#' @param corpuS a \code{corpuS} object.
#' @param features a named \code{data.frame} with as columns the new features to add to the \code{corpuS} inputted.
#'
#' @return An updated \code{corpuS} object.
#'
#' @export
add_features <- function(corpuS, features) {

  check_class(corpuS, "corpuS")

  for (i in seq_along(features)) {
    quanteda::docvars(corpuS, field = names(features)[i]) <- features[[i]]
  }

  # update features vector
  corpuS$features <- c(corpuS$features, colnames(features))

  return(corpuS)
}

