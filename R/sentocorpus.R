
#' Create a sentocorpus object
#'
#' @author Samuel Borms
#'
#' @description Formalizes the structure of a collection of texts into a well-defined corpus, by calling the \code{corpus()}
#' constructor from the \pkg{quanteda} package, performing a set of checks and preparing it for further sentiment analysis.
#'
#' @details A \code{sentocorpus} object can be regarded as a specialized instance of a \pkg{quanteda} corpus. In theory, all
#' \pkg{quanteda} functions applicable to its corpus object can also be applied to a \code{sentocorpus} object. However,
#' changing a given \code{sentocorpus} object too drastically using some of \pkg{quanteda}'s functions might alter the very
#' structure the corpus is meant to have (as defined in the \code{"corpusdf"} argument) to be able to be used as an input
#' in other functions of the \pkg{sentometrics} package. There are functions, including \code{quanteda::corpus_sample()} or
#' \code{quanteda::corpus_subset()}, that do not change the actual corpus structure and may come in handy. To add additional
#' additional features, we recommend to use \code{add_features()}. In the future, we will formalize the interaction between the
#' \pkg{quanteda} package (as well as other text mining packages).
#'
#' @param corpusdf a \code{data.table} (or \code{data.frame}) with as named columns and \emph{in this order}: a document
#' \code{id} column, a \code{date} column, a \code{text} column (i.e. the columns where all texts to analyze reside), and a
#' series of feature columns of type \code{numeric}, with values pointing to the applicability of a particular feature to
#' a particular text. The latter columns are often binary (1 means the feature is applicable to the document in the same row)
#' or as a percentage to specify the degree of connectedness of a feature to a document. Features could be topics (e.g. legal,
#' political or economic), but also article sources (e.g. online or printed press), amongst many more options. If no particular
#' features are of interest to your analysis, have only one additional column with all values set to 1. Provide the \code{date}
#' column as \code{"yyyy-mm-dd"}. All spaces in the names of the features are automatically replaced by underscores.
#' @param do.clean a \code{logical}, if \code{TRUE} all texts undergo a cleaning routine to eliminate common textual
#' garbage. This includes a brute force replacement of HTML tags and non-alphanumeric characters by an empty string.
#'
#' @return A \code{sentocorpus} object, derived from a \pkg{quanteda} corpus classed list keeping the elements \code{documents},
#' \code{metadata} and\code{settings}. The \code{documents} element incorporates the corpus represented as a \code{data.table}.
#'
#' @seealso \code{\link[quanteda]{corpus}}
#'
#' @examples
#' data("useconomynews")
#'
#' # corpus construction
#' corpus <- sento_corpus(corpusdf = useconomynews)
#'
#' # take a random subset using a quanteda's package function
#' corpusSmall <- quanteda::corpus_sample(corpus, size = 500)
#'
#' @export
sento_corpus <- function(corpusdf, do.clean = FALSE) {

  # check for presence of id, date and text columns
  nonfeatures <- c("id", "date", "text")
  cols <- stringi::stri_replace_all(colnames(corpusdf), "_", regex = " ")
  colnames(corpusdf) <- cols
  if (!all(nonfeatures %in% cols) | (cols[1] != "id" | cols[2] != "date" | cols[3] != "text"))
    stop("The input data.table (data.frame) should have its first columns named 'id', 'date' and 'text', in this order.")
  # check for type of text column
  if (!is.character(corpusdf[["text"]])) stop("The 'text' column should be of type character.")
  # check for date format
  dates <- as.Date(corpusdf$date, format = "%Y-%m-%d")
  if (any(is.na(dates))) stop("At least some dates are not in appropriate format. Should be 'yyyy-mm-dd'.")
  else corpusdf$date <- dates
  # check for duplicated feature names, if no issues add to output list
  features <- cols[!(cols %in% nonfeatures)]
  if (sum(duplicated(features)) > 0) {
    duplics <- unique(features[duplicated(features)])
    stop(paste0("Names of feature columns are not unique. Following names occur at least twice: ",
                paste0(duplics, collapse = ", "), "."))
  }
  corpusdf <- as.data.table(corpusdf)
  isNumeric <- sapply(features, function(x) return(is.numeric(corpusdf[[x]])))
  if (any(!isNumeric)) {
    toDrop <- names(isNumeric)[isNumeric == FALSE]
    corpusdf <- corpusdf[, (toDrop) := NULL]
    warning(paste0("Following feature columns were dropped as they are not of type numeric: ", paste0(toDrop, collapse = ", "), "."))
    if (length(toDrop) == length(isNumeric))
      stop("No remaining feature columns. Please add uniquely named feature columns of type numeric.")
  }
  if (do.clean) corpusdf <- clean(corpusdf)

  # construct corpus as a quanteda corpus
  c <- quanteda::corpus(x = corpusdf, docid_field = "id", text_field = "text",
                        metacorpus = list(info = "This is a sentocorpus object directly based on the quanteda corpus."))
  c$tokens <- NULL

  class(c) <- c("sentocorpus", class(c))

  return(c)
}

clean <- function(corpusdf) {
  corpusdf$text <- stringi::stri_replace_all(corpusdf$text, replacement = "", regex = "<.*?>") # html tags
  corpusdf$text <- stringi::stri_replace_all(corpusdf$text, replacement = "", regex = '[\\"]')
  corpusdf$text <- stringi::stri_replace_all(corpusdf$text, replacement = "", regex = "[^-a-zA-Z0-9,&. ]")
  return(corpusdf)
}

#' Add feature columns to a sentocorpus
#'
#' @author Samuel Borms
#'
#' @description Adds new named feature columns to provided \code{sentocorpus} object.
#'
#' @param sentocorpus a \code{sentocorpus} object.
#' @param featuresdf a named \code{data.table} (or \code{data.frame}) with as columns the new features of type \code{numeric}
#' to add to the \code{sentocorpus} inputted. If the number of rows in \code{featuresdf} is not equal to the number of documents
#' in \code{sentocorpus}, recycling will occur.
#'
#' @return An updated \code{sentocorpus} object.
#'
#' @examples
#' data("useconomynews")
#'
#' # construct a corpus and add random features to it
#' corpus <- sento_corpus(corpusdf = useconomynews)
#' corpus <- add_features(corpus,
#'                        featuresdf = data.table(random = runif(quanteda::ndoc(corpus))))
#'
#' @export
add_features <- function(sentocorpus, featuresdf) {
  check_class(sentocorpus, "sentocorpus")
  features <- colnames(featuresdf)
  isNumeric <- sapply(features, function(x) return(is.numeric(featuresdf[[x]])))
  toAdd <- which(isNumeric)
  for (i in toAdd) {
    quanteda::docvars(sentocorpus, field = features[i]) <- featuresdf[[i]]
  }
  if (length(toAdd) != length(isNumeric))
    warning(paste0("Following columns were not added as they are not of type numeric: ", names(which(!isNumeric))))
  return(sentocorpus)
}

