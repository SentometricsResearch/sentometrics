
#' Create a sentocorpus object
#'
#' @description Assembles a collection of texts (i.e. a corpus), by calling the \code{corpus()} instructor from the
#' \pkg{quanteda} package, perform a set of checks and prepare it for further analysis.
#'
#' @param corpusdf a \code{data.table} (or \code{data.frame}) with as named columns and \emph{in this order}: a document
#' \code{id} column, a \code{date} column, a \code{text} column (i.e. the columns where all texts to analyze reside), and a
#' series of feature columns of type \code{numeric}, with values pointing to the applicability of a particular feature to
#' a particular text. The latter columns are often binary (1 means the feature is applicable to the document in the same row)
#' or as a percentage to specify the degree of connectedness of a feature to a document. Features could be for example topics
#' (e.g. economic, political or legal), but also article sources (e.g. online or printed press), amongst many more
#' possibilities. Feature column names should be unique. Provide the \code{date} column as \code{"yyyy-mm-dd"}.
#' @param do.clean a \code{logical}, if \code{TRUE} all texts undergo a cleaning routine to eliminate common textual
#' garbage. This includes ... . TO DO.
#'
#' @return A \code{sentocorpus} object, as a \code{quanteda} classed list keeping the elements \code{documents},
#' \code{metadata} and\code{settings}, while adding the elements \code{features} (a \code{character} vector of
#' all features part of the corpus.), and \code{nDocs} (the number of documents in the corpus).
#'
#' @seealso \code{\link[quanteda]{corpus}}
#'
#' @examples
#' # corpus construction
#' data("useconomynews")
#' corpus <- sento_corpus(corpusdf = useconomynews)
#'
#' # take a random subset using a quanteda's package function
#' corpusSmall <- quanteda::corpus_sample(corpus, size = 500)
#'
#' @export
sento_corpus <- function(corpusdf, do.clean = FALSE) {

  # check for presence of id, date and text columns
  nonfeatures <- c("id", "date", "text")
  cols <- colnames(corpusdf)
  if (!all(nonfeatures %in% cols) | (cols[1] != "id" | cols[2] != "date" | cols[3] != "text"))
    stop("The input data.frame should have its first columns named 'id', 'date' and 'text', in this order.")
  # check for type of text column
  if (!is.character(corpusdf[["text"]])) stop("The 'text' column should be of type character.")
  # check for date format
  dates <- as.Date(corpusdf$date, format = "%Y-%m-%d")
  if (all(is.na(dates))) stop("Dates are not in appropriate format. Should be 'yyyy-mm-dd'.")
  else corpusdf$date <- dates
  # check for duplicated feature names, if no issues add to output list
  features <- cols[!(cols %in% nonfeatures)]
  if (sum(duplicated(features)) > 0) {
    duplics <- unique(features[duplicated(features)])
    stop(paste0("Names of features are not unique. Following names occur at least twice: ",
                paste0(duplics, collapse = ", "), "."))
  }
  corpusdf <- as.data.table(corpusdf)
  isNumeric <- sapply(features, function(x) return(is.numeric(corpusdf[[x]])))
  if (any(!isNumeric)) {
    toDrop <- names(isNumeric)[isNumeric == FALSE]
    corpusdf <- corpusdf[, (toDrop) := NULL]
    warning(paste0("Following feature columns were dropped as they are not of type numeric: ", toDrop))
  }
  if (do.clean) corpusdf <- clean(corpusdf)

  # construct corpus as a quanteda corpus
  c <- quanteda::corpus(x = corpusdf, docid_field = "id", text_field = "text",
                        metacorpus = list(info = "This is a sentocorpus object directly based on the quanteda corpus."))
  c$tokens <- NULL
  c$features <- names(which(isNumeric))

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
#' @description Adds new named feature columns to provided \code{sentocorpus} object.
#'
#' @param sentocorpus a \code{sentocorpus} object.
#' @param featuresdf a sensibly named \code{data.frame} with as columns the new features of type \code{numeric} to add
#' to the \code{sentocorpus} inputted. If the number of rows in \code{featuresdf} is not equal to the number of documents in
#' \code{sentocorpus}, recycling will occur.
#'
#' @return An updated \code{sentocorpus} object.
#'
#' @examples
#' # construct a corpus and add random features to it
#' data("useconomynews")
#' corpus <- sento_corpus(corpusdf = useconomynews)
#' corpus <- add_features(corpus, featuresdf = data.frame(random = runif(quanteda::ndoc(corpus))))
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
  sentocorpus$features <- c(sentocorpus$features, names(toAdd)) # update features vector
  return(sentocorpus)
}

