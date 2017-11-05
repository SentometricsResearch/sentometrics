
#' Create a sentocorpus object
#'
#' @author Samuel Borms
#'
#' @description Formalizes a collection of texts into a well-defined corpus object, by calling, amongst others, the
#' \code{\link[quanteda]{corpus}} function from the \pkg{quanteda} package. This package is a (very) fast text mining package;
#' for more info, see \href{http://quanteda.io/index.html}{quanteda}. Their formal corpus structure is required for better
#' memory management, corpus manipulation and sentiment calculation. This function mainly performs a set of checks on the
#' input data and prepares the corpus for further sentiment analysis.
#'
#' @details A \code{sentocorpus} object can be regarded as a specialized instance of a \pkg{quanteda} corpus. In theory, all
#' \pkg{quanteda} functions applicable to its corpus object can also be applied to a \code{sentocorpus} object. However,
#' changing a given \code{sentocorpus} object too drastically using some of \pkg{quanteda}'s functions might alter the very
#' structure the corpus is meant to have (as defined in the \code{corpusdf} argument) to be able to be used as an input
#' in other functions of the \pkg{sentometrics} package. There are functions, including \code{\link[quanteda]{corpus_sample}}
#' or \code{\link[quanteda]{corpus_subset}}, that do not change the actual corpus structure and may come in handy. To add
#' additional features, we recommend to use \code{\link{add_features}}.
#'
#' @param corpusdf a \code{data.frame} with as named columns and \emph{in this order}: a document \code{"id"} column, a
#' \code{"date"} column, a \code{"text"} column (i.e. the columns where all texts to analyze reside), and a series of feature
#' columns of type \code{numeric}, with values pointing to the applicability of a particular feature to a particular text. The
#' latter columns are often binary (1 means the feature is applicable to the document in the same row) or as a percentage to
#' specify the degree of connectedness of a feature to a document. Features could be topics (e.g. legal, political or economic),
#' but also article sources (e.g. online or printed press), amongst many more options. If you have no knowledge about features
#' or no particular features are of interest to your analysis, provide no feature columns. In that case, the corpus
#' constructor automatically adds an additional feature column named \code{"dummy"}. Provide the \code{date} column as
#' \code{"yyyy-mm-dd"}. The \code{id} column should be in \code{character} mode. All spaces in the names of the features are
#' replaced by underscores.
#' @param do.clean a \code{logical}, if \code{TRUE} all texts undergo a cleaning routine to eliminate common textual garbage.
#' This includes a brute force replacement of HTML tags and non-alphanumeric characters by an empty string.
#'
#' @return A \code{sentocorpus} object, derived from a \pkg{quanteda} corpus classed \code{list} keeping the elements
#' \code{"documents"}, \code{"metadata"} and \code{"settings"}. The first element incorporates the corpus represented as
#' a \code{data.frame}.
#'
#' @seealso \code{\link[quanteda]{corpus}}
#'
#' @examples
#' data("usnews")
#'
#' # corpus construction
#' corpus <- sento_corpus(corpusdf = usnews)
#'
#' # take a random subset making use of quanteda
#' corpusSmall <- quanteda::corpus_sample(corpus, size = 500)
#'
#' # deleting a feature
#' quanteda::docvars(corpus, field = "wapo") <- NULL
#'
#' # corpus creation when no features are present
#' corpusDummy <- sento_corpus(corpusdf = usnews[, 1:3])
#'
#' @export
sento_corpus <- function(corpusdf, do.clean = FALSE) {

  corpusdf <- as.data.frame(corpusdf)
  # check for presence of id, date and text columns
  nonfeatures <- c("id", "date", "text")
  cols <- stringi::stri_replace_all(colnames(corpusdf), "_", regex = " ")
  colnames(corpusdf) <- cols
  if (!all(nonfeatures %in% cols) | (cols[1] != "id" | cols[2] != "date" | cols[3] != "text"))
    stop("The input data.frame should have its first columns named 'id', 'date' and 'text', in this order.")
  # check for type of text column
  if (!is.character(corpusdf[["text"]])) stop("The 'text' column should be of type character.")
  # check for date format
  dates <- as.Date(corpusdf$date, format = "%Y-%m-%d")
  if (any(is.na(dates))) stop("Some dates are not in appropriate format. Should be 'yyyy-mm-dd'.")
  else corpusdf$date <- dates
  # check for duplicated feature names, if no issues add to output list
  features <- cols[!(cols %in% nonfeatures)]
  if (length(features) == 0) {
    corpusdf[["dummy"]] <- 1
    warning("No features detected. A 'dummy' feature valued at 1 throughout is added.")
  } else {
    if (sum(duplicated(features)) > 0) {
      duplics <- unique(features[duplicated(features)])
      stop(paste0("Names of feature columns are not unique. Following names occur at least twice: ",
                  paste0(duplics, collapse = ", "), "."))
    }
    isNumeric <- sapply(features, function(x) return(is.numeric(corpusdf[[x]])))
    if (any(!isNumeric)) {
      toDrop <- names(isNumeric)[isNumeric == FALSE]
      corpusdf[, toDrop] <- NULL
      warning(paste0("Following feature columns were dropped as they are not numeric: ", paste0(toDrop, collapse = ", "), "."))
      if (length(toDrop) == length(isNumeric))
        stop("No remaining feature columns. Please add uniquely named feature columns of type numeric.")
    }
  }
  if (do.clean) corpusdf <- clean(corpusdf)

  # construct corpus as a quanteda corpus
  corp <- quanteda::corpus(x = corpusdf, docid_field = "id", text_field = "text",
                           metacorpus = list(info = "This is a sentocorpus object directly based on the quanteda corpus."))
  corp$tokens <- NULL

  class(corp) <- c("sentocorpus", class(corp))

  return(corp)
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
#' @description Adds new feature columns, either user-supplied or based on a simple keyword(s) search, to provided
#' \code{sentocorpus} object.
#'
#' @details If a provided feature name is already part of the corpus, it will be replaced. The \code{featuresdf} and
#' \code{keywords} arguments can be provided at the same time, or only one of them, leaving the other at \code{NULL}.
#'
#' @param sentocorpus a \code{sentocorpus} object.
#' @param featuresdf a named \code{data.frame} with as columns the new features of type \code{numeric} to add to the
#' \code{sentocorpus} inputted. If the number of rows in \code{featuresdf} is not equal to the number of documents
#' in \code{sentocorpus}, recycling will occur.
#' @param keywords a named \code{list}. For every element, a new feature column is added with a value of 1 for the texts
#' in which the keyword(s) appear(s), and 0 if not. If no texts match a keyword, no column is added. The names are used
#' as the names of the new features.
#'
#' @return An updated \code{sentocorpus} object.
#'
#' @examples
#' data("usnews")
#'
#' # construct a corpus and add random features to it
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpus1 <- add_features(corpus,
#'                         featuresdf = data.frame(random = runif(quanteda::ndoc(corpus))))
#' corpus2 <- add_features(corpus,
#'                         keywords = list(pres = "president", war = "war"))
#'
#' @export
add_features <- function(sentocorpus, featuresdf = NULL, keywords = NULL) {
  check_class(sentocorpus, "sentocorpus")
  if (!is.null(featuresdf)) {
    features <- stringi::stri_replace_all(colnames(featuresdf), "_", regex = " ")
    isNumeric <- sapply(features, function(x) return(is.numeric(featuresdf[[x]])))
    toAdd <- which(isNumeric)
    for (i in toAdd) {
      quanteda::docvars(sentocorpus, field = features[i]) <- featuresdf[[i]]
    }
    if (length(toAdd) != length(isNumeric))
      warning(paste0("Following columns were not added as they are not of type numeric: ", names(which(!isNumeric))))
  }
  if (!is.null(keywords)) {
    if ("" %in% names(keywords) || is.null(names(keywords)))
      stop("Please provide proper names as part of the 'keywords' argument.")
    textsAll <- quanteda::texts(sentocorpus)
    for (j in seq_along(keywords)) {
      kwName <- stringi::stri_replace_all(names(keywords)[j], "_", regex = " ")
      if (length(keywords[[j]]) == 1) {
        regex <- paste0("\\b", keywords[[j]], "\\b")
      } else {
        regex <- paste0("\\b", paste0(keywords[[j]], collapse = "\\b|\\b"), "\\b")
      }
      occurrences <- as.numeric(stringi::stri_detect(textsAll, regex = regex))
      if (sum(occurrences) == 0) warning(paste0("Feature ", kwName, " is not added as it occurs in none of the documents."))
      else quanteda::docvars(sentocorpus, field = kwName) <- occurrences
    }
  }
  return(sentocorpus)
}

