
#' Create a sentocorpus object
#'
#' @author Samuel Borms
#'
#' @description Formalizes a collection of texts into a well-defined corpus object, by mainly calling the
#' \code{\link[quanteda]{corpus}} function from the \pkg{quanteda} package. This package provides a fast text mining
#' infrastructure; for more info, see \href{http://quanteda.io/index.html}{quanteda}. Their formal corpus structure is
#' required for better memory management, corpus manipulation, and sentiment calculation. This function mainly performs
#' a set of checks on the input data and prepares the corpus for further sentiment analysis.
#'
#' @details A \code{sentocorpus} object is a specialized instance of a \pkg{quanteda} corpus. In theory, all
#' \pkg{quanteda} functions applicable to its corpus object can also be applied to a \code{sentocorpus} object. However,
#' changing a given \code{sentocorpus} object too drastically using some of \pkg{quanteda}'s functions might alter the very
#' structure the corpus is meant to have (as defined in the \code{corpusdf} argument) to be able to be used as an input
#' in other functions of the \pkg{sentometrics} package. There are functions, including \code{\link[quanteda]{corpus_sample}}
#' or \code{\link[quanteda]{corpus_subset}}, that do not change the actual corpus structure and may come in handy. To add
#' additional features, use \code{\link{add_features}}. Binary features are useful as a mechanism to select the
#' texts which have to be integrated in the respective feature-based sentiment measure(s); beware however, the option
#' \code{do.ignoreZeros} should be set to \code{TRUE} and the within-document aggregation still considers the entire corpus
#' in case of \code{"tf-idf"} (see \code{\link{ctr_agg}}). Because of this (implicit) selection that can be performed, having
#' complementary features (e.g., \code{"economy"} and \code{"noneconomy"}) makes sense.
#'
#' @param corpusdf a \code{data.frame} (or a \code{data.table}, or a \code{tbl}) with as named columns: a document \code{"id"}
#' column (in \code{character} mode), a \code{"date"} column (as \code{"yyyy-mm-dd"}), a \code{"texts"} column (in
#' \code{character} mode), and a series of feature columns of type \code{numeric}, with values pointing to the applicability of
#' a particular feature to a particular text. The latter columns can be binary (\code{1} means the feature is applicable to
#' the document in the same row) or a value between 0 and 1 to specify the degree of connectedness of a feature to a
#' document. Features could be topics (e.g., legal, political, or economic), article sources (e.g., online or printed press),
#' amongst many more options. In the case of no feature column provided, the corpus constructor automatically adds a feature
#' named \code{"dummy"}. All spaces in the names of the features are replaced by underscores. If the feature columns have
#' values not between 0 and 1, they will be rescaled column-wise and a warning will be issued.
#' @param do.clean a \code{logical}, if \code{TRUE} all texts undergo a cleaning routine to eliminate common textual garbage.
#' This includes a brute force replacement of HTML tags and non-alphanumeric characters by an empty string. To use with care
#' if the text is meant to have non-alphanumeric characters! Preferably, cleaning is done outside of this function call.
#'
#' @return A \code{sentocorpus} object, derived from a \pkg{quanteda} corpus classed \code{list} with the elements
#' \code{"documents"}, \code{"metadata"}, and \code{"settings"} kept. The first element incorporates the corpus
#' represented as a \code{data.frame}.
#'
#' @seealso \code{\link[quanteda]{corpus}}, \code{\link{add_features}}
#'
#' @examples
#' data("usnews", package = "sentometrics")
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

  # check for presence of id, date and texts columns
  nonfeatures <- c("id", "date", "texts")
  cols <- stringi::stri_replace_all(colnames(corpusdf), "_", regex = " ")
  colnames(corpusdf) <- cols
  if (!all(nonfeatures %in% cols))
    stop("The input data.frame should have at least three columns named 'id', 'date' and 'texts'.")
  # check for type of texts column
  if (!is.character(corpusdf[["texts"]])) stop("The 'texts' column should be of type character.")
  # check for date format
  dates <- as.Date(corpusdf$date, format = "%Y-%m-%d")
  if (any(is.na(dates))) stop("Some dates are not in appropriate format. Should be 'yyyy-mm-dd'.")
  else corpusdf$date <- dates
  features <- cols[!(cols %in% nonfeatures)]
  if (!is_names_correct(features))
    stop("At least one feature's name contains '-'. Please provide proper names.")
  corpusdf <- corpusdf[, c("id", "date", "texts", features)]
  info <- "This is a sentocorpus object derived from a quanteda corpus object."

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
      toDrop <- names(which(!isNumeric))
      corpusdf[, toDrop] <- NULL
      warning(paste0("Following feature columns were dropped as they are not numeric: ", paste0(toDrop, collapse = ", "), "."))
      if (length(toDrop) == length(isNumeric)) {
        corpusdf[["dummy"]] <- 1
        warning("No remaining feature columns. A 'dummy' feature valued at 1 throughout is added.")
        if (do.clean) corpusdf <- clean_texts(corpusdf)
        corp <- quanteda::corpus(x = corpusdf, docid_field = "id", text_field = "texts", metacorpus = list(info = info))
        corp$tokens <- NULL
        class(corp) <- c("sentocorpus", class(corp))
        return(corp)
      }
    }
    featuresKept <- names(which(isNumeric))
    mins <- sapply(featuresKept, function(f) min(corpusdf[[f]], na.rm = TRUE)) >= 0
    maxs <- sapply(featuresKept, function(f) max(corpusdf[[f]], na.rm = TRUE)) <= 1
    isBounded <- sapply(1:length(featuresKept), function(j) return(all(c(mins[j], maxs[j]))))
    if (any(!isBounded)) {
      toScale <- featuresKept[!isBounded]
      for (col in toScale) {
        vals <- corpusdf[, col]
        corpusdf[, col] <- (vals - min(vals)) / (max(vals) - min(vals))
      }
      warning(paste0("Following feature columns have been rescaled: ", paste0(toScale, collapse = ", "), "."))
    }
  }

  if (do.clean) corpusdf <- clean_texts(corpusdf)
  corp <- quanteda::corpus(x = corpusdf, docid_field = "id", text_field = "texts", metacorpus = list(info = info))
  corp$tokens <- NULL
  class(corp) <- c("sentocorpus", class(corp))

  return(corp)
}

clean_texts <- function(corpusdf) {
  corpusdf$texts <- stringi::stri_replace_all(corpusdf$texts, replacement = "", regex = "<.*?>") # html tags
  corpusdf$texts <- stringi::stri_replace_all(corpusdf$texts, replacement = "", regex = '[\\"]')
  corpusdf$texts <- stringi::stri_replace_all(corpusdf$texts, replacement = "", regex = "[^-a-zA-Z0-9,&. ]")
  return(corpusdf)
}

#' Convert a quanteda corpus object into a sentocorpus object
#'
#' @author Samuel Borms
#'
#' @description Converts a \code{\link[quanteda]{corpus}} object into a \code{sentocorpus} object, by adding
#' user-supplied dates and doing proper rearranging.
#'
#' @param corpus a quanteda \code{\link[quanteda]{corpus}} object.
#' @param dates a sequence of dates as \code{"yyyy-mm-dd"}, of the same length as the number of documents
#' in the input \code{corpus}.
#' @param do.clean see \code{do.clean} argument from the \code{\link{sento_corpus}} function.
#'
#' @return A \code{sentocorpus} object, as returned by the \code{\link{sento_corpus}} function.
#'
#' @seealso \code{\link[quanteda]{corpus}}, \code{\link{sento_corpus}}
#'
#' @examples
#' data("usnews", package = "sentometrics")
#'
#' # reshuffle usnews data.frame
#' dates <- usnews$date
#' usnews$id <- usnews$date <- NULL
#' usnews$wrong <- "notNumeric"
#' colnames(usnews)[1] <- "myTexts"
#'
#' # set up quanteda corpus object
#' corpusQ <- quanteda::corpus(usnews, text_field = "myTexts")
#'
#' # corpus conversion
#' corpusS <- to_sentocorpus(corpusQ, dates = dates)
#'
#' @export
to_sentocorpus <- function(corpus, dates, do.clean = FALSE) {
  if (length(dates) != quanteda::ndoc(corpus))
    stop("The number of dates in 'dates' should be equal to the number of documents in 'corpus'.")
  corpusdf <- data.table::as.data.table(corpus$documents)
  corpusdf[, id := quanteda::docnames(corpus)]
  corpusdf[, date := dates]
  setcolorder(corpusdf, c("id", "date", "texts", setdiff(names(corpusdf), c("id", "date", "texts"))))
  return(sento_corpus(corpusdf, do.clean))
}

#' Add feature columns to a (sento)corpus object
#'
#' @author Samuel Borms
#'
#' @description Adds new feature columns, either user-supplied or based on keyword(s)/regex pattern search, to
#' a provided \code{sentocorpus} or a \pkg{quanteda} \code{\link[quanteda]{corpus}} object.
#'
#' @details If a provided feature name is already part of the corpus, it will be replaced. The \code{featuresdf} and
#' \code{keywords} arguments can be provided at the same time, or only one of them, leaving the other at \code{NULL}. We use
#' the \pkg{stringi} package for searching the keywords. The \code{do.regex} argument points to the corresponding elements
#' in \code{keywords}. For \code{FALSE}, we transform the keywords into a simple regex expression, involving \code{"\\b"} for
#' exact word boundary matching and (if multiple keywords) \code{|} as OR operator. The elements associated to \code{TRUE} do
#' not undergo this transformation, and are evaluated as given, if the corresponding keywords vector consists of only one
#' expression. For a large corpus and/or complex regex patterns, this function may require some patience. Scaling between 0
#' and 1 is performed via min-max normalization, per column.
#'
#' @param corpus a \code{sentocorpus} object created with \code{\link{sento_corpus}}, or a \pkg{quanteda}
#' \code{\link[quanteda]{corpus}} object.
#' @param featuresdf a named \code{data.frame} of type \code{numeric} where each columns is a new feature to be added to the
#' inputted \code{corpus} object. If the number of rows in \code{featuresdf} is not equal to the number of documents
#' in \code{corpus}, recycling will occur. The numeric values should be between 0 and 1 (included).
#' @param keywords a named \code{list}. For every element, a new feature column is added with a value of 1 for the texts
#' in which (at least one of) the keyword(s) appear(s), and 0 if not (for \code{do.binary = TRUE}), or with as value the
#' normalized number of times the keyword(s) occur(s) in the text (for \code{do.binary = FALSE}). If no texts match a
#' keyword, no column is added. The \code{list} names are used as the names of the new features. For more complex searching,
#' instead of keywords, one can also directly use a single regex expression to define a new feature (cf. the details section).
#' @param do.binary a \code{logical}, cf. argument \code{keywords}. If \code{do.binary = FALSE}, the counts are normalized
#' between 0 and 1,
#' @param do.regex a \code{logical} vector equal in length to the number of elements in the \code{keywords} argument
#' \code{list}, or a single value if it applies to all. It should be set to \code{TRUE} at those positions where a single
#' regex expression is used to identify the particular feature.
#'
#' @return An updated \code{corpus} object.
#'
#' @examples
#' data("usnews", package = "sentometrics")
#'
#' # construct a corpus and add (a) feature(s) to it
#' corpus <- sento_corpus(corpusdf = usnews)
#' corpus1 <- add_features(corpus,
#'                         featuresdf = data.frame(random = runif(quanteda::ndoc(corpus))))
#' corpus2 <- add_features(corpus,
#'                         keywords = list(pres = "president", war = "war"))
#' corpus3 <- add_features(corpus,
#'                         keywords = list(pres = c("Obama", "US president")),
#'                         do.binary = FALSE)
#' corpus4 <- add_features(corpus,
#'                         featuresdf = data.frame(all = 1),
#'                         keywords = list(pres1 = "Obama|US [p|P]resident",
#'                                         pres2 = "\\bObama\\b|\\bUS president\\b",
#'                                         war = "war"),
#'                         do.regex = c(TRUE, TRUE, FALSE))
#'
#' sum(corpus3$documents$pres) == sum(corpus4$documents$pres2) # TRUE
#'
#' # adding a complementary feature
#' nonpres <- data.frame(nonpres = as.numeric(!quanteda::docvars(corpus2)[["pres"]]))
#' corpus2 <- add_features(corpus2,
#'                         featuresdf = nonpres)
#'
#' @export
add_features <- function(corpus, featuresdf = NULL, keywords = NULL, do.binary = TRUE, do.regex = FALSE) {
  check_class(corpus, "corpus")

  if (!is.null(featuresdf)) {
    stopifnot(is.data.frame(featuresdf))
    if (!is_names_correct(colnames(featuresdf)))
      stop("At least one feature's name in 'featuresdf' contains '-'. Please provide proper names.")
    features <- stringi::stri_replace_all(colnames(featuresdf), "_", regex = " ")
    isNumeric <- sapply(featuresdf, is.numeric)
    mins <- sapply(featuresdf, min, na.rm = TRUE) >= 0
    maxs <- sapply(featuresdf, max, na.rm = TRUE) <= 1
    check <- sapply(1:length(features), function(j) return(all(c(isNumeric[j], mins[j], maxs[j]))))
    toAdd <- which(check) # logical vector
    for (i in toAdd) {
      quanteda::docvars(corpus, field = features[i]) <- featuresdf[[i]]
    }
    if (length(toAdd) != length(check))
        warning(paste0("Following columns are not added as they are not of type numeric or have values outside [0, 1]: ",
                       paste0(colnames(featuresdf)[which(!check)], collapse = ", ")))
  }
  if (!is.null(keywords)) {
    stopifnot(is.logical(do.binary) && is.logical(do.regex))
    if ("" %in% names(keywords) || is.null(names(keywords)) || !inherits(keywords, "list"))
      stop("Please provide a list with proper names as part of the 'keywords' argument.")
    if (!is_names_correct(names(keywords)))
      stop("At least one feature's name in 'keywords' contains '-'. Please provide proper names.")
    textsAll <- quanteda::texts(corpus)
    if (do.binary == TRUE) fct <- stringi::stri_detect
    else fct <- stringi::stri_count
    N <- length(keywords)
    if (length(do.regex) == 1 && N > 1) do.regex <- rep(do.regex, N)
    for (j in 1:N) {
      kwName <- stringi::stri_replace_all(names(keywords)[j], "_", regex = " ")
      if (do.regex[j] == TRUE) {
        if (length(keywords[[j]]) != 1)
          stop("Every corresponding element in 'keywords' for which 'do.regex' is TRUE should be of length 1.")
        else regex <- keywords[[j]]
      } else {
        if (length(keywords[[j]]) == 1) {
          regex <- paste0("\\b", keywords[[j]], "\\b")
        } else {
          regex <- paste0("\\b", paste0(keywords[[j]], collapse = "\\b|\\b"), "\\b")
        }
      }
      occurrences <- as.numeric(fct(textsAll, regex = regex))
      if (sum(occurrences) == 0)
        warning(paste0("Feature ", kwName, " is not added as it occurs in none of the documents."))
      else {
        occurrences <- (occurrences - min(occurrences)) / (max(occurrences) - min(occurrences)) # normalize to [0, 1]
        quanteda::docvars(corpus, field = kwName) <- occurrences
      }
    }
  }
  return(corpus)
}

