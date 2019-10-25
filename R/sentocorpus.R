
#' Create a sento_corpus object
#'
#' @author Samuel Borms
#'
#' @description Formalizes a collection of texts into a \code{sento_corpus} object derived from the \pkg{quanteda}
#' \code{\link[quanteda]{corpus}} object. The \pkg{quanteda} package provides a robust text mining infrastructure
#' (see \href{http://quanteda.io/index.html}{quanteda}), including a handy corpus manipulation toolset. This function
#' performs a set of checks on the input data and prepares the corpus for further analysis by structurally
#' integrating a date dimension and numeric metadata features.
#'
#' @details A \code{sento_corpus} object is a specialized instance of a \pkg{quanteda} \code{\link[quanteda]{corpus}}. Any
#' \pkg{quanteda} function applicable to its \code{\link[quanteda]{corpus}} object can also be applied to a \code{sento_corpus}
#' object. However, changing a given \code{sento_corpus} object too drastically using some of \pkg{quanteda}'s functions might
#' alter the very structure the corpus is meant to have (as defined in the \code{corpusdf} argument) to be able to be used as
#' an input in other functions of the \pkg{sentometrics} package. There are functions, including
#' \code{\link[quanteda]{corpus_sample}} or \code{\link[quanteda]{corpus_subset}}, that do not change the actual corpus
#' structure and may come in handy. To add additional features, use \code{\link{add_features}}. Binary features are useful as
#' a mechanism to select the texts which have to be integrated in the respective feature-based sentiment measure(s), but
#' applies only when \code{do.ignoreZeros = TRUE}. Because of this (implicit) selection that can be performed, having
#' complementary features (e.g., \code{"economy"} and \code{"noneconomy"}) makes sense.
#'
#' It is also possible to add one non-numerical feature, that is, \code{"language"}, to designate the language
#' of the corpus texts. When this feature is provided on corpus-level, a list of lexicons for different
#' languages is expected in the function \code{compute_sentiment}.
#'
#' @param corpusdf a \code{data.frame} (or a \code{data.table}, or a \code{tbl}) with as named columns: a document \code{"id"}
#' column (in \code{character} mode), a \code{"date"} column (as \code{"yyyy-mm-dd"}), a \code{"texts"} column
#' (in \code{character} mode), an optional \code{"language"} column (in \code{character} mode), and a series of
#' feature columns of type \code{numeric}, with values between 0 and 1 to specify the degree of connectedness of
#' a feature to a document. Features could be for instance topics (e.g., legal or economic) or article sources (e.g., online or
#' print). When no feature column is provided, a feature named \code{"dummyFeature"}
#' is added. All spaces in the names of the features are replaced by \code{'_'}. Feature columns with values not
#' between 0 and 1 are rescaled column-wise.
#' @param do.clean a \code{logical}, if \code{TRUE} all texts undergo a cleaning routine to eliminate common textual garbage.
#' This includes a brute force replacement of HTML tags and non-alphanumeric characters by an empty string. To use with care
#' if the text is meant to have non-alphanumeric characters! Preferably, cleaning is done outside of this function call.
#'
#' @return A \code{sento_corpus} object, derived from a \pkg{quanteda} \code{\link[quanteda]{corpus}} classed \code{list}
#' with elements \code{"documents"}, \code{"metadata"}, and \code{"settings"} kept. The first element incorporates
#' the corpus represented as a \code{data.frame}.
#'
#' @seealso \code{\link[quanteda]{corpus}}, \code{\link{add_features}}
#'
#' @examples
#' data("usnews", package = "sentometrics")
#'
#' # corpus construction
#' corp <- sento_corpus(corpusdf = usnews)
#'
#' # take a random subset making use of quanteda
#' corpusSmall <- quanteda::corpus_sample(corp, size = 500)
#'
#' # deleting a feature
#' quanteda::docvars(corp, field = "wapo") <- NULL
#'
#' # deleting all features results in the addition of a dummy feature
#' quanteda::docvars(corp, field = c("economy", "noneconomy", "wsj")) <- NULL
#'
#' \dontrun{
#' # to add or replace features, use the add_features() function...
#' quanteda::docvars(corp, field = c("wsj", "new")) <- 1}
#'
#' # corpus creation when no features are present
#' corpusDummy <- sento_corpus(corpusdf = usnews[, 1:3])
#'
#' # corpus creation with a qualitative language feature
#' usnews[["language"]] <- "en"
#' usnews[["language"]][c(200:400)] <- "nl"
#' corpusLang <- sento_corpus(corpusdf = usnews)
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

  # check if language is provided
  if ("language" %in% cols) {
    nonfeatures <- c(nonfeatures, "language")
    # if (!all(sapply(corpusdf$language, is_iso_language)))
    #   stop("Not all text contain ISO 639 code. Check ISOcodes::ISO_639_2 for list of available options.")
  }

  features <- cols[!(cols %in% nonfeatures)]
  if (!is_names_correct(features))
    stop("At least one feature's name contains '-'. Please provide proper names.")
  corpusdf <- corpusdf[, c(nonfeatures, features)]

  info <- "This is a sento_corpus object derived from a quanteda corpus object."

  if (length(features) == 0) {
    corpusdf[["dummyFeature"]] <- 1
    warning("No features detected. A 'dummyFeature' feature valued at 1 throughout is added.")
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
      warning(paste0("Following feature columns are dropped as they are not numeric: ", paste0(toDrop, collapse = ", "), "."))
      if (length(toDrop) == length(isNumeric)) {
        corpusdf[["dummyFeature"]] <- 1
        warning("No remaining feature columns. A 'dummyFeature' feature valued at 1 throughout is added.")
        if (do.clean) corpusdf <- clean_texts(corpusdf)
        corp <- quanteda::corpus(x = corpusdf, docid_field = "id", text_field = "texts", metacorpus = list(info = info))
        class(corp) <- c("sento_corpus", class(corp))
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
  class(corp) <- c("sento_corpus", class(corp))
  setorder(corp$documents, "date", na.last=FALSE)
  return(corp)
}

clean_texts <- function(corpusdf) {
  corpusdf$texts <- stringi::stri_replace_all(corpusdf$texts, replacement = "", regex = "<.*?>") # html tags
  corpusdf$texts <- stringi::stri_replace_all(corpusdf$texts, replacement = "", regex = '[\\"]')
  corpusdf$texts <- stringi::stri_replace_all(corpusdf$texts, replacement = "", regex = "[^-a-zA-Z0-9,&. ]")
  return(corpusdf)
}

#' Add feature columns to a (sento_)corpus object
#'
#' @author Samuel Borms
#'
#' @description Adds new feature columns, either user-supplied or based on keyword(s)/regex pattern search, to
#' a provided \code{sento_corpus} or a \pkg{quanteda} \code{\link[quanteda]{corpus}} object.
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
#' @param corpus a \code{sento_corpus} object created with \code{\link{sento_corpus}}, or a \pkg{quanteda}
#' \code{\link[quanteda]{corpus}} object.
#' @param featuresdf a named \code{data.frame} of type \code{numeric} where each columns is a new feature to be added to the
#' inputted \code{corpus} object. If the number of rows in \code{featuresdf} is not equal to the number of documents
#' in \code{corpus}, recycling will occur. The numeric values should be between 0 and 1 (included).
#' @param keywords a named \code{list}. For every element, a new feature column is added with a value of 1 for the texts
#' in which (at least one of) the keyword(s) appear(s), and 0 if not (for \code{do.binary = TRUE}), or with as value the
#' normalized number of times the keyword(s) occur(s) in the text (for \code{do.binary = FALSE}). If no texts match a
#' keyword, no column is added. The \code{list} names are used as the names of the new features. For more complex searching,
#' instead of just keywords, one can also directly use a single regex expression to define a new feature (see the details section).
#' @param do.binary a \code{logical}, if \code{do.binary = FALSE}, the number of occurrences are normalized
#' between 0 and 1 (see argument \code{keywords}).
#' @param do.regex a \code{logical} vector equal in length to the number of elements in the \code{keywords} argument
#' \code{list}, or a single value if it applies to all. It should be set to \code{TRUE} at those positions where a single
#' regex expression is used to identify the particular feature.
#'
#' @return An updated \code{corpus} object.
#'
#' @examples
#' set.seed(505)
#'
#' # construct a corpus and add (a) feature(s) to it
#' corpus <- quanteda::corpus_sample(
#'   sento_corpus(corpusdf = sentometrics::usnews), 500
#' )
#' corpus1 <- add_features(corpus,
#'                         featuresdf = data.frame(random = runif(quanteda::ndoc(corpus))))
#' corpus2 <- add_features(corpus,
#'                         keywords = list(pres = "president", war = "war"),
#'                         do.binary = FALSE)
#' corpus3 <- add_features(corpus,
#'                         keywords = list(pres = c("Obama", "US president")))
#' corpus4 <- add_features(corpus,
#'                         featuresdf = data.frame(all = 1),
#'                         keywords = list(pres1 = "Obama|US [p|P]resident",
#'                                         pres2 = "\\bObama\\b|\\bUS president\\b",
#'                                         war = "war"),
#'                         do.regex = c(TRUE, TRUE, FALSE))
#'
#' sum(quanteda::docvars(corpus3, "pres")) ==
#'   sum(quanteda::docvars(corpus4, "pres2")) # TRUE
#'
#' # adding a complementary feature
#' nonpres <- data.frame(nonpres = as.numeric(!quanteda::docvars(corpus3, "pres")))
#' corpus3 <- add_features(corpus3, featuresdf = nonpres)
#'
#' @export
add_features <- function(corpus, featuresdf = NULL, keywords = NULL, do.binary = TRUE, do.regex = FALSE) {
  check_class(corpus, "corpus")

  classIn <- class(corpus)
  class(corpus) <- c("corpus", "list") # needed to avoid use of `docvars<-.sento_corpus`() function

  if (!is.null(featuresdf)) {
    stopifnot(is.data.frame(featuresdf))
    if (!is_names_correct(colnames(featuresdf)))
      stop("At least one feature's name in 'featuresdf' contains '-'. Please provide proper names.")
    features <- stringi::stri_replace_all(colnames(featuresdf), "_", regex = " ")
    isNumeric <- sapply(featuresdf, is.numeric)
    mins <- sapply(featuresdf, function(col) tryCatch(min(col, na.rm = TRUE) >= 0, error = function(e) FALSE))
    maxs <- sapply(featuresdf, function(col) tryCatch(max(col, na.rm = TRUE) <= 1, error = function(e) FALSE))
    check <- sapply(1:length(features), function(j) return(all(c(isNumeric[j], mins[j], maxs[j]))))
    toAdd <- which(check) # logical vector
    for (i in toAdd) {
      quanteda::docvars(corpus, field = features[i]) <- featuresdf[[i]]
    }
    if (length(toAdd) != length(check))
        warning(paste0("Following columns are not added as they are not of type numeric or have values outside [0, 1]: ",
                       paste0(colnames(featuresdf)[which(!check)], collapse = ", ")), call. = FALSE)
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
  class(corpus) <- classIn
  corpus
}

#' @importFrom quanteda docvars<-
#' @export
`docvars<-.sento_corpus` <- function(x, field, value) {
  if (!is.null(value)) {
    stop("To add or replace features in a sento_corpus object, use the add_features() function.", call. = FALSE)
  } else {
    # xNew <- NextMethod("docvars<-")
    xNew <- x
    class(xNew) <- c("corpus", "list")
    quanteda::docvars(xNew, field = field) <- value
    vars <- colnames(quanteda::docvars(xNew))
    if (!("date" %in% vars)) stop("You cannot remove the 'date' column from a sento_corpus object.", call. = FALSE)
    if (length(vars) == 1) {
      xNew <- add_features(xNew, featuresdf = data.frame(dummyFeature = 1))
      warning("No remaining features. A 'dummyFeature' feature valued at 1 throughout is added.", call. = FALSE)
    }
  }
  class(xNew) <- class(x)
  xNew
}

#' Summarize the sento_corpus object
#'
#' @author Jeroen Van Pelt, Samuel Borms, Andres Algaba
#'
#' @description Summarizes the \code{sento_corpus} object and returns insights about the evolution of
#' documents, features and tokens over time.
#'
#' @details This function summarizes the \code{sento_corpus} object by generating statistics about
#' documents, features and tokens over time. The insights can be narrowed down to a chosen set of metadata
#' features. The same tokenization as in the sentiment calculation in \code{\link{compute_sentiment}} is used.
#'
#' @param x is a \code{sento_corpus} object created with \code{\link{sento_corpus}}
#' @param by a single \code{character} vector to specify the frequency time interval over which the statistics
#' need to be calculated.
#' @param features a \code{character} vector that can be used to select a subset of the features to analyse.
#'
#' @return returns a \code{list} containing:
#' \item{stats}{a \code{data.table} with statistics about the number of documents, total, average, minimum and maximum
#' number of tokens and the number of texts per features for each date.}
#' \item{plots}{a \code{list} with three plots representing the above statistics.}
#'
#' @examples
#' data("usnews", package = "sentometrics")
#'
#' corpus <- sento_corpus(usnews)
#'
#' # summary of corpus by day
#' summary1 <- corpus_summarize(corpus)
#'
#' # summary of corpus by month for both journals
#' summary2 <- corpus_summarize(corpus, by = "month",
#'                              features = c("wsj", "wapo"))
#'
#' @import ggplot2
#' @export
corpus_summarize <- function(x, by = "day", features = NULL) {
  check_class(x, "sento_corpus")
  if (!(by %in% c("year", "month", "week", "day"))) {
    stop(paste0(by, " is no current 'by' option."))
  }

  # statistics
  dt <- data.table::data.table(
    quanteda::docvars(x),
    "nTokens" = as.numeric(sapply(tokenize_texts(quanteda::texts(x)), length))
  )

  if (!is.null(features)) {
    if (!all(features %in% colnames(dt)[-c(1, ncol(dt))]))
      stop("Not all features provided in the 'features' argument are present in the corpus.")
    dt <- dt[, c(features, "date", "nTokens"), with = FALSE]
  }

  if (by == "year") {
    dt[, "date" := as.Date(paste0(format(as.Date(date), "%Y"), "-01-01"), "%Y-%m-%d")]
  } else if (by == "month") {
    dt[,"date":= as.Date(paste0(format(as.Date(date), "%Y-%m"), "-01"), "%Y-%m-%d")]
  } else if (by == "week") {
    dt[, "date" := ISOweek::ISOweek2date(paste(ISOweek::ISOweek(date), 1, sep = "-"))]
  }

  tokensDT <- dt[, .(date, nTokens)][, .(sum(nTokens), mean(nTokens), min(nTokens), max(nTokens)), by = date]
  tokensDT <- setnames(tokensDT, c( "date", "totalTokens", "meanTokens", "minTokens", "maxTokens"))

  featuresDT <- dt[, !"nTokens"]
  freqTexts <- featuresDT[, .("documents" = as.numeric(.N)), by = .(date)]
  freqFeatures <- featuresDT[, lapply(.SD, function(x) sum(ifelse(x > 0, 1, 0), na.rm = TRUE)), by = date]
  freqAll <- merge(freqTexts, freqFeatures, by = "date")

  stats <- merge(tokensDT, freqAll, by = "date")
  data.table::setcolorder(stats, c("date", "documents"))

  # plots
  docPlot <- ggplot(data.table::melt(freqAll[, .(date, documents)], id = "date", all = TRUE)) +
    geom_line(aes(x = date, y = value, color = variable, group = variable)) +
    theme_bw() +
    scale_x_date(name = "Date", date_labels = "%m-%Y") +
    scale_y_continuous(name = paste0("Number of documents (by ", by, ")")) +
    plot_theme(legendPos = "none")

  freqAllMelt <- data.table::melt(freqAll[, !"documents"], id = "date", all = TRUE)
  legendPos <- ifelse(length(unique(freqAllMelt[["variable"]])) <= 12, "top", "none")
  featPlot <- ggplot(freqAllMelt) +
    geom_line(aes(x = date, y = value, color = variable, group = variable)) +
    theme_bw() +
    scale_x_date(name = "Date", date_labels = "%m-%Y") +
    scale_y_continuous(name = paste0("Feature count (by ", by, ")")) +
    plot_theme(legendPos)

  tokPlot <- ggplot(data.table::melt(tokensDT[, !"totalTokens"], id = "date", all = TRUE)) +
    geom_line(aes(x = date, y = value, color = variable, group = variable)) +
    theme_bw() +
    scale_x_date(name = "Date", date_labels = "%m-%Y") +
    scale_y_continuous(name = paste0("Token count (by ", by, ")")) +
    plot_theme(legendPos = "top")

  # output
  summary <- list(
    stats = stats,
    plots = list(doc_plot = docPlot, feature_plot = featPlot, token_plot = tokPlot)
  )
  return(summary)
}

#' @export
as.sento_corpus.corpus <- function(x, dates = NULL, do.clean = FALSE) {
  features <- quanteda::docvars(x)
  if (is.null(dates)) {
    if (!("date" %in% colnames(features))) {
      stop("There is no 'date' metadata element in the corpus and 'dates' = NULL.")
    }
  } else {
    if (length(dates) != quanteda::ndoc(x))
      stop("The number of dates in 'dates' should be equal to the number of documents.")
    features$date <- dates # avoids accidental duplication
  }
  dt <- data.table::data.table("id" = quanteda::docnames(x),
                               "texts" = quanteda::texts(x),
                               features) # includes date column
  data.table::setcolorder(dt, c("id", "date", "texts"))
  sento_corpus(dt, do.clean)
}

#' @export
as.sento_corpus.VCorpus <- function(x, dates = NULL, do.clean = FALSE) {
  texts <- unlist(lapply(x, "[", "content"))
  hasLocalDate <- all(sapply(x, function(d) !is.null(NLP::meta(d, "date", type = "local"))))
  to_sento_corpus_tm(x, dates, do.clean, texts, hasLocalDate)
}

#' @export
as.sento_corpus.SimpleCorpus <- function(x, dates = NULL, do.clean = FALSE) {
  texts <- as.character(as.list(x))
  to_sento_corpus_tm(x, dates, do.clean, texts)
}

to_sento_corpus_tm <- function(x, dates, do.clean, texts, hasLocalDate = FALSE) {
  features <- NLP::meta(x, type = "indexed")
  if (length(x) != length(texts)) {
    stop("The input corpus does not have a single 'content' character vector per corpus element.")
  }
  if (is.null(dates)) {
    if (hasLocalDate == FALSE && !("date" %in% colnames(features))) {
      stop("There is no 'date' metadata element in the corpus and 'dates' = NULL.")
    } else if (hasLocalDate == TRUE && !("date" %in% colnames(features))) {
      features$date <- do.call("c", NLP::meta(x, "date"))
    }
  } else {
    if (length(dates) != length(x))
      stop("The number of dates in 'dates' should be equal to the number of documents.")
    features$date <- dates
  }
  dt <- data.table::data.table("id" = unlist(NLP::meta(x, "id")),
                               "texts" = texts,
                               features) # includes date column
  data.table::setcolorder(dt, c("id", "date", "texts"))
  sento_corpus(dt, do.clean)
}

#' Convert a quanteda or tm corpus object into a sento_corpus object
#'
#' @author Samuel Borms
#'
#' @description Converts most common \pkg{quanteda} and \pkg{tm} corpus objects into a
#' \code{sento_corpus} object. Appropriate available metadata is integrated as features;
#' for a \pkg{quanteda} corpus, this can come from \code{docvars(x)}, for a \pkg{tm} corpus,
#' only \code{meta(x, type = "indexed")} metadata is considered.
#'
#' @param x a \pkg{quanteda} \code{\link[quanteda]{corpus}} object, a \pkg{tm}
#' \code{\link[tm]{SimpleCorpus}} or a \pkg{tm} \code{\link[tm]{VCorpus}} object. For \pkg{tm}
#' corpora, every corpus element should consist of a single \code{"content"} \code{character} vector
#' as the document unit.
#' @param dates an optional sequence of dates as \code{"yyyy-mm-dd"}, of the same length as the number
#' of documents in the input corpus, to define the \code{"date"} column. If \code{dates = NULL}, the
#' \code{"date"} metadata element in the input corpus, if available, will be used but should be in the
#' same \code{"yyyy-mm-dd"} format.
#' @param do.clean see \code{\link{sento_corpus}}.
#'
#' @return A \code{sento_corpus} object, as returned by the \code{\link{sento_corpus}} function.
#'
#' @seealso \code{\link[quanteda]{corpus}}, \code{\link[tm]{SimpleCorpus}}, \code{\link[tm]{VCorpus}},
#' \code{\link{sento_corpus}}
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' txt <- system.file("texts", "txt", package = "tm")
#' reuters <- system.file("texts", "crude", package = "tm")
#'
#' # reshuffle usnews data.frame for use in quanteda and tm
#' dates <- usnews$date
#' usnews$wrong <- "notNumeric"
#' colnames(usnews)[c(1, 3)] <- c("doc_id", "text")
#'
#' # conversion from a quanteda corpus
#' qcorp <- quanteda::corpus(usnews,
#'                           text_field = "text", docid_field = "doc_id")
#' corp1 <- as.sento_corpus(qcorp)
#' corp2 <- as.sento_corpus(qcorp, sample(dates)) # overwrites "date" column
#'
#' # conversion from a tm SimpleCorpus corpus (DataframeSource)
#' tmSCdf <- tm::SimpleCorpus(tm::DataframeSource(usnews))
#' corp3 <- as.sento_corpus(tmSCdf)
#'
#' # conversion from a tm SimpleCorpus corpus (DirSource)
#' tmSCdir <- tm::SimpleCorpus(tm::DirSource(txt))
#' corp4 <- as.sento_corpus(tmSCdir, dates[1:length(tmSCdir)])
#'
#' # conversion from a tm VCorpus corpus (DataframeSource)
#' tmVCdf <- tm::VCorpus(tm::DataframeSource(usnews))
#' corp5 <- as.sento_corpus(tmVCdf)
#'
#' # conversion from a tm VCorpus corpus (DirSource)
#' tmVCdir <- tm::VCorpus(tm::DirSource(reuters),
#'                        list(reader = tm::readReut21578XMLasPlain))
#' corp6 <- as.sento_corpus(tmVCdir, dates[1:length(tmVCdir)])
#'
#' @export
as.sento_corpus <- function(x, dates = NULL, do.clean = FALSE) {
  UseMethod("as.sento_corpus", x)
}

