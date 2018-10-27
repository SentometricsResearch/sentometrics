
#' Set up lexicons (and valence word list) for use in sentiment analysis
#'
#' @author Samuel Borms
#'
#' @description Structures provided lexicon(s) and optionally valence words. One can for example combine (part of) the
#' built-in lexicons from \code{data("list_lexicons")} with other lexicons, and add one of the built-in valence word lists
#' from \code{data("list_valence_shifters")}. This function makes the output coherent, by converting all words to
#' lowercase and checking for duplicates. All entries consisting of more than one word are discarded, as required for
#' bag-of-words sentiment analysis.
#'
#' @param lexiconsIn a named \code{list} of (raw) lexicons, each element as a \code{data.table} or a \code{data.frame} with
#' respectively a words column and a polarity score column. The built-in lexicons
#' accessible via \code{list_lexicons} should be passed here first as well.
#' @param valenceIn a single valence word list as a \code{data.table} or a \code{data.frame} with respectively a \code{"x"}
#' and a \code{"y"} or \code{"t"} column. The first column has the words, \code{"y"} has the values for bigram
#' shifting, and \code{"t"} has the types of the valence shifter for a clustered approach to sentiment calculation
#' (supported types: \code{1} = negators, \code{2} = amplifiers, \code{3} = deamplifiers). If three columns
#' are provided, the first two will be considered only. This argument can be one of the built-in
#' valence word lists accessible via \code{list_valence_shifters}. A word that appears in both a lexicon
#' and the valence word list is prioritized as a lexical entry during sentiment calculation. If \code{NULL}, valence
#' shifting is not applied in the sentiment analysis.
#' @param do.split a \code{logical} that if \code{TRUE} splits every lexicon into a separate positive polarity and negative
#' polarity lexicon.
#'
#' @return A \code{list} of class \code{sentolexicons} with each lexicon as a separate element according to its name, as a
#' \code{data.table}, and optionally an element named \code{valence} that comprises the valence words. Every \code{"x"} column
#' contains the words, every \code{"y"} column contains the polarity scores. The \code{"t"} column for valence shifters
#' contains the valence shifting types.
#'
#' @examples
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#'
#' # lexicons straight from built-in word lists
#' l1 <- sento_lexicons(list_lexicons[c("LM_en", "HENRY_en")])
#'
#' # including a self-made lexicon, with and without valence shifters
#' lexIn <- c(list(myLexicon = data.table(w = c("nice", "boring"), s = c(2, -1))),
#'            list_lexicons[c("GI_en")])
#' valIn <- list_valence_shifters[["en"]]
#' l2 <- sento_lexicons(lexIn)
#' l3 <- sento_lexicons(lexIn, valIn)
#' l4 <- sento_lexicons(lexIn, valIn[, c("x", "y")], do.split = TRUE)
#' l5 <- sento_lexicons(lexIn, valIn[, c("x", "t")], do.split = TRUE)
#' l6 <- l5[c("GI_en_POS", "valence")] # preserves sentolexicons class
#'
#' \dontrun{
#' # include lexicons from lexicon package
#' lexIn2 <- list(hul = lexicon::hash_sentiment_huliu, joc = lexicon::hash_sentiment_jockers)
#' l7 <- sento_lexicons(c(lexIn, lexIn2), valIn)}
#'
#' \dontrun{
#' # faulty extraction, no replacement allowed
#' l5["valence"]
#' l2[0]
#' l3[22]
#' l4[1] <- l2[1]
#' l4[[1]] <- l2[[1]]
#' l4$GI_en_NEG <- l2$myLexicon}
#'
#' @export
sento_lexicons <- function(lexiconsIn, valenceIn = NULL, do.split = FALSE) {

  if (!("list" %in% class(lexiconsIn)))
    stop("The 'lexiconsIn' input should be a named list.")
  if (is.null(names(lexiconsIn)))
    stop("The lexicons are not named.")
  if (any(is.na(names(lexiconsIn))))
    stop("At least one lexicon's name is NA. Please provide proper names.")
  if (!is_names_correct(names(lexiconsIn)))
    stop("At least one lexicon's name contains '-'. Please provide proper names.")
  if (!is.data.frame(valenceIn) && !is.null(valenceIn))
    stop("The 'valenceIn' argument should be a data.table or data.frame if not NULL.")
  if (sum(duplicated(names(lexiconsIn))) > 0) { # check for duplicated lexicon names
    duplics <- unique(names(lexiconsIn[duplicated(names(lexiconsIn))]))
    stop(paste0("Names of lexicons are not unique. Following names occur at least twice: ",
                paste0(duplics, collapse = ", "), "."))
  }

  lexNames <- names(lexiconsIn)
  lexicons <- suppressWarnings(lapply(lexiconsIn, sento_as_key)) # supress warnings on removal of duplicated values
  names(lexicons) <- lexNames
  if (do.split == TRUE) { # split each lexicon into a positive and a negative polarity words only lexicon
    lexiconsPos <- lapply(lexicons, function(lex) return(lex[lex$y > 0]))
    names(lexiconsPos) <- paste0(names(lexicons), "_POS")
    lexiconsNeg <- lapply(lexicons, function(lex) return(lex[lex$y < 0]))
    names(lexiconsNeg) <- paste0(names(lexicons), "_NEG")
    lexicons <- c(lexiconsPos, lexiconsNeg)
  }
  lexicons <- lapply(lexicons, function(l) l[!stringi::stri_detect(l$x, regex = "\\s+"), ])
  if (!is.null(valenceIn)) {
    if (!all(names(valenceIn) %in% c("x", "y", "t")) || !(ncol(valenceIn) %in% c(2, 3)))
      stop("Provide columns 'x' and 'y' and/or 't' to the 'valenceIn' argument.")
    if (ncol(valenceIn) == 3) valenceIn <- valenceIn[, 1:2]
    if ("t" %in% names(valenceIn)) {
      if (!all(unique(valenceIn[["t"]]) %in% c(1, 2, 3)))
        stop("Supported types of valence shifters under the 't' column are 1, 2 and 3.")
    }
    valenceIn$x <- stringi::stri_trans_tolower(valenceIn$x)
    valenceIn <- valenceIn[!(stringi::stri_detect(valenceIn$x, regex = "\\s+") | duplicated(valenceIn$x)), ]
    lexicons[["valence"]] <- valenceIn
  }

  class(lexicons) <- c("sentolexicons", class(lexicons))

  return(lexicons)
}

#' @export
`[.sentolexicons` <- function(x, i, ...) {
  xNew <- NextMethod("[")
  if (length(xNew) == 0 || length(xNew) > length(x) || any(is.na(names(xNew)))) stop("Indexing out of bounds.")
  if (all(names(xNew) == "valence")) stop("Keep at least one lexicon (on top of a table of valence shifters).")
  class(xNew) <- class(x)
  xNew
}

#' @export
`[<-.sentolexicons` <- function(x, i, value) {
  stop("Replacement not allowed.")
}

#' @export
`[[<-.sentolexicons` <- function(x, i, value) {
  stop("Replacement not allowed.")
}

#' @export
`$<-.sentolexicons` <- function(x, i, value) {
  stop("Replacement not allowed.")
}

#' @export
`names<-.sentolexicons` <- function(x, value) {
  if (any(duplicated(value))) stop("No duplicated names allowed.")
  NextMethod("names<-")
}

