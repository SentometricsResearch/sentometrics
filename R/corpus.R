
# overall depends: stringi, dplyr (@import), data.table (@import), xts, tidytext, tidyr

# INPUT:
### A dataframe with: id (unique), date, text, features (remaining columns) -- id to be determined if useful?

add_features <- function(corpuS, features) UseMethod("add_features")

corpuS <- function(text_df, minWords = 200, maxWords = Inf) { # constructor

  # construct corpus as a quanteda corpus
  c <- quanteda::corpus(text_df,
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

  # tokenize into proper words and apply lower and uppor bounds for number of words per text
  tWords <- quanteda::tokenize(c,
                               remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                               ngrams = 1)

  c <- quanteda::corpus_subset(c, quanteda::ntoken(tWords) >= minWords && quanteda::ntoken(tWords) <= maxWords)

  class(c) <- c("corpuS", class(c))

  return(c)

}

add_features.corpuS <- function(corpuS, features) {

  for (i in seq_along(features)) {
    quanteda::docvars(corpuS, field = names(features)[i]) <- features[[i]]
  }

  # update features vector
  corpuS$features <- c(corpuS$features, names(features))

  return(corpuS)
}

