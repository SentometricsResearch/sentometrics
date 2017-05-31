
setwd("C:/Users/gebruiker/Desktop/Shiny App")

source("sourceall.R")

require("plyr")
require("xlsx")
require("stringi")
require("data.table")

data_folder <- "data/"
text_files_full = list.files(path = paste0(data_folder, "text"), full.names = TRUE)
text_files = list.files(path = paste0(data_folder, "text"), full.names = FALSE)

###### combine and convert all input xml files to one data.table / data.frame
# xmldt <- rbindlist(lapply(text_files_full, xmlToDataFrame, stringsAsFactors = FALSE))
# save(xmldt, file = "xmldt.rda")

# xmldt_new <- xmlToDataFrame("data/text/360texts-2016.xml", stringsAsFactors = FALSE)
# save(xmldt_new, file = "xmldt_new.rda")

load("xmldt.rda")
load("xmldt_new.rda") # doesn't have body and lead (are empty strings)

###### a bit of data inspection
categ <- xmldt_new$category
categ_freq <- plyr::count(categ)
(categ_order <- categ_freq[order(categ_freq[, 2], decreasing = TRUE), ])

xmlPOL <- xmldt_new[xmldt_new$category == "POL", ]
plyr::count(as.Date(xmlPOL$date, "%d/%m/%Y"))

###### split corpus by language
languages = unique(xmldt$language)
corpus = vector(mode = 'list', length = length(languages))
names(corpus) = languages
for(i in languages) {
  xmli <- xmldt[xmldt$language == i, ]

  # subset by itemID where date is largest (most recent article in case of duplicate IDs)
  xmli <- plyr::ddply(xmli, .(itemId), function(x) x[which.max(as.Date(x$date, "%d/%m/%Y")), ])
  #### potentially faster alternatives with dplyr/data.table

  # add new meta data (category, desk and keywords)
  xml_newi <- xmldt_new[xmldt_new$language == i, ]
  ids <- xmli[xmli$id != "", "id"] # these ids have a non-empty body and lead

  meta <- filter(xml_newi, id %in% ids)[, c("id", "category", "desk", "keywords")]

  xmliIN <- join(xmli, meta, by = "id")

  corpus[[i]] <- xmliIN
}

# save(corpus, file = "corpus.rda")

load("corpus.rda")

corpusPOL <- lapply(corpus, dplyr::filter, category == "POL")

###### create lexicons according to sentimentr format
lexicons = list(French = list(), Dutch = list())
input_lexicons = c("pattern", "GI_GT", "McDonald_GT")
for(lan in names(lexicons)) {
  for(lexic in input_lexicons) {
    inp = paste0(data_folder, "lexicon/", lan, "_", lexic, "_lexicon.csv")
    lex = read.csv(inp, encoding = "UTF-8")
    lex = lex[validUTF8(as.character(lex[, 1])), ] # some Dutch words pose a problem (egoïstisch, clichématig, etc.)
    lex[, 1] = stringr::str_to_lower(lex[, 1])
    lex[, 1] = stringr::str_trim(lex[, 1])
    lex = lex[!duplicated(lex), ]
    lexicons[[lan]][[lexic]] = sentimentr::as_key(lex) # words duplicated with different scores are removed
  }
}
names(lexicons) = c("fr", "nl")

###### sentiment calculation (sentiment calculation itself not very fast for large corpora)
sentiment <- function(corpus, lexicons, lang, lexicon, keywords = NA) {
  out <- list()

  for(lan in lang) {
    corp <- corpus[[lan]]
    texts <- corp$body

    if(!any(is.na(keywords))) {
      n <- length(texts)
      keys <- as.data.frame(matrix(nrow = n, ncol = length(keywords)))
      colnames(keys) <- keywords
      for(key_w in keywords)
        keys[, key_w] <- stringi::stri_detect_fixed(texts, key_w) # TRUE means keyword present in text
        ### TO ADD:
        ### (1) number of occurences of keyword per doc
        ### (2) sentiment score aggregation across docs based on occurrence
        ### (3) check for different keywords and information in other metadata (keywords metadata + entiteiten)
    } else {
      keys <- NULL
    }

    for(lexic in lexicon) {
      lex <- lexicons[[lan]][[lexic]]

      sentCalc <- sentiment_custom(text.var = texts, polarity_dt = lex) # based on 'sentimentr' package

      sent <- sentCalc[, list(words = gsub(" :", " =", paste0(unlist(words), collapse = "; ")), # words useful here?
                          word_count = sum(word_count, na.rm = TRUE),
                          doc_counter = 1, # 1 doc per element_id (later summed)
                          net_sent = sum((sentiment * word_count) / sum(word_count, na.rm = TRUE), na.rm = TRUE),
                          lexicon = gsub("_GT", "", lexic), # lexicon identifier
                          language = lan), # language identifier
                       by = list(element_id)]
      sent$date <- as.Date(corp$date[sent$element_id], "%d/%m/%Y")
      if(!is.null(keys)) sent <- cbind(sent, keys)

      name <- paste0(gsub("_GT", "", lexic), " (", lan, ")")
      out[[name]] <- sent

      cat("done:", lan, "-", lexic, "\n") # progress statement
    }
  }
  return(out)
}

keywords <- c("Trump", "Michel", "Brexit", "Vlaams Belang", "PS")
out <- sentiment(corpusPOL, lexicons, c("nl", "fr"), c("GI_GT", "McDonald_GT", "pattern"), keywords)
save(out, file = "sentimentPOL.rda")

sapply(lapply(out, "[", j = keywords, with = FALSE), colSums) # number of documents per keyword

###################################################################################

#### unused stuff ####

# and_or <- function(x, choice) { # helper function
#   if(choice == "&") {
#     ifelse(all(x), TRUE, FALSE) # rows with all words
#   } else if(choice == "|") {
#     ifelse(any(x), TRUE, FALSE) # rows with at least one of the words
#   }
# }
# data <- data[apply(data[, input$keywords, with = FALSE], 1, and_or, choice), ]

