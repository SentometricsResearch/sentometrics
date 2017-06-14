
setwd("C:/Users/gebruiker/Desktop/Shiny App")

source("sourceall.R")

require("plyr")
require("xlsx")
require("stringi")
require("data.table")

data_folder <- "data/"
text_files_full <- list.files(path = paste0(data_folder, "text"), full.names = TRUE)
text_files <- list.files(path = paste0(data_folder, "text"), full.names = FALSE)

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

desk <- xmldt_new$desk
desk_freq <- plyr::count(desk)
(desk_order <- desk_freq[order(desk_freq[, 2], decreasing = TRUE), ])

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

###### create lexicons according to sentimentr format
lexicons = list(French = list(), Dutch = list())
valences = list()
input_lexicons = list(French = c("FEEL", "McDonald_GT"), Dutch = c("GI_GT", "McDonald_GT"))
for(lan in names(lexicons)) {
  input_lexicons_lang <- input_lexicons[[lan]]
  for(lexic in input_lexicons_lang) {
    cat(lan, lexic, "\n")
    inp = paste0(data_folder, "lexicon/", lan, "_", lexic, "_lexicon.csv")
    lex = read.csv(inp, sep = ";")
    # lex = lex[validUTF8(as.character(lex[, 1])), ]
    lex[, 1] = stringr::str_to_lower(lex[, 1])
    lex[, 1] = stringr::str_trim(lex[, 1])
    lex = lex[!duplicated(lex), ]
    lexicons[[lan]][[lexic]] = sentimentr::as_key(lex) # duplicated words are removed
  }
  val <- read.csv(file = paste0(data_folder, "lexicon/", "valence_", lan, ".csv"), sep = ";")
  valences[[lan]] <- sentimentr::as_key(val)
}
names(lexicons) = c("fr", "nl")
names(valences) = c("fr", "nl")
lexicons <- lapply(lexicons, function(x) {names(x) <- c("General", "Financial"); return(x)})

for(lan in names(lexicons)) {
  val <- valences[[lan]]
  sames <- c() # indices where lexicon words and valence words are the same
  for(lex in lexicons[[lan]]) {
    same <- which(val$x %in% lex$x) # x are the words
    sames <- c(sames, same)
  }
  valences[[lan]] <- val[!unique(sames), ]
}

###### corpus selection

corpus$fr$keywords <- stringr::str_to_upper(corpus$fr$keywords)
corpus$nl$keywords <- stringr::str_to_upper(corpus$nl$keywords)

kwsBelgaFR <- unlist(stringr::str_extract_all(corpus$fr$keywords, stringr::boundary("word")))
freqsFR <- plyr::count(kwsBelgaFR)
kwsOrdFR <- freqsFR[order(freqsFR[, 2], decreasing = TRUE), ]
kwsOrdFR[1:30, ] # frequent keywords in French articles

kwsBelgaNL <- unlist(stringr::str_extract_all(corpus$nl$keywords, stringr::boundary("word")))
freqsNL <- plyr::count(kwsBelgaNL)
kwsOrdNL <- freqsNL[order(freqsNL[, 2], decreasing = TRUE), ]
kwsOrdNL[1:30, ] # frequent keywords in Dutch articles

terFR <- stringi::stri_detect_fixed(corpus$fr$keywords, "TERRORISME")
terNL <- stringi::stri_detect_fixed(corpus$nl$keywords, "TERRORISME")

corpusTER <- list(fr = corpus$fr[terFR & corpus$fr$body != " ", ],
                  nl = corpus$nl[terNL & corpus$nl$body != " ", ])

footFR <- stringi::stri_detect_fixed(corpus$fr$keywords, "FOOTBALL")
footNL <- stringi::stri_detect_fixed(corpus$nl$keywords, "VOETBAL")

corpusFOOT <- list(fr = corpus$fr[footFR & corpus$fr$body != " ", ],
                   nl = corpus$nl[footNL & corpus$nl$body != " ", ])


finWordsFR <- c("Bel 20", "Bel20", "BEL20", "BEL 20", "bourse", "dividende", "index")
finWordsNL <- c("Bel 20", "Bel20", "BEL20", "BEL 20", "beurs", "dividend", "index")

finFR <- stringi::stri_detect_regex(corpus$fr$body, paste0(finWordsFR, collapse = "|"))
finNL <- stringi::stri_detect_regex(corpus$nl$body, paste0(finWordsNL, collapse = "|"))

corpusFIN <- list(fr = corpus$fr[finFR & corpus$fr$body != " ", ],
                  nl = corpus$nl[finNL & corpus$nl$body != " ", ])

corpusECO <- lapply(corpus, dplyr::filter, category == "ECO" & body != " ")

# corpusPOL <- lapply(corpus, dplyr::filter, category == "POL")

###### sentiment calculation (sentiment calculation itself not very fast for large corpora)

sentimentr_calc <- function(text, lex, val) {

  sentCalc <- sentiment_custom(text.var = text,
                               polarity_dt = lex,
                               valence_shifters_dt = val) # based on 'sentimentr' package

  sent <- sentCalc[, list(words = gsub(" :", " =", paste0(unlist(words), collapse = "; ")), # words useful here?
                          word_count = sum(word_count, na.rm = TRUE),
                          doc_counter = 1, # 1 doc per element_id (later summed)
                          net_sent = sum((sentiment * word_count) / sum(word_count, na.rm = TRUE), na.rm = TRUE)),
                   by = list(element_id)]

  return(sent)
}

sentiment <- function(corpus, lexicons, valences, languagesIn, lexiconsIn, keywords = NA) {

  out <- list()

  if(!(all(is.na(keywords)))) {

    for(key_w in keywords) {
      for(lan in languagesIn) {
        corp <- corpus[[lan]]
        ind <- stringi::stri_detect_fixed(corp$body, key_w) # TRUE means keyword present in text
        corp <- corp[ind, ]
        text <- corp$body[corp$body != " "] # leave out remaining empty texts

        val <- valences[[lan]]
        for(lexic in lexiconsIn) {
          lex <- lexicons[[lan]][[lexic]]

          sent <- sentimentr_calc(text, lex, val)
          sent$date <- as.Date(corp$date[sent$element_id], "%d/%m/%Y")
          sent$keyword <- key_w
          sent$lexicon <-lexic
          sent$language <- lan
          sent$desk <- corp$desk

          name <- paste0(key_w, "_", lexic, " (", lan, ")")
          out[[name]] <- sent

          cat("done:", key_w, "-", lan, "-", lexic, "\n") # progress statement
        }
      }
    }
  } else { # if no keywords supplied, calculate sentiment in provided corpus

    for(lan in languagesIn) {
      corp <- corpus[[lan]]
      text <- corp$body[corp$body != " "]

      val <- valences[[lan]]
      for(lexic in lexiconsIn) {
        lex <- lexicons[[lan]][[lexic]]

        sent <- sentimentr_calc(text, lex, val)
        sent$date <- as.Date(corp$date[sent$element_id], "%d/%m/%Y")
        sent$lexicon <- lexic
        sent$language <- lan
        sent$desk <- corp$desk

        name <- paste0(lexic, " (", lan, ")")
        out[[name]] <- sent

        cat("done:", lan, "-", lexic, "\n") # progress statement
      }
    }
  }

  return(out)
}

ter <- sentiment(corpusTER, lexicons, valences, c("nl", "fr"), c("General", "Financial"))
ter <- lapply(ter, function(x) {x$keyword <- "Terrorism"; return(x)})
names(ter) <- paste0("Terrorism_", names(ter))

foot <- sentiment(corpusFOOT, lexicons, valences, c("nl", "fr"), c("General", "Financial"))
foot <- lapply(foot, function(x) {x$keyword <- "Football"; return(x)})
names(foot) <- paste0("Football_", names(foot))

fin <- sentiment(corpusFIN, lexicons, valences, c("nl", "fr"), c("General", "Financial"))
fin <- lapply(fin, function(x) {x$keyword <- "Financial"; return(x)})
names(fin) <- paste0("Financial_", names(fin))

eco <- sentiment(corpusECO, lexicons, valences, c("nl", "fr"), c("General", "Financial"))
eco <- lapply(eco, function(x) {x$keyword <- "Economy"; return(x)})
names(eco) <- paste0("Economy_", names(eco))

keywords <- c("Brexit", "Trump", "Michel", "CETA")
outKw <- sentiment(corpus, lexicons, valences, c("nl", "fr"), c("General", "Financial"), keywords)

comb <- c(ter, foot, fin, eco, outKw)
outTmp <- rbindlist(comb, use.names = TRUE)
out <- outTmp

save(out, file = "sentimentShiny.rda")

# keywords <- c("Trump", "Michel", "Brexit", "Vlaams Belang", "PS")
# out <- sentiment(corpusPOL, lexicons, c("nl", "fr"), c("GI_GT", "McDonald_GT", "pattern"), keywords)
# save(out, file = "sentimentPOL.rda")

plyr::count(out$keyword) # number of documents per keyword

###################################################################################

