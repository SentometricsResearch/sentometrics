
loc <- "nonpackage/SHINY (BELGA)"

str <- c(paste0(loc, "/code/"))
fileSources <- list.files(str, pattern = "*.R$", full.names = TRUE, ignore.case = TRUE)
ind <- stringi::stri_detect(fileSources, regex = "(sentimentr)")
sapply(fileSources[ind], source, .GlobalEnv)

require("plyr")
require("xlsx")
require("stringi")
require("data.table")
require("quanteda")
require("XML")
require("compiler")

###### (combine and) convert all input xml files to one data.frame (xmldtXX)

# data_folder <- "nonpackage/nonpackage/SHINY (BELGA)/data/"
# text_files_full <- list.files(path = paste0(data_folder, "text"), pattern = "belgatext", full.names = TRUE)
# text_files <- list.files(path = paste0(data_folder, "text"), pattern = "belgatext", full.names = FALSE)
# xmldt16old <- rbindlist(lapply(text_files_full, xmlToDataFrame, stringsAsFactors = FALSE))
# save(xmldt16old, file = "nonpackage/nonpackage/SHINY (BELGA)/data/xmldt16old.rda")

xmldt16 <- xmlToDataFrame(paste0(loc, "/data/text/360texts-2016.xml", stringsAsFactors = FALSE))
save(xmldt16, file = paste0(loc, "/data/xmldt16.rda"))

xmldt15 <- xmlToDataFrame(paste0(loc, "/data/text/360texts-2015.xml", stringsAsFactors = FALSE))
save(xmldt15, file = paste0(loc, "/data/xmldt15.rda"))

xmldt14 <- xmlToDataFrame(paste0(loc, "/data/text/360texts-2014.xml", stringsAsFactors = FALSE))
save(xmldt14, file = paste0(loc, "/data/xmldt14.rda"))

xmldt13 <- xmlToDataFrame(paste0(loc, "/data/text/360texts-2013.xml", stringsAsFactors = FALSE))
save(xmldt13, file = paste0(loc, "/data/xmldt13.rda"))

xmldt12 <- xmlToDataFrame(paste0(loc, "/data/text/360texts-2012.xml", stringsAsFactors = FALSE))
save(xmldt12, file = paste0(loc, "/data/mldt12.rda"))

xmldt11 <- xmlToDataFrame(paste0(loc, "/data/text/360texts-2011.xml", stringsAsFactors = FALSE))
save(xmldt11, file = paste0(loc, "/data/xmldt11.rda"))

xmldt10 <- xmlToDataFrame(paste0(loc, "/data/360texts-2010.xml", stringsAsFactors = FALSE))
save(xmldt10, file = paste0(loc, "/data/xmldt10.rda"))

load(paste0(loc, "/data/xmldt16old.rda"))
load(paste0(loc, "/data/xmldt16.rda")) # doesn't have body and lead (are empty strings), thus matched with above
load(paste0(loc, "/data/xmldt15.rda"))
load(paste0(loc, "/data/xmldt14.rda"))
load(paste0(loc, "/data/xmldt13.rda"))
load(paste0(loc, "/data/xmldt12.rda"))
load(paste0(loc, "/data/xmldt11.rda"))
load(paste0(loc, "/data/xmldt10.rda"))

###### a bit of data inspection

categ <- xmldt13$category
categ_freq <- plyr::count(categ)
(categ_order <- categ_freq[order(categ_freq[, 2], decreasing = TRUE), ])

desk <- xmldt16$desk
desk_freq <- plyr::count(desk)
(desk_order <- desk_freq[order(desk_freq[, 2], decreasing = TRUE), ])

xmlPOL <- xmldt16[xmldt16$category == "POL", ]
plyr::count(as.Date(xmlPOL$date, "%d/%m/%Y"))

###### split corpus by language (2016)

languages <- unique(xmldt16$language)
corpus <- vector(mode = 'list', length = length(languages))
names(corpus) <- languages
for(i in languages) {
  xmldt16oldi <- xmldt16old[xmldt16old$language == i, ]

  # subset by itemID where date is largest (most recent article in case of duplicate IDs)
  xmldt16oldi <- plyr::ddply(xmldt16oldi, .(itemId), function(x) x[which.max(as.Date(x$date, "%d/%m/%Y")), ])

  # add new meta data (category, desk and keywords)
  xmldt16i <- xmldt16[xmldt16$language == i, ]
  ids <- xmldt16oldi[xmldt16oldi$id != "", "id"] # these ids have a non-empty body and lead

  meta <- filter(xmldt16i, id %in% ids)[, c("id", "category", "desk", "keywords")]

  IN <- join(xmldt16oldi, meta, by = "id")

  corpus[[i]] <- IN
}

# corpusOld <- corpus
# save(corpusOld, file = paste0(loc, "/data/corpusOld.rda")

###### split corpus by language (2015 - 2010)

data <- list(xmldt15, xmldt14, xmldt13, xmldt12, xmldt11, xmldt10)
languages <- unique(unlist(lapply(data, function(x) unique(x$language))))[1:2] # third is " "
corpusNew <- vector(mode = 'list', length = length(languages))
names(corpusNew) <- languages
for(j in 1:length(languages)) {
  corpusNew[[j]] <- vector(mode = 'list', length = length(data))
  names(corpusNew[[j]]) <- c("2015", "2014", "2013", "2012", "2011", "2010")
  for (i in 1:length(data)) {
    cat(i, j, "\n")
    year <- data[[i]]
    language <- languages[j]
    year <- year[year$language == language, ]

    # subset by itemID where date is largest (most recent article in case of duplicate IDs)
    IN <- plyr::ddply(year, .(itemId), function(x) x[which.max(as.Date(x$date, "%d/%m/%Y")), ])

    corpusNew[[j]][[i]] <- IN
  }
}

# save(corpusNew, file = paste0(loc, "/data/corpusNew.rda")

# load(paste0(loc, "/data/corpusOld.rda")
# load(paste0(loc, "/data/corpusNew.rda")

corpusNew[["fr"]][["2016"]] <- corpusOld[["fr"]]
corpusNew[["nl"]][["2016"]] <- corpusOld[["nl"]]

corpusFull <- corpusNew

# save(corpusFull, file = paste0(loc, "/data/corpusFull.rda")

load(paste0(loc, "/data/corpusFull.rda"))

# SentiWordNet <- read.table("LEXICON/SentiWordNet_3.0.0_20130122.txt", header = FALSE, fill = TRUE)
# names(SentiWordNet) <- c("POS", "ID", "PosScore", "NegScore", "SynsetTerms")

###### create lexicons according to sentimentr format

lexicons <- list(French = list(), Dutch = list())
valences <- list()
input_lexicons = list(French <- c("FEEL", "McDonald_GT"), Dutch = c("GI_GT", "McDonald_GT"))
for(lan in names(lexicons)) {
  input_lexicons_lang <- input_lexicons[[lan]]
  for(lexic in input_lexicons_lang) {
    cat(lan, lexic, "\n")
    inp <- paste0("LEXICON/", lan, "_", lexic, "_lexicon.csv")
    lex <- read.csv(inp, sep = ";")
    # lex = lex[validUTF8(as.character(lex[, 1])), ]
    lex[, 1] <- stringr::str_to_lower(lex[, 1])
    lex[, 1] <- stringr::str_trim(lex[, 1])
    lex <- lex[!duplicated(lex), ]
    lexicons[[lan]][[lexic]] <- sentimentr::as_key(lex) # duplicated words are removed
  }
  val <- read.csv(file <- paste0("LEXICON/", "valence_", lan, ".csv"), sep = ";")
  valences[[lan]] <- sentimentr::as_key(val)
}
names(lexicons) <- c("fr", "nl")
names(valences) <- c("fr", "nl")
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

# save(lexicons, file = paste0(loc, "/data/lexicons.rda")
# save(valences, file = paste0(loc, "/data/valences.rda")

load(paste0(loc, "/data/lexicons.rda"))
load(paste0(loc, "/data/valences.rda"))

###### keywords and categories inspection

corpus <- corpusFull
corpus <- lapply(corpus, function(x)
{xOut <- lapply(seq_along(x),
                function(i) {x[[i]]["year"] <- names(x)[i]; return(x[[i]])});
names(xOut) <- names(x);
return(xOut)
})

lapply(corpus$fr, function(x) {x$keywords <- stringr::str_to_upper(x$keywords)})
lapply(corpus$nl, function(x) {x$keywords <- stringr::str_to_upper(x$keywords)})

kwsFR <- lapply(corpus$fr, function(x) return(unlist(stringr::str_extract_all(x$keywords, stringr::boundary("word")))))
freqsFR <- lapply(kwsFR, plyr::count)
freqFR_Ord <- lapply(freqsFR, function(x) return(x[order(x[, 2], decreasing = TRUE), ]))
top30FR <- lapply(freqFR_Ord, function(x) return(x[1:30, ])) # frequent keywords in French articles per year

kwsNL <- lapply(corpus$nl, function(x) return(unlist(stringr::str_extract_all(x$keywords, stringr::boundary("word")))))
freqsNL <- lapply(kwsNL, plyr::count)
freqNL_Ord <- lapply(freqsNL, function(x) return(x[order(x[, 2], decreasing = TRUE), ]))
top30NL <- lapply(freqNL_Ord, function(x) return(x[1:30, ])) # frequent keywords in French articles per year

# frequent keywords across all years
fullFR_Ord <- as.data.table(rbindlist(freqFR_Ord))[, list(freqTot = sum(freq)), by = x][order(freqTot, decreasing = TRUE)]
fullNL_Ord <- as.data.table(rbindlist(freqNL_Ord))[, list(freqTot = sum(freq)), by = x][order(freqTot, decreasing = TRUE)]

selkwsFR <- c("FOOTBALL", "JUDICIAIRE", "ENTREPRISES", "FINANCES", "GVTFED", "PARTIS", "TERRORISME")
selkwsNL <- c("VOETBAL", "GERECHT", "BEDRIJVEN", "FINANCIEN", "FEDREG", "PARTIJEN", "TERRORISME")

catFR <- as.data.table(plyr::count(rbindlist(corpus$fr, use.names = TRUE)$category))[order(freq, decreasing = TRUE)]
catNL <- as.data.table(plyr::count(rbindlist(corpus$nl, use.names = TRUE)$category))[order(freq, decreasing = TRUE)]

selcatFR <- c("POL", "ECO")
selcatNL <- c("POL", "ECO")

###### corpus selection

corpusFOOT <- list(fr = lapply(corpus$fr, function(x) x[stringi::stri_detect_fixed(x$keywords, "FOOTBALL") & x$body != "  ", ]),
                   nl = lapply(corpus$nl, function(x) x[stringi::stri_detect_fixed(x$keywords, "VOETBAL") & x$body != "  ", ]))
corpusFOOTall <- lapply(corpusFOOT, rbindlist, use.names = TRUE)

corpusTER <- list(fr = lapply(corpus$fr, function(x) x[stringi::stri_detect_fixed(x$keywords, "TERRORISME") & x$body != "  ", ]),
                  nl = lapply(corpus$nl, function(x) x[stringi::stri_detect_fixed(x$keywords, "TERRORISME") & x$body != "  ", ]))
corpusTERall <- lapply(corpusTER, rbindlist, use.names = TRUE)

corpusFIN <- list(fr = lapply(corpus$fr, function(x) x[stringi::stri_detect_fixed(x$keywords, "FINANCES") & x$body != "  ", ]),
                  nl = lapply(corpus$nl, function(x) x[stringi::stri_detect_fixed(x$keywords, "FINANCIEN") & x$body != "  ", ]))
corpusFINall <- lapply(corpusFIN, rbindlist, use.names = TRUE)

corpusECO <- lapply(corpus, lapply, dplyr::filter, category == "ECO" & body != " ")
corpusECOall <- lapply(corpusECO, rbindlist, use.names = TRUE)

corpusPOL <- lapply(corpus, lapply, dplyr::filter, category == "POL" & body != " ")
corpusPOLall <- lapply(corpusPOL, rbindlist, use.names = TRUE)

# finWordsFR <- c("Bel 20", "Bel20", "BEL20", "BEL 20", "bourse", "dividende", "index")
# finWordsNL <- c("Bel 20", "Bel20", "BEL20", "BEL 20", "beurs", "dividend", "index")

# finFR <- stringi::stri_detect_regex(corpus$fr$body, paste0(finWordsFR, collapse = "|"))
# finNL <- stringi::stri_detect_regex(corpus$nl$body, paste0(finWordsNL, collapse = "|"))

# corpusFIN <- list(fr = corpus$fr[finFR & corpus$fr$body != " ", ], nl = corpus$nl[finNL & corpus$nl$body != " ", ])

to_quanteda_dfm <- function(corp) {

  c <- quanteda::corpus(corp, docid_field = "id", text_field = "body")

  dfm <- quanteda::dfm(quanteda::tokenize(c, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE,
                                          remove_separators = TRUE, ngrams = 1))

  return(dfm)
}

dfmFOOT_fr <- to_quanteda_dfm(corpusFOOTall$fr); save(dfmFOOT_fr, file = paste0(loc, "/data/dfmFOOT_fr.rda"))
dfmFOOT_nl <- to_quanteda_dfm(corpusFOOTall$nl); save(dfmFOOT_nl, file = paste0(loc, "/data/dfmFOOT_nl.rda"))
dfmTER_fr <- to_quanteda_dfm(corpusTERall$fr); save(dfmTER_fr, file = paste0(loc, "/data/dfmTER_fr.rda"))
dfmTER_nl <- to_quanteda_dfm(corpusTERall$nl); save(dfmTER_nl, file = paste0(loc, "/data/dfmdfmTER_nl.rda"))
dfmFIN_fr <- to_quanteda_dfm(corpusFINall$fr); save(dfmFIN_fr, file = paste0(loc, "/data/dfmFIN_fr.rda"))
dfmFIN_nl <- to_quanteda_dfm(corpusFINall$nl); save(dfmFIN_nl, file = paste0(loc, "/data/dfmFIN_nl.rda"))
dfmECO_fr <- to_quanteda_dfm(corpusECOall$fr); save(dfmECO_fr, file = paste0(loc, "/data/dfmECO_fr.rda"))
dfmECO_nl <- to_quanteda_dfm(corpusECOall$nl); save(dfmECO_nl, file = paste0(loc, "/data/dfmECO_nlr.rda"))
dfmPOL_fr <- to_quanteda_dfm(corpusPOLall$fr); save(dfmPOL_fr, file = paste0(loc, "/data/dfmPOL_fr.rda"))
dfmPOL_nl <- to_quanteda_dfm(corpusPOLall$nl); save(dfmPOL_nl, file = paste0(loc, "/data/dfmPOL_nl.rda"))

load(paste0(loc, "/data/dfmFOOT_fr.rda"))
load(paste0(loc, "/data/dfmFOOT_nl.rda"))
load(paste0(loc, "/data/dfmTER_fr.rda"))
load(paste0(loc, "/data/dfmdfmTER_nl.rda"))
load(paste0(loc, "/data/dfmFIN_fr.rda"))
load(paste0(loc, "/data/dfmFIN_nl.rda"))
load(paste0(loc, "/data/dfmECO_fr.rda"))
load(paste0(loc, "/data/dfmECO_nlr.rda"))
load(paste0(loc, "/data/dfmPOL_fr.rda"))
load(paste0(loc, "/data/dfmPOL_nl.rda"))

###### sentiment calculation (sentiment calculation from sentimentr not very fast for large corpora)

sentimentr_complex <- function(text, lex, val) {

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

sentimentr_simple <- function(texts, lexicon) {

  id <- id2 <- P <- N <- NULL

  words <- lexicon[[1]]
  space_words <-  words[grep("\\s", words)]

  dt <- data.table(id = 1:length(texts), docs = texts)
  dt[, 'words' := list(make_words(space_fill(docs, space_words)))] # slow (store as an object and pass as an argument?)

  # Make the data frame long by stretching out words
  word_dat <- dt[, .(words = unlist(words)), by = c('id')]
  word_dat[, id2 := seq_len(.N), by = 'id']

  # Point polarity word locations and the value for polarized word
  word_dat[, "P"] <- lexicon[word_dat[["words"]]][[2]] # feature of as_key()

  scoresdt <- word_dat[, list(net_sent = sum(P, na.rm = TRUE),
                              word_count = as.double(ifelse(length(words) > 1, length(words), 0)),
                              doc_counter = 1), by = id]

  ### word_count sometimes +- a few words for same text...

  return(scoresdt)
}

sentiment_sento <- function(dfm, lexicon) {

  allWords <- quanteda::featnames(dfm)

  wCounts <- rowSums(dfm, na.rm = TRUE)
  weights <- quanteda::tf(dfm, scheme = "prop") # weight = (words freq. / total words freq.) per document
  fdmWeighted <- t(weights)

  lexWords <- lexicon$x
  lexScores <- lexicon$y

  # locate polarized words and set weights to their polarity or keep at zero
  allScores <- rep(0, length(allWords))
  polInd <- allWords %in% lexWords
  polWords <- allWords[allWords %in% lexWords]
  allScores[polInd] <- lexScores[lexWords %in% polWords]
  names(allScores) <- allWords

  # scores per document equal to (frequency * weight * polarity score)
  scores <- rowSums(t(fdmWeighted * allScores))

  sent <- data.table(id = as.numeric(1:nrow(dfm)))
  sent$doc_id <- rownames(dfm)
  sent$net_sent <- scores
  sent$word_count <- wCounts
  sent$doc_counter <- 1

  return(sent)
}

sentiment <- function(corpus, lexicons, valences = NULL, languagesIn, lexiconsIn, keywords = NA) {

  out <- list()

  if(!(all(is.na(keywords)))) {

    for(key_w in keywords) {
      for(lan in languagesIn) {
        corp <- corpus[[lan]]
        ind <- stringi::stri_detect_fixed(corp$body, key_w) & corp$body != " " # TRUE means keyword present in text
        corp <- corp[ind, ]
        text <- corp$body
        dates <- corp$date

        if (!is.null(valences)) val <- valences[[lan]]
        for(lexic in lexiconsIn) {
          lex <- lexicons[[lan]][[lexic]]

          # sent <- sentimentr_complex(text, lex, val)
          sent <- sentimentr_simple(text, lex)
          sent$date <- as.Date(dates, "%d/%m/%Y")
          sent$keyword <- key_w
          sent$lexicon <-lexic
          sent$language <- lan
          sent$desk <- corp$desk
          sent$year <- corp$year

          name <- paste0(key_w, "_", lexic, " (", lan, ")")
          out[[name]] <- sent

          cat("done:", key_w, "-", lan, "-", lexic, "\n") # progress statement
        }
      }
    }
  } else { # if no keywords supplied, calculate sentiment for provided corpus

    for(lan in languagesIn) {
      corp <- corpus[[lan]]
      ind <- corp$body != " "
      corp <- corp[ind, ]
      text <- corp$body
      dates <- corp$date

      if (!is.null(valences)) val <- valences[[lan]]
      for(lexic in lexiconsIn) {
        lex <- lexicons[[lan]][[lexic]]

        # sent <- sentimentr_complex(text, lex, val)
        sent <- sentimentr_simple(text, lex)
        sent$date <- as.Date(dates, "%d/%m/%Y")
        sent$lexicon <- lexic
        sent$language <- lan
        sent$desk <- corp$desk
        sent$year <- corp$year

        name <- paste0(lexic, " (", lan, ")")
        out[[name]] <- sent

        cat("done:", lan, "-", lexic, "\n") # progress statement
      }
    }
  }

  return(out)
}

sentiment_dfm <- function(dfm1, dfm2, corpus, lexicons, languagesIn, lexiconsIn) {

  out <- list()
  dfms <- list(dfm1, dfm2)
  names(dfms) <- languagesIn

  for(lan in languagesIn) {
    dfm <- dfms[[lan]]
    corp <- corpus[[lan]]

    for(lexic in lexiconsIn) {
      lex <- lexicons[[lan]][[lexic]]

      sent <- sentiment_sento(dfm, lex)
      sent$date <- as.Date(corp$date, "%d/%m/%Y")
      sent$lexicon <- lexic
      sent$language <- lan
      sent$desk <- corp$desk
      sent$year <- corp$year

      name <- paste0(lexic, " (", lan, ")")
      out[[name]] <- sent

      cat("done:", lan, "-", lexic, "\n") # progress statement
    }
  }

  return(out)
}

######  calculations using sentimentr_simple()

foot <- sentiment(corpusFOOTall, lexicons, languagesIn = c("nl", "fr"), lexiconsIn = c("General", "Financial"))
foot <- lapply(foot, function(x) {x$keyword <- "Football"; return(x)})
names(foot) <- paste0("Football_", names(foot))
save(foot, file = paste0(loc, "/data/foot.rda"))

ter <- sentiment(corpusTERall, lexicons, languagesIn = c("nl", "fr"), lexiconsIn = c("General", "Financial"))
ter <- lapply(ter, function(x) {x$keyword <- "Terrorism"; return(x)})
names(ter) <- paste0("Terrorism_", names(ter))
save(ter, file = paste0(loc, "/data/ter.rda"))

fin <- sentiment(corpusFINall, lexicons, languagesIn = c("nl", "fr"), lexiconsIn = c("General", "Financial"))
fin <- lapply(fin, function(x) {x$keyword <- "Financial"; return(x)})
names(fin) <- paste0("Finance", names(fin))
save(fin, file = paste0(loc, "/data/fin.rda"))

eco <- sentiment(corpusECOall, lexicons, languagesIn = c("nl", "fr"), lexiconsIn = c("General", "Financial"))
eco <- lapply(eco, function(x) {x$keyword <- "Economy"; return(x)})
names(eco) <- paste0("Economy_", names(eco))
save(eco, file = paste0(loc, "/data/eco.rda"))

pol <- sentiment(corpusPOLall, lexicons, languagesIn = c("nl", "fr"), lexiconsIn = c("General", "Financial"))
pol <- lapply(pol, function(x) {x$keyword <- "Political"; return(x)})
names(pol) <- paste0("Politics_", names(pol))
save(pol, file = paste0(loc, "/data/pol.rda"))

load(paste0(loc, "/data/foot.rda"))
load(paste0(loc, "/data/ter.rda"))
load(paste0(loc, "/data/fin.rda"))
load(paste0(loc, "/data/eco.rda"))
load(paste0(loc, "/data/pol.rda"))

comb <- c(foot, ter, fin, eco, pol)
outTmp <- rbindlist(comb, use.names = TRUE)
out <- outTmp

save(out, file = paste0(loc, "/sentimentShinyAll.rda"))
load(paste0(loc, "/sentimentShinyAll.rda"))

###### calculations using sentiment_sento()

l <- c("nl", "fr")

foot2 <- sentiment_dfm(dfmFOOT_nl, dfmFOOT_fr, corpusFOOTall, lexicons, languagesIn = l, lexiconsIn = c("General", "Financial"))
foot2 <- lapply(foot2, function(x) {x$keyword <- "Football"; return(x)})
names(foot2) <- paste0("Football_", names(foot2))
save(foot2, file = paste0(loc, "/data/foot2.rda"))

ter2 <- sentiment_dfm(dfmTER_nl, dfmTER_fr, corpusTERall, lexicons, languagesIn = l, lexiconsIn = c("General", "Financial"))
ter2 <- lapply(ter2, function(x) {x$keyword <- "Terrorism"; return(x)})
names(ter2) <- paste0("Terrorism_", names(ter2))
save(ter2, file = paste0(loc, "/data/ter2.rda"))

fin2 <- sentiment_dfm(dfmFIN_nl, dfmFIN_fr, corpusFINall, lexicons, languagesIn = l, lexiconsIn = c("General", "Financial"))
fin2 <- lapply(fin2, function(x) {x$keyword <- "Financial"; return(x)})
names(fin2) <- paste0("Financial_", names(fin2))
save(fin2, file = paste0(loc, "/data/fin2.rda"))

eco2 <- sentiment_dfm(dfmECO_nl, dfmECO_fr, corpusECOall, lexicons, languagesIn = l, lexiconsIn = c("General", "Financial"))
eco2 <- lapply(eco2, function(x) {x$keyword <- "Economy"; return(x)})
names(eco2) <- paste0("Economy_", names(eco2))
save(eco2, file = paste0(loc, "/data/eco2.rda"))

pol2 <- sentiment_dfm(dfmPOL_nl, dfmPOL_fr, corpusPOLall, lexicons, languagesIn = l, lexiconsIn = c("General", "Financial"))
pol2 <- lapply(pol2, function(x) {x$keyword <- "Political"; return(x)})
names(pol2) <- paste0("Political_", names(pol2))
save(pol2, file = paste0(loc, "/data/pol2.rda"))

load(paste0(loc, "/data/foot2.rda"))
load(paste0(loc, "/data/ter2.rda"))
load(paste0(loc, "/data/fin2.rda"))
load(paste0(loc, "/data/eco2.rda"))
load(paste0(loc, "/data/pol2.rda"))

comb2 <- c(foot2, ter2, fin2, eco2, pol2)
outTmp <- rbindlist(comb2, use.names = TRUE)
out <- outTmp

save(out, file = paste0(loc, "/sentimentShinyAll2.rda"))
load(paste0(loc, "/sentimentShinyAll2.rda"))

years <- unique(out$year)
outTmps <- list()
for (yr in years) {
  for (lan in c("fr", "nl")) {
    cat(lan, yr, "\n")
    corp <- corpus[[lan]][[yr]]
    outTmp <- setkey(out[language == lan & year == yr, ], doc_id)
    corpTmp <- setkey(data.table(lead = corp$lead, id = corp$id), id)

    reps <- outTmp[, list(n = .N), by = doc_id]

    leadsIn <- corpTmp[id %in% outTmp$doc_id, ]$lead
    leadsIn <- rep(leadsIn, reps$n)
    outTmp[, "lead" := leadsIn]
    outTmps[[paste0(lan, yr)]] <- outTmp
  }
}

out <- rbindlist(outTmps)
save(out, file = paste0(loc, "/sentimentShinyAll3.rda"))

