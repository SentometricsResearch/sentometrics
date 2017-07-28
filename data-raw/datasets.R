
############################################################
################## Data sets manipulation ##################
############################################################

######################### US ECONOMY TEXTS CORPUS (1951-2014)

useconomy <- readr::read_csv("data-raw/US_economic_news_1951-2014.csv")
useconomy$text <- stringi::stri_replace_all(useconomy$text, replacement = " ", regex = "</br></br>")
useconomy$text <- stringi::stri_replace_all(useconomy$text, replacement = "", regex = '[\\"]')

# TODO: further text cleaning
# TODO: drop < 1970

months <- lapply(stringi::stri_split(useconomy$date, regex = "/"), "[", 1)
days <- lapply(stringi::stri_split(useconomy$date, regex = "/"), "[", 2)
years <- lapply(stringi::stri_split(useconomy$date, regex = "/"), "[", 3)
yearsLong <- lapply(years, function(x) if (as.numeric(x) > 14) return(paste0("19", x)) else return(paste0("20", x)))

datesNew <- paste0(paste0(unlist(months), "/"), paste0(unlist(days), "/"), unlist(yearsLong))
useconomy$dateNew <- datesNew

dates <- as.character(as.Date(useconomy$dateNew, format = "%m/%d/%Y")) # character date is input requirement
useconomy$dateNewDate <- dates

freqDays <- plyr::count(dates)
freqMonths <- plyr::count(unlist(lapply(stringi::stri_split(dates, regex = "-"),
                                        function(x) return(paste0(x[1:2], collapse = "/")))))
freqYears <- plyr::count(unlist(lapply(stringi::stri_split(dates, regex = "-"), "[", 1)))

USECONOMYNEWS <- useconomy

USECONOMYNEWS$date <- USECONOMYNEWS$dateNewDate

source <- sapply(stringi::stri_split(USECONOMYNEWS$articleid, regex = "_"), "[", 1)
wsj <- ifelse(source == "wsj", 1, 0) # wall street journal
wapo <- abs(wsj - 1) # washington post
USECONOMYNEWS$wsj <- wsj
USECONOMYNEWS$wapo <- wapo

economy <- ifelse(USECONOMYNEWS$relevance == "yes", 1, 0)
noneconomy <- abs(economy - 1)
USECONOMYNEWS$economy <- economy
USECONOMYNEWS$noneconomy <- noneconomy

# delete obsolete columns
USECONOMYNEWS$dateNewDate <- USECONOMYNEWS$dateNew <- USECONOMYNEWS$`_last_judgment_at` <-
USECONOMYNEWS$`_trusted_judgments` <- USECONOMYNEWS$`positivity:confidence` <- USECONOMYNEWS$`relevance:confidence` <-
USECONOMYNEWS$relevance_gold <- USECONOMYNEWS$articleid <- USECONOMYNEWS$`_unit_state` <- USECONOMYNEWS$`_golden` <-
USECONOMYNEWS$positivity_gold <- USECONOMYNEWS$relevance <- USECONOMYNEWS$positivity <- NULL

USECONOMYNEWS <- data.table::as.data.table(USECONOMYNEWS)
USECONOMYNEWS <- USECONOMYNEWS[order(date)]
colnames(USECONOMYNEWS)[1] <- "id"

save(USECONOMYNEWS, file = "data/USECONOMYNEWS.rda", compress = 'xz')
# load("data/USECONOMYNEWS.rda")

######################### S&P500 Index (1988-2014, monthly)

# makes returns in line with USECONOMYNEWS corpus
# "1988-03-01" means monthly return from March to April 1988, such that it aligns with all articles in March

sp500 <- readr::read_csv("data-raw/S&P500.csv")
sp500 <- as.data.frame(sp500[c("Date", "Adj Close")])
sp500 <- sp500[sp500$Date <= "2015-01-01", ]
sp500 <- xts::xts(sp500$`Adj Close`[-1], sp500$Date[-nrow(sp500)])
sp500 <- PerformanceAnalytics::Return.calculate(sp500)
SP500 <- sp500[-1, ]

save(SP500, file = "data/SP500.rda", compress = 'xz')

######################### LEXICONS

# LM csv file reordering
lmRaw <- read.csv("data-raw/lexicons-raw/LM_raw.csv", sep = ";")
lmPOS <- lmRaw[lmRaw$Positive != 0, ][, c("Word", "Positive")]
lmPOS$Polarity <- 1
lmPOS$Positive <- NULL
lmNEG <- lmRaw[lmRaw$Negative != 0, ][, c("Word", "Negative")]
lmNEG$Polarity <- -1
lmNEG$Negative <- NULL
lm <- rbind(lmPOS, lmNEG)
write.csv2(lm, file = "data-raw/lexicons-raw/LM.csv", row.names = FALSE)
# read.csv("data-raw/lexicons-raw/LM.csv")

# FEEL csv file reordering
feelRaw <- read.csv("data-raw/lexicons-raw/FEEL_raw.csv", sep = ";", encoding = "UTF-8")
feelPOS <- feelRaw[feelRaw$polarity == "positive", ][, c("word", "polarity")]
feelPOS$polarity <- 1
feelNEG <- feelRaw[feelRaw$polarity == "negative", ][, c("word", "polarity")]
feelNEG$polarity <- -1
feel <- rbind(feelPOS, feelNEG)
write.csv2(feel, file = "data-raw/lexicons-raw/FEEL.csv", row.names = FALSE)
# read.csv("data-raw/lexicons-raw/FEEL.csv")

# FEEL NL translation restructuring
f <- read.csv("data-raw/lexicons-raw/FEEL_nl.csv", sep = ";")
w <- as.character(f$word[f$word != ""])
p <- f$polarity[!is.na(f$polarity)]
new <- data.frame(word = w, polarity = p)
write.csv2(new, file = "data-raw/lexicons-raw/FEEL_nl.csv", row.names = FALSE)

# FEEL ENG translation restructuring
f <- read.csv("data-raw/lexicons-raw/FEEL_eng.csv", sep = ";")
w <- as.character(f$word[f$word != ""])
p <- f$polarity[!is.na(f$polarity)]
new <- data.frame(word = w, polarity = p)
write.csv2(new, file = "data-raw/lexicons-raw/FEEL_eng.csv", row.names = FALSE)

# GI csv file reordering
giRaw <- read.csv("data-raw/lexicons-raw/GI_raw.csv", sep = ";")
giPOS <- giRaw[giRaw$Positiv == "Positiv", ][, c("Entry", "Positiv")]
giPOS$Polarity <- 1
giPOS$Positiv <- NULL
giNEG <- giRaw[giRaw$Negativ == "Negativ", ][, c("Entry", "Negativ")]
giNEG$Polarity <- -1
giNEG$Negativ <- NULL
gi <- rbind(giPOS, giNEG)
write.csv2(gi, file = "data-raw/lexicons-raw/GI.csv", row.names = FALSE)
# read.csv("data-raw/lexicons-raw/GI.csv")

prepare_word_list <- function(fileName, type, name) {

  w <- read.csv(paste0("data-raw/", type, "-raw/", fileName), sep = ";")
  colnames(w) <- c("x", "y")

  # cleaning
  w$x <- as.character(w$x)
  w$x <- stringr::str_to_lower(w$x)
  w$x <- stringr::str_trim(w$x)
  w$x <- stringi::stri_replace_all(w$x, "", regex = '\\?') # remove question markets
  w$x <- stringi::stri_replace_all(w$x, "", regex = '[\\"]') # remove double quotes
  w$x <- stringi::stri_replace_all(w$x, "", regex = '[()]') # remove single brackets
  w$x <- stringi::stri_replace_all(w$x, "", regex = '[@]') # remove @ character
  w$y <- as.numeric(w$y)

  # change to as_key() format
  w <- w[!duplicated(w$x), ]
  w <- sentimentr::as_key(w, comparison = NULL) # makes absolutely sure duplicated words are removed
  w <- w[w$x != "" & w$x != " " & w$x != "#naam", ]

  fileOut <- paste0("data-raw/", name, ".rda")

  return(list(w = w, file = fileOut))
}

# save lexicons with appropriate names to data-raw/ folder
typeL <- "lexicons"
l <- prepare_word_list("LM.csv", typeL, "LEXICON_LM_ENG"); "LEXICON_LM_ENG" <- l$w
save(LEXICON_LM_ENG, file = l$file)
l <- prepare_word_list("LM_fr.csv", typeL, "LEXICON_LM_FR_tr"); "LEXICON_LM_FR_tr" <- l$w
save(LEXICON_LM_FR_tr, file = l$file)
l <- prepare_word_list("LM_nl.csv", typeL, "LEXICON_LM_NL_tr"); "LEXICON_LM_NL_tr" <- l$w
save(LEXICON_LM_NL_tr, file = l$file)
l <- prepare_word_list("FEEL.csv", typeL, "LEXICON_FEEL_FR"); "LEXICON_FEEL_FR" <- l$w
save(LEXICON_FEEL_FR, file = l$file)
l <- prepare_word_list("FEEL_nl.csv", typeL, "LEXICON_FEEL_NL_tr"); "LEXICON_FEEL_NL_tr" <- l$w
save(LEXICON_FEEL_NL_tr, file = l$file)
l <- prepare_word_list("FEEL_eng.csv", typeL, "LEXICON_FEEL_ENG_tr"); "LEXICON_FEEL_ENG_tr" <- l$w
save(LEXICON_FEEL_ENG_tr, file = l$file)
l <- prepare_word_list("GI.csv", typeL,"LEXICON_GI_ENG"); "LEXICON_GI_ENG" <- l$w
save(LEXICON_GI_ENG, file = l$file)
l <- prepare_word_list("GI_fr.csv", typeL, "LEXICON_GI_FR_tr"); "LEXICON_GI_FR_tr" <- l$w
save(LEXICON_GI_FR_tr, file = l$file)
l <- prepare_word_list("GI_nl.csv", typeL, "LEXICON_GI_NL_tr"); "LEXICON_GI_NL_tr" <- l$w
save(LEXICON_GI_NL_tr, file = l$file)
l <- prepare_word_list("HENRY.csv", typeL, "LEXICON_HENRY_ENG"); "LEXICON_HENRY_ENG" <- l$w
save(LEXICON_HENRY_ENG, file = l$file)
l <- prepare_word_list("HENRY_fr.csv", typeL, "LEXICON_HENRY_FR_tr"); "LEXICON_HENRY_FR_tr" <- l$w
save(LEXICON_HENRY_FR_tr, file = l$file)
l <- prepare_word_list("HENRY_nl.csv", typeL, "LEXICON_HENRY_NL_tr"); "LEXICON_HENRY_NL_tr" <- l$w
save(LEXICON_HENRY_NL_tr, file = l$file)

# creates and places LEXICON data in data folder
form_word_list <- function(type) {

  if (type == "LEXICON") pattern <- c("LEXICON_")
  else if (type == "VALENCE") pattern <- c("NEGATORS_") # change if additional valence shifter categories are added

  objects <- list.files("data-raw/", pattern = pattern)

  listed <- vector(mode = "list")
  for (o in 1:length(objects)) {
    obj <- load(paste0("data-raw/", objects[o])) # name
    toAdd <- eval(parse(text = obj)) # object
    listed[[obj]] <- toAdd
    cat("added:", obj, "\n")
  }
  if (type == "LEXICON") {
    assign("LEXICONS", value = listed, pos = 1)
    save(LEXICONS, file = "data/LEXICONS.rda", compress = 'xz')
  }
  else if (type == "VALENCE") {
    assign("VALENCE", value = listed, pos = 1)
    save(VALENCE, file = "data/VALENCE.rda", compress = 'xz')
  }
}

form_word_list(type = "LEXICON")
# load("data/LEXICONS.rda")

######################### VALENCE WORD LISTS

# get negators from lexicon package
negators <- lexicon::hash_valence_shifters[y == 1]
negators$y <- as.numeric(negators$y)
negators$y <- -1
write.csv2(negators, file = "data-raw/valence-raw/NEGATORS.csv", row.names = FALSE)

typeV <- "valence"
v <- prepare_word_list("NEGATORS.csv", typeV, "NEGATORS_ENG"); "NEGATORS_ENG" <- v$w
save(NEGATORS_ENG, file = v$file)
v <- prepare_word_list("NEGATORS_fr.csv", typeV, "NEGATORS_FR_tr"); "NEGATORS_FR_tr" <- v$w
save(NEGATORS_FR_tr, file = v$file)
v <- prepare_word_list("NEGATORS_nl.csv", typeV, "NEGATORS_NL_tr"); "NEGATORS_NL_tr" <- v$w
save(NEGATORS_NL_tr, file = v$file)

form_word_list(type = "VALENCE")
# load("data/VALENCE.rda")

