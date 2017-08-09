
############################################################
################## Data sets manipulation ##################
############################################################

######################### US ECONOMY TEXTS CORPUS (1951-2014)

useconomy <- readr::read_csv("data-raw/US_economic_news_1951-2014.csv")
useconomy$text <- stringi::stri_replace_all(useconomy$text, replacement = " ", regex = "</br></br>")
useconomy$text <- stringi::stri_replace_all(useconomy$text, replacement = "", regex = '[\\"]')
# useconomy$text <- stringi::stri_replace_all(useconomy$text, replacement = "", regex = "[%#*<=>@^_`|~{}�]")
useconomy$text <- stringi::stri_replace_all(useconomy$text, replacement = "", regex = "[^-a-zA-Z0-9,&. ]")

months <- lapply(stringi::stri_split(useconomy$date, regex = "/"), "[", 1)
days <- lapply(stringi::stri_split(useconomy$date, regex = "/"), "[", 2)
years <- lapply(stringi::stri_split(useconomy$date, regex = "/"), "[", 3)
yearsLong <- lapply(years, function(x) if (as.numeric(x) > 14) return(paste0("19", x)) else return(paste0("20", x)))
datesNew <- paste0(paste0(unlist(months), "/"), paste0(unlist(days), "/"), unlist(yearsLong))
datesNew <- as.character(as.Date(datesNew, format = "%m/%d/%Y")) # character date is input requirement
useconomy$dateNew <- datesNew

freqDays <- plyr::count(datesNew)
freqMonths <- plyr::count(unlist(lapply(stringi::stri_split(datesNew, regex = "-"),
                                        function(x) return(paste0(x[1:2], collapse = "/")))))
freqYears <- plyr::count(unlist(lapply(stringi::stri_split(datesNew, regex = "-"), "[", 1)))

USECONOMYNEWS <- useconomy
USECONOMYNEWS$date <- USECONOMYNEWS$dateNew

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
USECONOMYNEWS$dateNew <- USECONOMYNEWS$`_last_judgment_at` <- USECONOMYNEWS$`_trusted_judgments` <-
USECONOMYNEWS$`positivity:confidence` <- USECONOMYNEWS$`relevance:confidence` <- USECONOMYNEWS$relevance_gold <-
USECONOMYNEWS$articleid <- USECONOMYNEWS$`_unit_state` <- USECONOMYNEWS$`_golden` <- USECONOMYNEWS$positivity_gold <-
USECONOMYNEWS$relevance <- USECONOMYNEWS$positivity <- NULL

USECONOMYNEWS <- data.table::as.data.table(USECONOMYNEWS)
USECONOMYNEWS <- USECONOMYNEWS[order(date)]
colnames(USECONOMYNEWS)[1] <- "id"
USECONOMYNEWS <- subset(USECONOMYNEWS, date >= "1980-01-01") # drop all before 1980
setcolorder(USECONOMYNEWS, c("id", "date", "text", "headline", "wsj", "wapo", "economy", "noneconomy"))
useconomynews <- USECONOMYNEWS # back to lowercase before saving

save(useconomynews, file = "data/useconomynews.rda", compress = 'xz')
# load("data/useconomynews.rda")

######################### S&P500 Index (1988-2014, monthly)

# makes returns in line with USECONOMYNEWS corpus
# "1988-03-01" means monthly return from March to April 1988, such that it aligns with all articles in March

sp500 <- readr::read_csv("data-raw/S&P500.csv")
sp500 <- as.data.frame(sp500[c("Date", "Adj Close")])
sp500 <- sp500[sp500$Date <= "2015-01-01", ]
sp500 <- xts::xts(sp500$`Adj Close`[-1], sp500$Date[-nrow(sp500)])
sp500 <- PerformanceAnalytics::Return.calculate(sp500)
sp500 <- sp500[-1, ]
sp500 <- data.frame(date = zoo::index(sp500), return = as.numeric(sp500))

save(sp500, file = "data/sp500.rda", compress = 'xz')

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
  if (type == "lexicons") colnames(w) <- c("x", "y")
  else colnames(w) <- c("x", "t", "y")

  # cleaning
  w$x <- as.character(w$x)
  w$x <- stringr::str_to_lower(w$x)
  w$x <- stringr::str_trim(w$x)
  w$x <- stringi::stri_replace_all(w$x, "", regex = '\\?') # remove question markets
  w$x <- stringi::stri_replace_all(w$x, "", regex = '[\\"]') # remove double quotes
  w$x <- stringi::stri_replace_all(w$x, "", regex = '[()]') # remove single brackets
  w$x <- stringi::stri_replace_all(w$x, "", regex = '[@]') # remove @ character
  w$y <- as.numeric(w$y)
  if (type == "valence") w$t <- as.numeric(w$t)

  # change to as_key() format
  w <- w[!duplicated(w$x), ]
  if (type != "valence") {
    w <- sentimentr::as_key(w, comparison = NULL) # makes absolutely sure duplicated words are removed
  }
  w <- w[w$x != "" & w$x != " " & w$x != "#naam", ]

  fileOut <- paste0("data-raw/", name, ".rda")

  return(list(w = w, file = fileOut))
}

# save lexicons with appropriate names to data-raw/ folder
typeL <- "lexicons"
l <- prepare_word_list("LM.csv", typeL, "LM_eng"); "LM_eng" <- l$w
save(LM_eng, file = l$file)
l <- prepare_word_list("LM_fr.csv", typeL, "LM_fr_tr"); "LM_fr_tr" <- l$w
save(LM_fr_tr, file = l$file)
l <- prepare_word_list("LM_nl.csv", typeL, "LM_nl_tr"); "LM_nl_tr" <- l$w
save(LM_nl_tr, file = l$file)
l <- prepare_word_list("FEEL.csv", typeL, "FEEL_fr"); "FEEL_fr" <- l$w
save(FEEL_fr, file = l$file)
l <- prepare_word_list("FEEL_nl.csv", typeL, "FEEL_nl_tr"); "FEEL_nl_tr" <- l$w
save(FEEL_nl_tr, file = l$file)
l <- prepare_word_list("FEEL_eng.csv", typeL, "FEEL_eng_tr"); "FEEL_eng_tr" <- l$w
save(FEEL_eng_tr, file = l$file)
l <- prepare_word_list("GI.csv", typeL,"GI_eng"); "GI_eng" <- l$w
save(GI_eng, file = l$file)
l <- prepare_word_list("GI_fr.csv", typeL, "GI_fr_tr"); "GI_fr_tr" <- l$w
save(GI_fr_tr, file = l$file)
l <- prepare_word_list("GI_nl.csv", typeL, "GI_nl_tr"); "GI_nl_tr" <- l$w
save(GI_nl_tr, file = l$file)
l <- prepare_word_list("HENRY.csv", typeL, "HENRY_eng"); "HENRY_eng" <- l$w
save(HENRY_eng, file = l$file)
l <- prepare_word_list("HENRY_fr.csv", typeL, "HENRY_fr_tr"); "HENRY_fr_tr" <- l$w
save(HENRY_fr_tr, file = l$file)
l <- prepare_word_list("HENRY_nl.csv", typeL, "HENRY_nl_tr"); "HENRY_nl_tr" <- l$w
save(HENRY_nl_tr, file = l$file)

# creates and places LEXICON data in data folder
form_word_list <- function(type) {

  if (type == "lexicon") pattern <- paste0(c("FEEL", "GI", "HENRY", "LM"), collapse = "|")
  else if (type == "valence") pattern <- c("valence_") # change if additional valence shifter categories are added

  objects <- list.files("data-raw/", pattern = pattern)

  listed <- vector(mode = "list")
  for (o in 1:length(objects)) {
    obj <- load(paste0("data-raw/", objects[o])) # name
    toAdd <- eval(parse(text = obj)) # object
    listed[[obj]] <- toAdd
    cat("added:", obj, "\n")
  }
  if (type == "lexicon") {
    assign("lexicons", value = listed, pos = 1)
    save(lexicons, file = "data/lexicons.rda", compress = 'xz')
  }
  else if (type == "valence") {
    assign("valence", value = listed, pos = 1)
    save(valence, file = "data/valence.rda", compress = 'xz')
  }
}

form_word_list(type = "lexicon")
# load("data/lexicons.rda")

######################### VALENCE WORD LISTS

# get negators from lexicon package
negators <- lexicon::hash_valence_shifters[y == 1]
negators$y <- as.numeric(negators$y)
negators$y <- -1
write.csv2(negators, file = "data-raw/valence-raw/NEGATORS.csv", row.names = FALSE)

typeV <- "valence"
v <- prepare_word_list("NEGATORS.csv", typeV, "valence_eng"); "valence_eng" <- v$w
save(valence_eng, file = v$file)
v <- prepare_word_list("NEGATORS_fr.csv", typeV, "valence_fr"); "valence_fr" <- v$w
save(valence_fr, file = v$file)
v <- prepare_word_list("NEGATORS_nl.csv", typeV, "valence_nl"); "valence_nl" <- v$w
save(valence_nl, file = v$file)

form_word_list(type = "valence")
# load("data/valence.rda")

