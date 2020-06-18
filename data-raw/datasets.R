
############################################################
################## Data sets manipulation ##################
############################################################

######################### US ECONOMY TEXTS CORPUS (1951-2014)

useconomy <- readr::read_csv("data-raw/US_economic_news_1951-2014.csv")
useconomy$texts <- stringi::stri_replace_all(useconomy$text, replacement = " ", regex = "</br></br>")
useconomy$texts <- stringi::stri_replace_all(useconomy$texts, replacement = "", regex = '[\\"]')
# useconomy$texts <- stringi::stri_replace_all(useconomy$texts, replacement = "", regex = "[%#*<=>@^_`|~{}�]")
useconomy$texts <- stringi::stri_replace_all(useconomy$texts, replacement = "", regex = "[^-a-zA-Z0-9,&.' ]")
useconomy$text <- NULL

months <- lapply(stringi::stri_split(useconomy$date, regex = "/"), "[", 1)
days <- lapply(stringi::stri_split(useconomy$date, regex = "/"), "[", 2)
years <- lapply(stringi::stri_split(useconomy$date, regex = "/"), "[", 3)
yearsLong <- lapply(years, function(x) if (as.numeric(x) > 14) return(paste0("19", x)) else return(paste0("20", x)))
datesNew <- paste0(paste0(unlist(months), "/"), paste0(unlist(days), "/"), unlist(yearsLong))
datesNew <- as.character(as.Date(datesNew, format = "%m/%d/%Y"))
useconomy$dateNew <- datesNew

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
USECONOMYNEWS$relevance <- USECONOMYNEWS$positivity <- USECONOMYNEWS$headline <- NULL

USECONOMYNEWS <- data.table::as.data.table(USECONOMYNEWS)
USECONOMYNEWS <- USECONOMYNEWS[order(date)]
colnames(USECONOMYNEWS)[1] <- "id"
USECONOMYNEWS <- subset(USECONOMYNEWS, date >= "1995-01-01") # limit scope
setcolorder(USECONOMYNEWS, c("id", "date", "texts", "wsj", "wapo", "economy", "noneconomy"))
useconomynews <- as.data.frame(USECONOMYNEWS) # back to lowercase before saving
useconomynews$id <- as.character(useconomynews$id)
usnews <- useconomynews

save(usnews, file = "data/usnews.rda", compress = 'xz')
# load("data/usnews.rda")

######################### Economic Policy Uncertainty Index (1985-2018, monthly)

epu <- readr::read_csv2("data-raw/US_EPU_1985-2018.csv")
epu$date <- as.Date(paste0(epu$Year, "-", epu$Month, "-01"))
epu$Year <- epu$Month <- NULL
colnames(epu)[1] <- "epu"
epu <- epu[c("date", "epu")]
epu <- as.data.frame(epu[epu$date >= "1985-01-01" & epu$date <= "2018-07-01", ])
plot(epu$epu, type = "l")

yb <- ifelse(epu$epu >= mean(epu$epu), 1, -1)
yb <- as.factor(yb)
levels(yb) <- c("below", "above") # binomial example series

ym <- epu$epu
ym[ym >= quantile(epu$epu)[3] & ym < quantile(epu$epu)[4]] <- 1
ym[ym >= quantile(epu$epu)[4] & ym != 1] <- 2
ym[ym <= quantile(epu$epu)[3] & ym > quantile(epu$epu)[2] & ym != 1 & ym != 2] <- -1
ym[ym != -1 & ym != 1 & ym != 2] <- -2
ym <- as.factor(ym)
levels(ym) <- c("below-", "below", "above", "above+") # multinomial example series

epu <- data.frame(date = epu$date, index = epu$epu, above = yb, aboveMulti = ym)
save(epu, file = "data/epu.rda", compress = 'xz')

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
f <- read.csv("data-raw/lexicons-raw/FEEL_en.csv", sep = ";")
w <- as.character(f$word[f$word != ""])
p <- f$polarity[!is.na(f$polarity)]
new <- data.frame(word = w, polarity = p)
write.csv2(new, file = "data-raw/lexicons-raw/FEEL_en.csv", row.names = FALSE)

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

  loc <- paste0("data-raw/", type, "-raw/", fileName)
  w <- read.csv(loc, sep = ";")

  # confirm if translation occured and retranslation
  stripped <- unlist(stringi::stri_split(fileName, regex = "_"))
  if (length(stripped) > 1) {
    original <- paste0(stripped[1], ".csv")
    ww <- read.csv(paste0("data-raw/", type, "-raw/", original), sep = ";")
    if (type == "lexicons") {
      keep <- list(
        (as.character(w$Retranslation) == as.character(ww[, 1])), # words are in first column of original
        (as.character(w[, 1]) != as.character(ww[, 1])) # high likelihood of actual translation
      )
      toKeep <- keep[[1]] == keep[[2]]
      w <- w[toKeep, -NCOL(w)]
    } else if (type == "valence") {
      toKeep <- (as.character(w[, 1]) != as.character(ww[, 1]))
      w <- w[toKeep, ]
    }
    print(paste0("kept: ", sum(toKeep)/length(toKeep)))
  }

  if (type == "lexicons") colnames(w) <- c("x", "y")
  else colnames(w) <- c("x", "t", "y")

  # cleaning
  w$x <- as.character(w$x)
  w$x <- stringi::stri_trans_tolower(w$x)
  w$x <- stringi::str_trim(w$x)
  w$x <- stringi::stri_replace_all(w$x, "", regex = '\\?') # remove question markets
  w$x <- stringi::stri_replace_all(w$x, "", regex = '[\\"]') # remove double quotes
  w$x <- stringi::stri_replace_all(w$x, "", regex = '[()]') # remove single brackets
  w$x <- stringi::stri_replace_all(w$x, "", regex = '[@]') # remove @ character
  w$y <- as.numeric(w$y)
  if (type == "valence") w$t <- as.numeric(w$t)

  # change to as_key() format
  w <- w[!duplicated(w$x), ]
  if (type == "lexicons") {
    w <- sentometrics:::sento_as_key(w) # makes absolutely sure duplicated words are removed
  }
  w <- w[w$x != "" & w$x != " " & w$x != "#naam", ]
  w <- w[!stringi::stri_detect(w$x, regex = "\\s+"), ]

  fileOut <- paste0("data-raw/", name, ".rda")

  return(list(w = w, file = fileOut))
}

# save lexicons with appropriate names to data-raw/ folder
typeL <- "lexicons"
l <- prepare_word_list("LM.csv", typeL, "LM_en"); "LM_en" <- l$w
save(LM_en, file = l$file)
l <- prepare_word_list("LM_fr.csv", typeL, "LM_fr_tr"); "LM_fr_tr" <- l$w
save(LM_fr_tr, file = l$file)
l <- prepare_word_list("LM_nl.csv", typeL, "LM_nl_tr"); "LM_nl_tr" <- l$w
save(LM_nl_tr, file = l$file)
l <- prepare_word_list("FEEL.csv", typeL, "FEEL_fr"); "FEEL_fr" <- l$w
save(FEEL_fr, file = l$file)
l <- prepare_word_list("FEEL_nl.csv", typeL, "FEEL_nl_tr"); "FEEL_nl_tr" <- l$w
save(FEEL_nl_tr, file = l$file)
l <- prepare_word_list("FEEL_en.csv", typeL, "FEEL_en_tr"); "FEEL_en_tr" <- l$w
save(FEEL_en_tr, file = l$file)
l <- prepare_word_list("GI.csv", typeL,"GI_en"); "GI_en" <- l$w
save(GI_en, file = l$file)
l <- prepare_word_list("GI_fr.csv", typeL, "GI_fr_tr"); "GI_fr_tr" <- l$w
save(GI_fr_tr, file = l$file)
l <- prepare_word_list("GI_nl.csv", typeL, "GI_nl_tr"); "GI_nl_tr" <- l$w
save(GI_nl_tr, file = l$file)
l <- prepare_word_list("HENRY.csv", typeL, "HENRY_en"); "HENRY_en" <- l$w
save(HENRY_en, file = l$file)
l <- prepare_word_list("HENRY_fr.csv", typeL, "HENRY_fr_tr"); "HENRY_fr_tr" <- l$w
save(HENRY_fr_tr, file = l$file)
l <- prepare_word_list("HENRY_nl.csv", typeL, "HENRY_nl_tr"); "HENRY_nl_tr" <- l$w
save(HENRY_nl_tr, file = l$file)

# creates and places LEXICON data in data folder
form_word_list <- function(type) {
  if (type == "lexicons") pattern <- paste0(c("FEEL", "GI", "HENRY", "LM"), collapse = "|")
  else if (type == "valence") pattern <- c("valence_") # change if additional valence shifter categories are added
  objects <- list.files("data-raw/", pattern = pattern)
  listed <- vector(mode = "list")
  for (o in 1:length(objects)) {
    obj <- load(paste0("data-raw/", objects[o])) # name
    toAdd <- eval(parse(text = obj)) # object
    listed[[obj]] <- toAdd
    cat("added:", obj, "\n")
  }
  if (type == "lexicons") {
    assign("lexicons", value = listed, pos = 1)
    save(lexicons, file = "data/lexicons.rda", compress = 'xz')
  }
  else if (type == "valence") {
    assign("valence", value = listed, pos = 1)
    save(valence, file = "data/valence.rda", compress = 'xz')
  }
}

form_word_list(type = "lexicons")

load("data/lexicons.rda")
list_lexicons <- lexicons
save(list_lexicons, file = "data/list_lexicons.rda")

######################### VALENCE WORD LISTS

load("data-raw/valence-raw/valShifters.rda")
names(valShifters) <- c("en", "fr", "nl")
valShifters <- lapply(valShifters, function(v) {
  # v <- v[t != 4]
  v$y <- -1
  v$t <- as.numeric(v$t)
  v[t == 2, "y"] <- 1.8
  v[t == 3, "y"] <- 0.2
  v$x <- as.character(v$x)
  Encoding(v$x) <- "latin1"
  v$x <- iconv(v$x, "latin1", "UTF-8")
  # v$t <- NULL
  v <- v[!stringi::stri_detect(v$x, regex = "\\s+"), ]
  setcolorder(v, c("x", "y", "t"))
  setkey(v, "x")
  v
})
list_valence_shifters <- valShifters

save(list_valence_shifters, file = "data/list_valence_shifters.rda", version = 2)

######################### ISO CODES TABLE

# iso <- na.omit(ISOcodes::ISO_639_2[, c("Alpha_2", "Name")])
# colnames(iso) <- c("code", "language")
# usethis::use_data(iso, internal = TRUE)

