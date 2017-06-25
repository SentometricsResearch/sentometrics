
require(tm)
require(qdapDictionaries)
require(qdap)
require(RWeka)

load("DATA/example_corpus.rda")
load("LEXICON/negativeDiction_McDonald.rda")
load("LEXICON/positiveDiction_McDonald.rda")

weighting = FALSE

data(negation.words)

str_negate <- function(x) {

  for(i in 1:length(negation.words)) {
    x = gsub(paste0(negation.words[i], " "), "not_", gsub("n't ", "n't not_", x))
  }

  return(x)
}

# Pre-process corpus
corpus = tm::tm_map(corpus, tm::content_transformer(tolower)) # change to lowercase
corpus = tm::tm_map(corpus, tm::stripWhitespace) # strip extra white space

# Sort the corpus from the farthest in time to the newest
timelist = lapply(corpus, meta, tag = "datetimestamp")
timedf = data.frame(time = as.character(timelist))
timelistorder = order(timedf$time)
corpus = corpus[timelistorder]

corpus = corpus[1:100] # speeding up for testing
timelist = lapply(corpus, meta, tag = "datetimestamp")

BigramTokenizer = function(x) RWeka::NGramTokenizer(x, Weka_control(min = 1, max = 1))
tdmControl = list(tokenize = BigramTokenizer, wordLengths = c(1, Inf))
if(weighting == "weightTfIdf"){
  tdmControl$weighting =   function(x) weightTfIdf(x, normalize = FALSE)
}

# Negating
for(i in 1:length(corpus)){
  corpus[[i]]$content = str_negate(corpus[[i]]$content)
}

# Modify lexicons for negation
positiveDiction = c(positiveDiction, paste0("not_", negativeDiction))
negativeDiction = c(negativeDiction, paste0("not_", positiveDiction))

# Term document matrix calculation
tdm = tm::TermDocumentMatrix(corpus, control = tdmControl)
tdm = tm::removeSparseTerms(tdm, 0.99999)
dtm = DocumentTermMatrix(corpus)
nwords = rowSums(as.matrix(dtm))

# Sentiment score
positiveScore = tm::tm_term_score(tdm, positiveDiction)
negativeScore = tm::tm_term_score(tdm, negativeDiction)

# Extract topics
Sub = lapply(corpus, meta, tag = "SUBJECT")
subjectList = c("ECONOMIC CONDITIONS",
                "CURRENCIES",
                "INTEREST RATES",
                "ECONOMIC POLICY",
                "GROSS DOMESTIC PRODUCT",
                "OUTPUT & DEMAND",
                "PRICES",
                "ECONOMIC GROWTH",
                "INFLATION",
                "RECESSION",
                "TRENDS",
                "EXPORT TRADE",
                "GERMAN CHANCELLORS",
                "MANUFACTURING OUTPUT",
                "COMPANY PROFITS",
                "UNEMPLOYMENT RATES",
                "REAL ESTATE",
                "COMPANY EARNINGS",
                "INTERNATIONAL TRADE",
                "BOND MARKETS",
                "BUDGET DEFICITS",
                "SALES FIGURES",
                "PRICE INCREASES",
                "OIL & GAS PRICES",
                "IMPORT TRADE",
                "CONSUMPTION",
                "EMPLOYMENT GROWTH",
                "ECONOMIC DECLINE",
                "JOB CREATION",
                "WAGES & SALARIES",
                "OIL & GAS INDUSTRY",
                "MANUFACTURING FACILITIES",
                "DEBT CRISIS",
                "RETAILERS",
                "CONSUMER CONFIDENCE",
                "CONSTRUCTION",
                "PRICE CHANGES",
                "BUSINESS CONFIDENCE",
                "HOUSING MARKET",
                "BUSINESS CLIMATE & CONDITIONS",
                "ECONOMIC STIMULUS",
                "UTILITIES INDUSTRY",
                "COMMODITIES PRICES",
                "HOME PRICES",
                "RETAIL SECTOR PERFORMANCE",
                "EMPLOYMENT",
                "ECONOMIC SURVEYS")
sublist = matrix(NA, nrow = length(corpus), ncol = length(subjectList))

for(i in 1:length(subjectList)) {
  sublist[, i] = as.numeric(as.character(lapply(Sub, FUN = function(x) {

    if(any(!is.na(x))) {
      return(as.numeric(as.character(x[x[, 1] == subjectList[i], 2])))
    } else {
      return(0)
    }

  })))
}
colnames(sublist) = subjectList

# Extract origin
origin = lapply(corpus, meta, tag = "origin")
origin[sapply(origin, length) == 0] <- NA
origin = unlist(origin)

sentiment = data.frame(date = unlist(timelist),
                       positiveScore = positiveScore,
                       negativeScore = negativeScore,
                       origin = origin,
                       nwords = nwords)

sentiment = cbind(sentiment, sublist)

# Note that there are multiple articles per day. The next step is to merge per topic to obtain topic/daily sentiment measures.
# The number appearing in each row of one of the topic columns is the probability in percent that the document is from that topic.

