
You collected a large number of texts and think it is a good idea to summarize your corpus into several textual sentiment time series, which you ponder could help predicting some variable you are interested in. However, you do not really know how to proceed next... Fortunately, you come across the **`sentometrics`** package, which does exactly what you need! Great!

## The R package **sentometrics**: What?

The **`sentometrics`** package is **an integrated framework for textual sentiment time series aggregation and prediction**. It accounts for the intrinsic challenge that, for a given text, sentiment can be computed in many different ways, as well as the large number of possibilities to pool sentiment across texts and time. This additional layer of manipulation does not exist in standard text mining and time series analysis packages. The package also provides an interface to econometrically model the impact of sentiment in texts on a given variable, as part of one coherent workflow. The package therefore integrates the fast _quantification_ of sentiment from texts, the _aggregation_ into different sentiment measures and the optimized _prediction_ based on these measures.

### For you to read

Our [vignette](https://ssrn.com/abstract=3067734) explains the ins and outs of the software package, and has an accompanying code example (see the `run_vignette.R` script under the _/examples_ folder on our GitHub repository). The complete documentation can be found on CRAN's [sentometrics](https://CRAN.R-project.org/package=sentometrics) page. 

## Installation

To install the latest package version from CRAN, simply do:

```R
install.packages("sentometrics")
```

The latest development version of **`sentometrics`** resides on the GitHub repository. To install this version (which may still contain bugs!), execute:

```R
devtools::install_github("sborms/sentometrics")
```

When installed, you are ready to load the package...

```R
library("sentometrics")
```

... and have some fun! 

## Many and more examples

### Example 1

A simple calculation of sentiment. The score is a substraction of the number of negative lexicon words (those with a score of -1) from the number of positive lexicon words (those with a score of 1).

```R
library("sentometrics")

data("usnews")

s <- compute_sentiment(
  sentometrics::usnews[["texts"]],
  sento_lexicons(sentometrics::list_lexicons[c("GI_en", "LM_en")]),
  how = "counts"
)
```

### Example 2

The same simple calculation as in Example 1, but using a `sento_corpus` object.

```R
library("sentometrics")

data("usnews")

corpus <- sento_corpus(usnews)
lexicons <- sento_lexicons(sentometrics::list_lexicons[c("GI_en", "LM_en", "HENRY_en")])

s <- compute_sentiment(corpus, lexicons, how = "counts")
```

### Example 3

Again, a simple textual sentiment calculation, but this time using a **`tm`** package corpus object. Super flexible! The output is this time slightly different, as the scores are divided by the total number of words.

```R
library("sentometrics")
library("tm")

data("usnews")

corpus <- SimpleCorpus(VectorSource(usnews[["texts"]]))
lexicons <- sento_lexicons(sentometrics::list_lexicons[c("GI_en", "LM_en", "HENRY_en")])

s <- compute_sentiment(corpus, lexicons, how = "proportional")
```

### Example 4

Even more flexibility in this example! You tokenize your corpus outside the sentiment computation function call, so you control exactly which words the lexicons are going to look into.

```R
library("sentometrics")
library("quanteda")

data("usnews")

corpus <- sento_corpus(usnews)
lexicons <- sento_lexicons(sentometrics::list_lexicons[c("GI_en", "LM_en", "HENRY_en")])

tks <- as.list(tokens(corpus, what = "fastestword"))

s <- compute_sentiment(corpus, lexicons, how = "counts", tokens = tks)
```

### Example 5

A textual sentiment computation on sentence-level, starting from a document-level corpus. Subsequently, the sentence-level scores are aggregated into document-level scores.

```R
library("sentometrics")
library("quanteda")

data("usnews")

corpus <- sento_corpus(usnews[, 1:3])

s <- compute_sentiment(
  corpus,
  sento_lexicons(sentometrics::list_lexicons[c("GI_en", "LM_en")]),
  how = "proportionalPol",
  do.sentence = TRUE
)

sDocs <- aggregate(s, ctr_agg(howDocs = "proportional"), do.full = FALSE)
```

From these sentiment scores, we find and display the 7 documents where most positive sentiment scores were detected.

```R
peakDocsPos <- peakdocs(sDocs, n = 7, type = "pos")
corpusPeaks <- corpus_subset(corpus, docnames(corpus) %in% peakDocsPos)
texts(corpusPeaks)
```

### Example 6

To aggregate document-level sentiment scores into time series only requires to specificy a few parameters regarding the weighting and the time frequency.

```R
library("sentometrics")

data("usnews")

corpus <- sento_corpus(usnews)

s <- compute_sentiment(
  corpus,
  sento_lexicons(sentometrics::list_lexicons[c("GI_en", "LM_en")]),
  how = "counts"
)

ctr <- ctr_agg(howDocs = "proportional",
               howTime = "equal_weight", by = "month", lag = 6)
measures <- aggregate(s, ctr)
```

The obtained measures can be plotted, all of them, or according to the three time series dimensions (the corpus features, the lexicons and the time weighting schemes).

```R
plot(measures)
plot(measures, "features")
plot(measures, "lexicons")
plot(measures, "time")
```

### Example 7

The aggregation into sentiment time series does not have to be done in two steps. Below one-step approach is recommended because very easy!

```R
library("sentometrics")
library("data.table")

data("usnews")

corpus <- sento_corpus(usnews)
lexicons <- sento_lexicons(sentometrics::list_lexicons[c("GI_en", "LM_en", "HENRY_en")])
ctr <- ctr_agg(howWithin = "counts",
               howDocs = "proportional",
               howTime = "linear", by = "week", lag = 14)

measures <- sento_measures(corpus, lexicons, ctr)
```

Similar to extracting the peak documents in a sentiment table, we extract here the peak dates in the sentiment time series matrix. Detected below are the 5 dates where average sentiment across all sentiment measures is lowest.

```R
peakDatesNeg <- peakdates(measures, n = 5, type = "neg", do.average = TRUE)
dtPeaks <- as.data.table(subset(measures, date %in% peakDatesNeg))
```

### Example 8

Sentiment measures can be further aggregated across any of the three dimensions. The computed time series are averages across the relevant measures.

```R
library("sentometrics")

data("usnews")

corpus <- sento_corpus(usnews)
lexicons <- sento_lexicons(sentometrics::list_lexicons[c("GI_en", "LM_en", "HENRY_en")])
ctr <- ctr_agg(howWithin = "counts",
               howDocs = "proportional",
               howTime = c("linear", "equal_weight"), by = "week", lag = 14)

measures <- sento_measures(corpus, lexicons, ctr)

measuresAgg <- aggregate(measures,
                         features = list("journal" = c("wsj", "wapo")),
                         time = list("linequal" = c("linear", "equal_weight")))

get_dimensions(measuresAgg) # inspect the contents of the three dimensions
```

### Example 9

To keep it at its simplest, all the sentiment measures computed can be condensed in a few global sentiment time series. The computation can be run in a weighted way as well.  

```R
library("sentometrics")
library("ggplot2")

data("usnews")

corpus <- sento_corpus(usnews)
lexicons <- sento_lexicons(sentometrics::list_lexicons[c("GI_en", "LM_en", "HENRY_en")])
ctr <- ctr_agg(howWithin = "counts",
               howDocs = "proportional",
               howTime = c("linear", "equal_weight"), by = "week", lag = 14)

measures <- sento_measures(corpus, lexicons, ctr)

measuresGlobal <- aggregate(measures, do.global = TRUE)
```

The output in this case is not a specific **`sentometrics`** _`sento_measures`_ object, but simply a _`data.table`_. Below produces a nice plot using the **`ggplot2`** package.

```R
ggplot(melt(measuresGlobal, id.vars = "date")) +
  aes(x = date, y = value, color = variable) +
  geom_line() +
  scale_x_date(name = "Date", date_labels = "%m-%Y") +
  scale_y_continuous(name = "Sentiment") +
  theme_bw() + 
  sentometrics:::plot_theme(legendPos = "top") # a small trick to finetune the plotting display
```

## Shiny application

You might also want to have a look at the [**`sentometrics.app`**](https://github.com/sborms/sentometrics.app) package. Its `sento_app()` function embeds a Shiny application that displays many of **`sentometrics`**' functionalities. Enjoy!

## Media

<p float="left">
<iframe width="410" height="270" src="https://www.youtube.com/embed/KC8LSBNvZrQ" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>
<iframe width="410" height="270" src="https://www.youtube.com/embed/nAlHzz4CP9E" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>
</p>

## Contact

Reach out to [Samuel Borms](mailto:samuel.borms@unine.ch) if you have questions, suggestions or want to help.

