
You collected a large number of texts and think it is a good idea to summarize your corpus into several textual sentiment time series, which you ponder could help predicting some variable you are interested in. However, you do not really know how to proceed next... Fortunately, you come across the **`sentometrics`** package, which does exactly what you need! Great!

## The R package **sentometrics**: What?

The **`sentometrics`** package is **an integrated framework for textual sentiment time series aggregation and prediction**. It accounts for the intrinsic challenge that, for a given text, sentiment can be computed in many different ways, as well as the large number of possibilities to pool sentiment across texts and time. This additional layer of manipulation does not exist in standard text mining and time series analysis packages. The package also provides an interface to econometrically model the impact of sentiment in texts on a given variable, as part of one coherent workflow. The package therefore integrates the fast _qualification_ of sentiment from texts, the _aggregation_ into different sentiment measures and the optimized _prediction_ based on these measures.

## Typical workflow

Below represent the typical **`sentometrics`** package workflow, and the associated output objects and functions. The steps are generally carried out linearly, but not necessarily so. For example, the model information from steps 4 and 5 can be exploited to further merge the sentiment measures in step 3. This serves as a broad taxonomy of what can be done and with which functions.

**Step 1**: Acquire and pre--process a selection of texts and generate relevant features

Functions: `sento_corpus()`, `add_features()`, and `to_sentocorpus()`. 

Outputs a _`sentocorpus`_ object.

**Step 2**: Choose lexicons and compute document--level textual sentiment

Functions: `sento_lexicons()`, `compute_sentiment()`, `to_sentiment()`, and `sentiment_bind()`.

Outputs a _`sentolexicons`_ object, and a _`sentiment`_ object or a sentiment scores `data.table`.

**Step 3**: Aggregate the sentiment into multiple textual sentiment time series

Functions: `ctr_agg()`, `sento_measures()`, `plot()`, `aggregate()`, `peakdocs()`, `diff()`, `scale()`, and `summary()`.

A bunch of functions to manipulate the sentiment measures are available (`measures_merge()`, `measures_global()`, `measures_subset()`, `measures_select()`, `measures_delete()`, and `measures_fill()`), as well as a number of convenient extractor functions (`nobs()`, `nmeasures()`, `get_dimensions()`, `get_dates()` and `get_measures()`). Make use of them to facilitate your analysis!

Outputs a _`sentomeasures`_ object.

**Step 4**: Calibrate (sparse) regression model and perform (out--of--sample) predictions

Functions: `ctr_model()`, and `sento_model()`.

Outputs a _`sentomodel`_ or a _`sentomodeliter`_ object.

**Step 5**: Evaluate prediction performance and retrieve sentiment attributions

Functions: `predict()`, `attributions()` (which outputs a _`attributions`_ object), `get_loss_data()`, `summary()`, and `plot()`.

## For you to read

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

## Media

<iframe width="560" height="315" src="https://www.youtube.com/embed/KC8LSBNvZrQ" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>

## Contact

Reach out to [Samuel Borms](mailto:samuel.borms@unine.ch) if you have questions, suggestions or want to help.

