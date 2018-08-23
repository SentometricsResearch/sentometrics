
You collected a large number of texts and think it is a good idea to summarize your corpus into several textual sentiment time series, which you ponder could help predicting some variable you are interested in. However, you do not really know how to proceed next... Fortunately, you come across the **`sentometrics`** package, which does exactly what you need! Great!

## The R package **sentometrics**: What?

The **`sentometrics`** package is **an integrated framework for textual sentiment time series aggregation and prediction**. It accounts for the intrinsic challenge that, for a given text, sentiment can be computed in many different ways, as well as the large number of possibilities to pool sentiment across texts and time. This additional layer of manipulation does not exist in standard text mining and time series analysis packages. The package also provides an interface to econometrically model the impact of sentiment in texts on a given variable, as part of one coherent workflow. The package therefore integrates the _qualification_ of sentiment from texts, the _aggregation_ into different sentiment measures and the optimized _prediction_ based on these measures.

The package was first created during [Google Summer of Code 2017](https://github.com/rstats-gsoc/gsoc2017/wiki/Sentometrics:-An-integrated-framework-for-text-based-multivariate-time-series-modeling-and-forecasting). It implements the main methodology developed in the paper "[Questioning the news about economic growth: Sparse forecasting using thousands of news--based sentiment values](https://ssrn.com/abstract=2976084)" (Ardia, Bluteau and Boudt, 2017) in a user--friendly way.

## Typical **sentometrics** workflow

Below series of steps represent a typical **`sentometrics`** package workflow, and the associated output objects and functions. The steps are generally carried out one after the other, but not necessarily so. For example, the model information from steps 4 and 5 can be exploited to further merge the sentiment measures in step 3. This serves as a broad taxonomy of what can be done and with which functions.

**Step 1**: Acquire and pre--process a selection of texts and generate relevant features
    
**_Functions:_** `sento_corpus()`, `add_features()`, and `to_sentocorpus()`.

**_Output:_** a `sentocorpus` object.

**Step 2**: Choose lexicons and compute document--level textual sentiment
    
**_Functions:_** `setup_lexicons()`, and `compute_sentiment()`.

**Step 3**: Aggregate the sentiment into multiple textual sentiment time series
    
**_Functions:_** `ctr_agg()`, `sento_measures()`, `plot()`, `perform_agg()`, `diff()`, `scale()`, and `summary()`.

A bunch of functions to manipulate the sentiment measures are available (`measures_merge()`, `measures_global()`, `measures_subset()`, `measures_select()`, `measures_delete()`, and `measures_fill()`), as well as a number of convenient extractor functions (`nobs()`, `nmeasures()`, `get_dimensions()`, and `get_dates()`). Make use of them to facilitate your analysis!

**_Output:_** a `sentomeasures` object.

**Step 4**: Calibrate (sparse) regression model and perform (out--of--sample) predictions
    
**_Functions:_** `ctr_model()`, and `sento_model()`.

**_Output:_** a `sentomodel` or a `sentomodeliter` object.

**Step 5**: Evaluate prediction performance and retrieve sentiment attributions
    
**_Functions:_** `predict()`, `retrieve_attributions()`, `plot_attributions()`, `peakdocs()`, `perform_MCS()`, `summary()`, and `plot()`.

## For you to read

A hands--on methodological introduction is given in our [vignette](https://ssrn.com/abstract=3067734), with an accompanying code example (see the `run_vignette.R` script under the _/examples_ folder on our GitHub repository). Reading it will help you understand the ins and outs of the software. The complete documentation can be found on CRAN's [sentometrics](https://CRAN.R-project.org/package=sentometrics) page. 

## Installation

To install the package from CRAN, simply do:

```R
install.packages("sentometrics")
```

The latest development version of **`sentometrics`** resides at [https://github.com/sborms/sentometrics](https://github.com/sborms/sentometrics). To install this version (which may still contain bugs!), execute:

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

We will continue to improve the **`sentometrics`** package, in order for you to ever more smoothly analyse the role of sentiment in texts on your variable(s) of interest! Reach out to the development team if you have questions, suggestions or want to help: [Samuel Borms](mailto:samuel.borms@unine.ch), [Keven Bluteau](mailto:keven.bluteau@unine.ch), [David Ardia](mailto:david.ardia@unine.ch) or [Kris Boudt](mailto:kris.boudt@vub.be).

