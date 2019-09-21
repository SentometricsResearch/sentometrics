
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

... and have some fun! You might also want to have a look at the [**`sentometrics.app`**](https://github.com/sborms/sentometrics.app) package. Its `sento_app()` function embeds a Shiny application that displays many of **`sentometrics`**' functionalities. Enjoy!

## Media

<p float="left">
<iframe width="410" height="270" src="https://www.youtube.com/embed/KC8LSBNvZrQ" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>
<iframe width="410" height="270" src="https://www.youtube.com/embed/nAlHzz4CP9E" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>
</p>

## Contact

Reach out to [Samuel Borms](mailto:samuel.borms@unine.ch) if you have questions, suggestions or want to help.

