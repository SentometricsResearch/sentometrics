
<a href='https://www.sentometrics.org'><img src='man/figures/logo.png' align="right" height="138.5"/></a>

## _sentometrics_: An Integrated Framework for Textual Sentiment Time Series Aggregation and Prediction

<!-- badges: start -->
[![CRAN](http://www.r-pkg.org/badges/version/sentometrics)](https://cran.r-project.org/package=sentometrics)
<!-- [![Build Status](https://travis-ci.org/SentometricsResearch/sentometrics.svg?branch=master)](https://travis-ci.org/SentometricsResearch/sentometrics) -->
<!-- [![codecov](https://codecov.io/github/SentometricsResearch/sentometrics/branch/master/graphs/badge.svg)](https://codecov.io/github/SentometricsResearch/sentometrics) -->
[![Downloads](http://cranlogs.r-pkg.org/badges/last-day/sentometrics?color=orange)](http://www.r-pkg.org/pkg/sentometrics)
[![Downloads](http://cranlogs.r-pkg.org/badges/sentometrics?color=orange)](http://www.r-pkg.org/pkg/sentometrics)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/sentometrics?color=orange)](http://www.r-pkg.org/pkg/sentometrics)
<!-- [![Pending Pull-Requests](http://githubbadges.herokuapp.com/SentometricsResearch/sentometrics/pulls.svg?style=flat)](https://github.com/SentometricsResearch/sentometrics/pulls) -->
<!-- [![Github Issues](http://githubbadges.herokuapp.com/SentometricsResearch/sentometrics/issues.svg)](https://github.com/SentometricsResearch/sentometrics/issues) -->
<!-- badges: end -->

### Introduction

The **`sentometrics`** package is an **integrated framework for textual sentiment time series aggregation and prediction**. It accounts for the intrinsic challenge that textual sentiment can be computed in many different ways, as well as the large number of possibilities to pool sentiment into a time series index. The package integrates the fast _quantification_ of sentiment from texts, the _aggregation_ into different sentiment time series, and the _prediction_ based on these measures. All in one coherent workflow!

See the [package website](https://SentometricsResearch.github.io/sentometrics) and the [vignette](https://doi.org/10.2139/ssrn.3067734) for plenty of examples and details. We also refer to this [survey](https://doi.org/10.1111/joes.12370) organized as an overview of the required steps in a typical econometric analysis of sentiment from alternative (such as textual) data, and following companion [web page](https://sborms.github.io/econometrics-meets-sentiment).

### Installation

To install the package from CRAN, simply do:

```R
install.packages("sentometrics")
```

To install the latest development version of **`sentometrics`** (which may contain bugs!), execute:

```R
devtools::install_github("SentometricsResearch/sentometrics")
```

### Shiny application

For a visual interface as a Shiny application of the package's core functionalities, install the [**`sentometrics.app`**](https://github.com/SentometricsResearch/sentometrics.app) package, and run the `sento_app()` function.

### Reference

Please cite **`sentometrics`** in publications. Use `citation("sentometrics")`.

### Acknowledgements

This software package originates from a
[Google Summer of Code 2017](https://github.com/rstats-gsoc/gsoc2017/wiki/Sentometrics:-An-integrated-framework-for-text-based-multivariate-time-series-modeling-and-forecasting) project, was further developed 
during a follow-up [Google Summer of Code 2019](https://github.com/rstats-gsoc/gsoc2019/wiki/sentometrics) project, and benefited generally from financial support by [Innoviris](https://innoviris.brussels), [IVADO](https://www.ivado.ca), [swissuniversities](https://www.swissuniversities.ch), and the [Swiss National Science Foundation](http://www.snf.ch) (grants #179281 and #191730).

