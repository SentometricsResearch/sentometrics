
# sentometrics
## An Integrated Framework for Textual Sentiment Time Series Aggregation and Prediction

<!--- comment out when submitting to CRAN until CRAN/pandoc issues (e.g. handshake) solved --->
[![CRAN](http://www.r-pkg.org/badges/version/sentometrics)](https://cran.r-project.org/package=sentometrics)
[![Build Status](https://travis-ci.org/sborms/sentometrics.svg?branch=master)](https://travis-ci.org/sborms/sentometrics)
[![codecov](https://codecov.io/github/sborms/sentometrics/branch/master/graphs/badge.svg)](https://codecov.io/github/sborms/sentometrics)
[![Downloads](http://cranlogs.r-pkg.org/badges/sentometrics?color=brightgreen)](http://www.r-pkg.org/pkg/sentometrics)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/sentometrics?color=brightgreen)](http://www.r-pkg.org/pkg/sentometrics)
<!--- [![Pending Pull-Requests](http://githubbadges.herokuapp.com/sborms/sentometrics/pulls.svg?style=flat)](https://github.com/sborms/sentometrics/pulls) --->
<!--- [![Github Issues](http://githubbadges.herokuapp.com/sborms/sentometrics/issues.svg)](https://github.com/sborms/sentometrics/issues) --->

<a href='https://www.sentometrics.org'><img src='man/figures/logo.png' align="right" height="138.5"/></a>

### Introduction

The **`sentometrics`** package is **an integrated framework for textual sentiment time series aggregation and prediction**. It accounts for the intrinsic challenge that, for a given text, sentiment can be computed in many different ways, as well as the large number of possibilities to pool sentiment across texts and time. This additional layer of manipulation does not exist in standard text mining and time series analysis packages. The package therefore integrates the fast _quantification_ of sentiment from texts, the _aggregation_ into different sentiment time series and the optimized _prediction_ based on these measures.

See the [project page](https://sborms.github.io/sentometrics/), the [vignette](https://doi.org/10.2139/ssrn.3067734) and following [paper](https://doi.org/10.1016/j.ijforecast.2018.10.010) for respectively a brief and an extensive introduction to the package, and a real-life macroeconomic forecasting application. We also refer to this [survey](https://doi.org/10.2139/ssrn.2652876) for an overview of the workflow in a typical econometric analysis of sentiment from alternative (such as textual) data.

### Installation

To install the package from CRAN, simply do:

```R
install.packages("sentometrics")
```

The latest development version of **`sentometrics`** is available at [https://github.com/sborms/sentometrics](https://github.com/sborms/sentometrics). To install this version (which may contain bugs!), execute:

```R
devtools::install_github("sborms/sentometrics")
```

### References

Please cite **`sentometrics`** in publications. Use `citation("sentometrics")`.

### Acknowledgements

This software package originates from a
[Google Summer of Code 2017](https://github.com/rstats-gsoc/gsoc2017/wiki/Sentometrics:-An-integrated-framework-for-text-based-multivariate-time-series-modeling-and-forecasting) project, was further developed 
during a follow-up [Google Summer of Code 2019](https://github.com/rstats-gsoc/gsoc2019/wiki/sentometrics) project, and benefited generally from financial support by [Innoviris](https://innoviris.brussels), [IVADO](https://www.ivado.ca), [swissuniversities](https://www.swissuniversities.ch), and the [Swiss National Science Foundation](http://www.snf.ch) (grant #179281).

