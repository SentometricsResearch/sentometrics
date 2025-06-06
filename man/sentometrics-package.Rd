% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sentometrics.R
\docType{package}
\name{sentometrics-package}
\alias{sentometrics}
\alias{sentometrics-package}
\title{sentometrics: An Integrated Framework for Textual Sentiment Time Series Aggregation and Prediction}
\description{
The \pkg{sentometrics} package is an integrated framework for textual sentiment time series
aggregation and prediction. It accounts for the intrinsic challenge that, for a given text, sentiment can
be computed in many different ways, as well as the large number of possibilities to pool sentiment across
texts and time. This additional layer of manipulation does not exist in standard text mining and time series
analysis packages. The package therefore integrates the fast \emph{quantification} of sentiment from texts,
the \emph{aggregation} into different sentiment time series and the optimized \emph{prediction} based on
these measures.
}
\note{
Please cite the package in publications. Use \code{citation("sentometrics")}.
}
\section{Main functions}{

\itemize{
\item Corpus (features) generation: \code{\link{sento_corpus}}, \code{\link{add_features}},
\code{\link{as.sento_corpus}}
\item Sentiment computation and aggregation into sentiment measures: \code{\link{ctr_agg}},
\code{\link{sento_lexicons}}, \code{\link{compute_sentiment}}, \code{\link{aggregate.sentiment}},
\code{\link{as.sentiment}}, \code{\link{sento_measures}}, \code{\link{peakdocs}},
\code{\link{peakdates}}, \code{\link{aggregate.sento_measures}}
\item Sparse modeling: \code{\link{ctr_model}}, \code{\link{sento_model}}
\item Prediction and post-modeling analysis: \code{\link{predict.sento_model}},
\code{\link{attributions}}
}
}

\references{
Ardia, Bluteau, Borms and Boudt (2021). \strong{The R Package sentometrics to Compute, Aggregate, and
Predict with Textual Sentiment}. \emph{Journal of Statistical Software 99(2), 1-40},
\doi{10.18637/jss.v099.i02}.

Ardia, Bluteau and Boudt (2019). \strong{Questioning the news about economic growth: Sparse forecasting using
thousands of news-based sentiment values}. \emph{International Journal of Forecasting 35, 1370-1386},
\doi{10.1016/j.ijforecast.2018.10.010}.
}
\seealso{
Useful links:
\itemize{
  \item \url{https://sentometrics-research.com/sentometrics/}
  \item Report bugs at \url{https://github.com/SentometricsResearch/sentometrics/issues}
}

}
\author{
\strong{Maintainer}: Samuel Borms \email{borms_sam@hotmail.com} (\href{https://orcid.org/0000-0001-9533-1870}{ORCID})

Authors:
\itemize{
  \item David Ardia \email{david.ardia@hec.ca} (\href{https://orcid.org/0000-0003-2823-782X}{ORCID})
  \item Keven Bluteau \email{keven.bluteau@usherbrooke.ca} (\href{https://orcid.org/0000-0003-2990-4807}{ORCID})
  \item Kris Boudt \email{kris.boudt@vub.be} (\href{https://orcid.org/0000-0002-1000-5142}{ORCID})
}

Other contributors:
\itemize{
  \item Jeroen Van Pelt \email{jeroenvanpelt@hotmail.com} [contributor]
  \item Andres Algaba \email{andres.algaba@vub.be} [contributor]
}

}
