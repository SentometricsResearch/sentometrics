
#' @name Sentometrics
#' @docType package
#'
#' @title Sentometrics: An Integrated Framework for Textual Sentiment Based Multivariate Time Series Modeling and Forecasting
#'
#' @description The Sentometrics package is designed to do time series analysis based on textual sentiment.
#' It accounts for the intrinsic challenge that for a given text sentiment can be computed in hundreds of different
#' ways, as well as the large number of possibilities to pool sentiment across text and time. This additional layer of
#' manipulation does not exist in standard time series analysis packages. As a final outcome, this package provides
#' an automated means to econometrically model the impact of sentiment in texts on a given variable, by optimizing the
#' sentiment extraction and aggregation for the forecasting task. Aggregation can be optimized across several dimensions,
#' for example word term weighting schemes or time lag structures. The package created therefore integrates the qualification
#' of sentiment from text, the aggregation into different sentiment measures and the optimized forecasting based on
#' these measures.
#'
#' @section Functions:
#' \itemize{
#' \item Sentiment computation and aggregation into sentiment measures: \code{\link{...}}
#' \item Sparse modelling: \code{\link{...}}
#' \item Forecasting and post-modelling analysis: \code{\link{...}}
#' }
#'
#' @section Update:
#' The latest version of the package is available at \url{https://github.com/ArdiaD/Sentometrics}.
#'
#' @author Samuel Borms, Keven Bluteau, David Ardia and Kris Boudt.
#'
#' @note The ideas behind the sentiment aggregation framework can be consulted in the working paper titled 'Aggregating the
#' Panel of Daily Textual Sentiment for Sparse Forecasting of Economic Growth' (Ardia, Bluteau and Boudt, 2017) at
#' \url{https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2976084}.
#'
#' @note Please cite the package in publications. Use \code{citation("Sentometrics")}.
"_PACKAGE"

#' Built-in lexicons
#'
#' @description
#' A list containing all built-in lexicons, with two columns: a \code{x} column with the words, and a \code{y} column with
#' the polarities. The list element names incorporate consecutively the name and language, and 'tr' as suffix if the lexicon is
#' translated. The lexicons are in the form required for further sentiment analysis. The built-in lexicons are the following:
#'
#' \itemize{
#'   \item xxx. yyy (source: \link{})
#' }
#'
#' @format A list with all built-in lexicons, appropriately named.
"LEXICONS"

