
#' @name sentometrics
#' @docType package
#'
#' @title sentometrics: An Integrated Framework for Textual Sentiment Based Multivariate Time Series Modeling and Forecasting
#'
#' @description The sentometrics package is designed to do time series analysis based on textual sentiment. It accounts
#' for the intrinsic challenge that for a given text sentiment can be computed in hundreds of different ways, as well as
#' the large number of possibilities to pool sentiment across text and time. This additional layer of manipulation
#' does not exist in standard time series analysis packages. As a final outcome, this package provides an automated means
#' to econometrically model the impact of sentiment in texts on a given variable, by optimizing the sentiment extraction
#' and aggregation for the forecasting task. Aggregation can be optimized across several dimensions, for example word term
#' weighting schemes or time lag structures. The package created therefore integrates the qualification of sentiment from
#' text, the aggregation into different sentiment measures and the optimized forecasting based on these measures.
#'
#' @section Functions:
#' \itemize{
#' \item Sentiment computation and aggregation into sentiment measures: to do
#' \item Sparse modelling: to do
#' \item Forecasting and post-modelling analysis: to do
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
#' @note Please cite the package in publications. Use \code{citation("sentometrics")}.
"_PACKAGE"

#' Built-in lexicons
#'
#' @docType data
#'
#' @description
#' A list containing all built-in lexicons as a \code{data.table} with two columns: a \code{x} column with the words, and a
#' \code{y} column with the polarities. The list element names incorporate consecutively the name and language, and
#' \code{"_tr"} as suffix if the lexicon is translated. The lexicons are in the form required for further sentiment analysis.
#' The built-in lexicons are the following:
#'
#' \itemize{
#'   \item FEEL_eng_tr (FEEL: French Expanded Emotion Lexicon)
#'   \item FEEL_fr
#'   \item FEEL_nl_tr
#'   \item GI_eng (GI: General Inquirer, i.e. Harvard IV-4 combined with Laswell)
#'   \item GI_fr_tr
#'   \item GI_nl_tr
#'   \item HENRY_eng (HENRY: Henry)
#'   \item HENRY_fr_tr
#'   \item HENRY_nl_tr
#'   \item LM_eng (LM: Loughran and McDonald)
#'   \item LM_fr_tr
#'   \item LM_nl_tr
#' }
#'
#' @usage data("lexicons")
#'
#' @format A list with all built-in lexicons, appropriately named as \code{"NAME_language(_tr)"} .
#'
#' @source \href{http://www.lirmm.fr/~abdaoui/FEEL}{FEEL lexicon}
#' @source \href{http://www.wjh.harvard.edu/~inquirer/spreadsheet_guide.htm}{GI lexicon}
#' @source \href{https://study.sagepub.com/sites/default/files/1\%20Henry\%202008_0.pdf}{HENRY lexicon}
#' @source \href{https://www3.nd.edu/~mcdonald/Word_Lists.html}{LM lexicon}
#'
#' @examples
#' lexicons[c("FEEL_eng_tr", "LM_eng")]
"lexicons"

#' Built-in valence word lists
#'
#' @docType data
#'
#' @description
#' A list containing all built-in valence word lists, a \code{data.table} with three columns: a \code{x} column with the
#' words, a \code{t} column with the type of valence words, and a \code{y} column with the value associated to a word and
#' type of valence shifter. The list element names incorporate the language of the valence word list. All non-English word
#' lists are translated. The valence word lists are in the form required for further sentiment analysis. The built-in valence
#' word lists are the following:
#'
#' \itemize{
#'   \item valence_eng
#'   \item valence_fr
#'   \item valence_nl
#' }
#'
#' @usage data("valence")
#'
#' @format A list with all built-in valence word lists, appropriately named.
#'
#' @source \code{\link[lexicon]{hash_valence_shifters}} (negators)
"valence"

#' Texts relevant (and not) to the US economy
#'
#' @docType data
#'
#' @description
#' A collection of texts annotated by humans in terms of relevance to the US economoy or not. The texts come from two major
#' journals in the US (The Wall Street Journal and The Washington Post) and cover 6801 documents between 1980 and 2014. It
#' contains following information:
#'
#' \itemize{
#'   \item id. ID identifier.
#'   \item date. Date as \code{"yyyy-mm-dd"}.
#'   \item text. Texts in \code{character} format.
#'   \item headline. Headlines in \code{character} format.
#'   \item wsj. Equals 1 if the article comes from The Wall Street Journal.
#'   \item wapo. Equals 1 if the article comes from The Washington Post.
#'   \item economy. Equals 1 if the article is relevant to the US economy.
#'   \item noneconomy. Equals 1 if the article is not relevant to the US economy.
#' }
#'
#' @usage data("useconomynews")
#'
#' @format A \code{data.table}, formatted as required to be an input for \code{\link{sento_corpus}}.
#'
#' @source \href{https://www.crowdflower.com/data-for-everyone/}{Economic News Article Tone and Relevance}
"useconomynews"

#' Monthly S&P 500 Index returns
#'
#' @docType data
#'
#' @description
#' Monthly returns for the S&P 500 Index between March 1988 and December 2014, including a binomial and a multinomial example
#' series.
#'
#' \itemize{
#'   \item date. Date as \code{"yyyy-mm-01"}.
#'   \item return. A \code{numeric} monthly return value.
#'   \item up. A \code{factor} with value \code{"pos"} if return is greater than zero, else \code{"neg"}.
#'   \item upMulti. A \code{factor} with values \code{"pos+"}, \code{"pos"}, \code{"neg"} and \code{"neg-"} if returns are
#'   greater than 0.05 and 0, or smaller than 0 and -0.05, respectively and in a mutually exclusive sense.
#' }
#'
#' @usage data("sp500")
#'
#' @format A \code{data.frame} with 322 rows and 4 columns.
#'
#' @source \href{https://finance.yahoo.com/quote/\%5EGSPC/history?p=\%5EGSPC}{S&P 500 (^GSPC) at Yahoo Finance}
"sp500"

