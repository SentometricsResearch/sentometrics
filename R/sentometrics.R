
#' @title An Integrated Framework for Textual Sentiment Time Series Aggregation and Prediction
#'
#' @description The \pkg{sentometrics} package is designed to do time series analysis based on textual sentiment. It accounts
#' for the intrinsic challenge that, for a given text, sentiment can be computed in many ways, as well as the large
#' number of possibilities to pool sentiment across text and time. This additional layer of manipulation does not exist
#' in standard time series analysis and text mining packages. The package also provides an automated means to
#' econometrically model the impact of sentiment in texts on a given variable, by first computing a wide range of textual
#' sentiment time series and then selecting those that are most informative. Altogether, \pkg{sentometrics} integrates the
#' \emph{qualification} of sentiment from texts, the \emph{aggregation} into different sentiment measures and the optimized
#' \emph{prediction} based on these measures.
#'
#' @section Main functions:
#' \itemize{
#' \item Sentiment computation and aggregation into sentiment measures: \code{\link{sento_corpus}}, \code{\link{ctr_agg}},
#' \code{\link{compute_sentiment}}, \code{\link{sento_measures}}, \code{\link{to_global}}
#' \item Sparse modelling: \code{\link{ctr_model}}, \code{\link{sento_model}}
#' \item Prediction and post-modelling analysis: \code{\link{predict.sentomodel}}, \code{\link{retrieve_attributions}},
#' \code{\link{perform_MCS}}
#' }
#'
#' @section Update:
#' The development version of the package is available at \url{https://github.com/sborms/sentometrics}.
#'
#' @note The methodology behind the sentiment aggregation framework can be consulted in the working paper ``Questioning
#' the news about economic growth: Sparse forecasting using thousands of news-based sentiment values'' (Ardia, Bluteau, and
#' Boudt, 2017) at \url{https://ssrn.com/abstract=2976084}. The vignette  ``The R package sentometrics
#' to compute, aggregate and predict with textual sentiment'' (Ardia, Bluteau, Borms and Boudt, 2017) at
#' \url{https://ssrn.com/abstract=3067734} provides a hands-on introduction to the methodology and
#' the package's functionalities.
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
#' \code{"_tr"} as suffix if the lexicon is translated. The translation was done via Microsoft Translator through Microsoft
#' Word. Only the entries that conform to the original language entry after retranslation, and those that have actually been
#' translated, are kept. The last condition is assumed to be fulfilled when the translation differs from the original entry.
#' All words are in lowercase. The lexicons are in the format required for further sentiment analysis. The built-in lexicons
#' are the following:
#'
#' \itemize{
#'   \item FEEL_eng_tr (French Expanded Emotion Lexicon)
#'   \item FEEL_fr
#'   \item FEEL_nl_tr
#'   \item GI_eng (General Inquirer, i.e. Harvard IV-4 combined with Laswell)
#'   \item GI_fr_tr
#'   \item GI_nl_tr
#'   \item HENRY_eng (Henry)
#'   \item HENRY_fr_tr
#'   \item HENRY_nl_tr
#'   \item LM_eng (Loughran and McDonald)
#'   \item LM_fr_tr
#'   \item LM_nl_tr
#' }
#'
#' Other immediate lexicon options can be found in the \pkg{lexicon} package, more specifically the datasets preceded by
#' \code{hash_sentiment_}.
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
#' words, a \code{t} column with the type of valence words, and a \code{y} column with the values associated to each word and
#' type of valence shifter. The list element names incorporate the language of the valence word list. All non-English word
#' lists are translated via Microsoft Translator through Microsoft Word. Only the entries whose translation differs from
#' the original entry are kept. The valence word lists are in the form required for further sentiment analysis. All words
#' are in lowercase. The built-in valence word lists are the following:
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

#' Texts (not) relevant to the U.S. economy
#'
#' @docType data
#'
#' @description
#' A collection of texts annotated by humans in terms of relevance to the U.S. economy or not. The texts come from two major
#' journals in the U.S. (The Wall Street Journal and The Washington Post) and cover 4145 documents between 1995 and 2014. It
#' contains following information:
#'
#' \itemize{
#'   \item id. A \code{character} ID identifier.
#'   \item date. Date as \code{"yyyy-mm-dd"}.
#'   \item text. Texts in \code{character} format.
#'   \item wsj. Equals 1 if the article comes from The Wall Street Journal.
#'   \item wapo. Equals 1 if the article comes from The Washington Post.
#'   \item economy. Equals 1 if the article is relevant to the U.S. economy.
#'   \item noneconomy. Equals 1 if the article is not relevant to the U.S. economy.
#' }
#'
#' @usage data("usnews")
#'
#' @examples
#' data("usnews")
#' usnews[3192, "text"]
#' usnews[1:5, c("id", "date", "text")]
#'
#' @format A \code{data.frame}, formatted as required to be an input for \code{\link{sento_corpus}}.
#'
#' @source \href{https://www.crowdflower.com/data-for-everyone/}{Economic News Article Tone and Relevance}
"usnews"

#' Monthly Economic Policy Uncertainty Index
#'
#' @docType data
#'
#' @description
#' Monthly values of a news-based index of U.S. Economic Policy Uncertainty (EPU) between January 1980 and September
#' 2014, including a binomial and a multinomial example series. For more information on its calculation, see
#' \href{http://www.policyuncertainty.com/methodology.html}{this}. Following columns are present:
#'
#' \itemize{
#'   \item date. Date as \code{"yyyy-mm-01"}.
#'   \item index. A \code{numeric} monthly index value.
#'   \item above. A \code{factor} with value \code{"above"} if the index is greater than the mean of the entire series, else
#'   \code{"below"}.
#'   \item aboveMulti. A \code{factor} with values \code{"above+"}, \code{"above"}, \code{"below"} and \code{"below-"} if the
#'   index is greater than the 75\% quantile and the 50\% quantile, or smaller than the 50\% quantile and the 25\% quantile,
#'   respectively and in a mutually exclusive sense.
#' }
#'
#' @usage data("epu")
#'
#' @format A \code{data.frame} with 417 rows and 4 columns.
#'
#' @source \href{http://www.policyuncertainty.com/research.html}{Research on Economic Policy Uncertainty}
"epu"

#' @useDynLib sentometrics,.registration = TRUE
#' @importFrom Rcpp evalCpp
NULL

