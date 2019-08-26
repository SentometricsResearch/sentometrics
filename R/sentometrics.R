
### TODO: add GSoC 2019 (also in readme file)
#' @title sentometrics: An Integrated Framework for Textual Sentiment Time Series Aggregation and Prediction
#'
#' @description The \pkg{sentometrics} package is an integrated framework for textual sentiment time series
#' aggregation and prediction. It accounts for the intrinsic challenge that, for a given text, sentiment can
#' be computed in many different ways, as well as the large number of possibilities to pool sentiment across
#' texts and time. This additional layer of manipulation does not exist in standard text mining and time series
#' analysis packages. The package therefore integrates the fast \emph{qualification} of sentiment from texts,
#' the \emph{aggregation} into different sentiment time series and the optimized \emph{prediction} based on
#' these measures.
#'
#' @section Main functions:
#' \itemize{
#' \item Corpus features generation: \code{\link{sento_corpus}}, \code{\link{add_features}}
#' \item Sentiment computation and aggregation into sentiment measures: \code{\link{ctr_agg}},
#' \code{\link{sento_lexicons}}, \code{\link{compute_sentiment}}, \code{\link{aggregate.sentiment}},
#' \code{\link{sento_measures}}, \code{\link{peakdocs}}, \code{\link{peakdates}}, and a series of
#' \code{measures_xyz}, generic and extractor functions
#' \item Sparse modelling: \code{\link{ctr_model}}, \code{\link{sento_model}}
#' \item Prediction and post-modelling analysis: \code{\link{predict.sentomodel}}, \code{\link{attributions}}
#' }
#'
#' The \code{\link{sento_app}} function is a Shiny interface for fast document-level sentiment computation and
#' downloading of the obtained scores.
#'
#' @section Update:
#' The development version of the package resides at \url{https://github.com/sborms/sentometrics}.
#'
#' @note Please cite the package in publications. Use \code{citation("sentometrics")}.
#'
#' @references Ardia, Bluteau and Boudt (2018). ``Questioning the news about economic growth: Sparse forecasting using
#' thousands of news-based sentiment values''. \emph{International Journal of Forecasting, forthcoming},
#' \url{https://doi.org/10.2139/ssrn.2976084}.
#' @references Ardia, Bluteau, Borms and Boudt (2018). ``The R package sentometrics to compute, aggregate and
#' predict with textual sentiment''. \emph{Working paper}, \url{https://doi.org/10.2139/ssrn.3067734}.
"_PACKAGE"

#' Built-in lexicons
#'
#' @docType data
#'
#' @description
#' A \code{list} containing all built-in lexicons as a \code{data.table} with two columns: a \code{x} column with the words,
#' and a \code{y} column with the polarities. The \code{list} element names incorporate consecutively the name and language
#' (based on the two-letter ISO code convention as in \code{\link[stopwords]{stopwords}}), and \code{"_tr"} as
#' suffix if the lexicon is translated. The translation was done via Microsoft Translator through Microsoft
#' Word. Only the entries that conform to the original language entry after retranslation, and those that have actually been
#' translated, are kept. The last condition is assumed to be fulfilled when the translation differs from the original entry.
#' All words are unigrams and in lowercase. The built-in lexicons are the following:
#'
#' \itemize{
#'   \item FEEL_en_tr
#'   \item FEEL_fr (Abdaoui, \enc{Azé}{Aze}, Bringay and Poncelet, 2017)
#'   \item FEEL_nl_tr
#'   \item GI_en (General Inquirer, i.e. Harvard IV-4 combined with Laswell)
#'   \item GI_fr_tr
#'   \item GI_nl_tr
#'   \item HENRY_en (Henry, 2008)
#'   \item HENRY_fr_tr
#'   \item HENRY_nl_tr
#'   \item LM_en (Loughran and McDonald, 2011)
#'   \item LM_fr_tr
#'   \item LM_nl_tr
#' }
#'
#' Other useful lexicons can be found in the \pkg{lexicon} package, more specifically the datasets preceded by
#' \code{hash_sentiment_}.
#'
#' @usage data("list_lexicons")
#'
#' @examples
#' data("list_lexicons", package = "sentometrics")
#' list_lexicons[c("FEEL_en_tr", "LM_en")]
#'
#' @format A \code{list} with all built-in lexicons, appropriately named as \code{"NAME_language(_tr)"} .
#'
#' @references Abdaoui, \enc{Azé}{Aze}, Bringay and Poncelet (2017). ``FEEL: French Expanded Emotion Lexicon''.
#' \emph{Language Resources & Evaluation 51, 833-855}, \url{https://doi.org/10.1007/s10579-016-9364-5}.
#' @references Henry (2008). ``Are  investors  influenced  by  how  earnings  press  releases  are  written?''.
#' \emph{Journal  of  Business Communication 45, 363-407}, \url{https://doi.org/10.1177/0021943608319388}.
#' @references Loughran and McDonald (2011). ``When is a liability not a liability? Textual analysis, dictionaries, and 10-Ks''.
#' \emph{Journal of Finance 66, 35-65}, \url{https://doi.org/10.1111/j.1540-6261.2010.01625.x}.
#'
#' @source \href{http://www.lirmm.fr/~abdaoui/FEEL}{FEEL lexicon}. Retrieved November 1, 2017.
#' @source \href{http://www.wjh.harvard.edu/~inquirer/spreadsheet_guide.htm}{GI lexicon}. Retrieved November 1, 2017.
#' @source \href{https://study.sagepub.com/sites/default/files/1\%20Henry\%202008_0.pdf}{HENRY lexicon}. Retrieved
#' November 1, 2017.
#' @source \href{https://sraf.nd.edu/textual-analysis/resources}{LM lexicon}. Retrieved
#' November 1, 2017.
"list_lexicons"

#' Built-in valence word lists
#'
#' @docType data
#'
#' @description
#' A \code{list} containing all built-in valence word lists, as \code{data.table}s with three columns: a \code{x} column with
#' the words, a \code{y} column with the values associated to each word, and a \code{t} column with the type of valence
#' shifter (\code{1} = negators, \code{2} = amplifiers, \code{3} = deamplifiers). The \code{list} element names indicate the
#' language (based on the two-letter ISO code convention as in \code{\link[stopwords]{stopwords}}) of the valence word list.
#' All non-English word lists are translated via Microsoft Translator through Microsoft Word. Only the entries whose
#' translation differs from the original entry are kept. All words are unigrams and in lowercase. The built-in valence word
#' lists are available in following languages:
#'
#' \itemize{
#'   \item English (\code{"en"})
#'   \item French (\code{"fr"})
#'   \item Dutch (\code{"nl"})
#' }
#'
#' @usage data("list_valence_shifters")
#'
#' @examples
#' data("list_valence_shifters", package = "sentometrics")
#' list_valence_shifters["en"]
#'
#' @format A \code{list} with all built-in valence word lists, appropriately named.
#'
#' @source \code{\link[lexicon]{hash_valence_shifters}} (English valence shifters). Retrieved August 24, 2018.
"list_valence_shifters"

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
#'   \item texts. Texts in \code{character} format.
#'   \item wsj. Equals 1 if the article comes from The Wall Street Journal.
#'   \item wapo. Equals 1 if the article comes from The Washington Post (complementary to `wsj').
#'   \item economy. Equals 1 if the article is relevant to the U.S. economy.
#'   \item noneconomy. Equals 1 if the article is not relevant to the U.S. economy (complementary to `economy').
#' }
#'
#' @usage data("usnews")
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' usnews[3192, "texts"]
#' usnews[1:5, c("id", "date", "texts")]
#'
#' @format A \code{data.frame}, formatted as required to be an input for \code{\link{sento_corpus}}.
#'
#' @source \href{https://www.figure-eight.com/data-for-everyone/}{Economic News Article Tone and Relevance}. Retrieved
#' November 1, 2017.
"usnews"

#' Monthly Economic Policy Uncertainty Index
#'
#' @docType data
#'
#' @description
#' Monthly news-based U.S. Economic Policy Uncertainty (EPU) index (Baker, Bloom and Davis, 2015). Goes from January 1985
#' to July 2018, and includes a binomial and a multinomial example series. Following columns are present:
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
#' @examples
#' data("epu", package = "sentometrics")
#' head(epu)
#'
#' @format A \code{data.frame} with 403 rows and 4 columns.
#'
#' @references Baker, Bloom and Davis (2016). ``Measuring Economic Policy Uncertainty''.
#' \emph{The Quarterly Journal of Economics 131, 1593-1636}, \url{https://doi.org/10.1093/qje/qjw024}.
#'
#' @source \href{http://www.policyuncertainty.com/us_monthly.html}{Measuring Economic Policy Uncertainty}. Retrieved
#' August 24, 2018.
"epu"

#' @useDynLib sentometrics,.registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom RcppParallel RcppParallelLibs
NULL

