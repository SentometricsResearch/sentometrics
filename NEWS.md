
## sentometrics 0.7.6

- fixed memory allocation issue in the `compute_sentiment()` function

## sentometrics 0.7.5

- new functions: `as.data.table.sento_corpus()`, `as.data.frame.sento_corpus()`, and `as.data.frame.sento_measures()`
- embedded a small workaround in the `plot.attributions()` function to guaranty same plotting behaviour after update of **`ggplot2`** package that gave buggy output for the `geom_area()` layer
- integrated for overall consistency the `measures_global()` function into the `aggregate.sento_measures()` function, adding a `do.global` argument to enact it
- slightly changed the clusters-based sentence-level sentiment computation (different weighting of
adversative conjunctions)
- clarified the documentation for the `peakdates()` and `peakdocs()` functions
- put the Shiny application made available in previous package update (i.e., the `sento_app()` function) in a separate sole-purpose package **`sentometrics.app`** (see https://github.com/sborms/sentometrics.app) 
- moved the **`data.table`** package from Depends to Imports (see https://github.com/Rdatatable/data.table/issues/3076)
- no change by reference of input sentiment objects in the `merge.sentiment()` function anymore, and modified the merging to give for instance a simple column binding of sentment methods when all else is equal
- correct pass-through of default `how` argument in the `compute_sentiment()` function
- added a few adversative conjunctions to all word lists in `list_valence_shifters`
- added a `do.normalize` option to the `weights_beta()` and `weights_exponential()` functions
- added a `do.inverse` option to the `weights_exponential()` function and associated `do.inverseExp` argument in the `ctr_agg()` function
- modified some names of options for within-document or within-sentence aggregation (i.e., across tokens): `"squareRootCounts"` into `"proportionalSquareRoot"`, `"invertedExponential"` into `"inverseExponential"`, and `"invertedUShaped"` into `"inverseUShaped"`
- corrected the numerator (number of documents or sentences instead of token frequency) in all weighting schemes involving the inverse document frequency (IDF)
- aligned all formulas concerning the exponential weighting curves
- the `compute_sentiment()` function now also can do a sentence-level calculation using the bigrams valence shifting approach
- fixed a small bug that did not allow to have different valence shifters lists for a multi-language sentiment calculation

## sentometrics 0.7.0

- new functions: `measures_update()`, `subset.sento_measures()`, `as.sentiment()`, `as.sento_measures()`, `as.data.table.sentiment()`, `corpus_summarize()`, `sento_app()`, and `aggregate.sento_measures()`
- defunct all deprecated functions as well as the functions replaced by the new functions (_wiping the slate clean..._) 
- handled reverse dependency issue raised by **`quanteda`** developers regarding their new corpus object
- renamed the class objects coming from any `sento_xyz()` function into the name of the function (e.g., the `sento_measures()` function now gives a `sento_measures` object instead of a `sentomeasures` object)
- fixed a small bug in the `aggregate.sento_measures()` (previously `measures_merge()`) function to take the mean instead of the sum in a particular case
- added many more within- and across-document weighting schemes (see the `get_hows()` function for an overview)
- added the flexibility to do an explicit sentence-by-sentence sentiment computation (see `do.sentence` argument in the `compute_sentiment()` function)
- added the flexibility to create a multi-language `sento_corpus` object to do a multi-language sentiment computation (applying different lexicons to texts written in different languages)
- expanded the `compute_sentiment()` function to also take **`tm`** `SimpleCorpus` and `VCorpus` objects
- added the **`tm`** and **`NLP`** packages to Suggests

## sentometrics 0.5.6

- new function: `peakdates()`
- modified the purpose of the `peakdocs()` function and added a `peakdates()` function to properly handle the entire functionality of extracting peaks 
- a series of documentation fixes

## sentometrics 0.5.5

- new functions: `sentiment_bind()`, and `to_sentiment()`
- defined replacement (of lexicons and names) for a `sentolexicons` object
- properly handled `lag = 1` in the `ctr_agg()` function, and set weights to 1 by default for `n = 1` in the `weights_beta()` function
- solved single failing test for older R version (3.4.4)
- removed the **`abind`** package from Imports
- removed the **`zoo`** package from Imports, by replacing the single occurrence of the `zoo::na.locf()` function by the `fill_NAs()` helper function (written in **`Rcpp`**)
- extended the `quanteda::docvars()` replacement method to a `sentocorpus` object
- modified information criterion estimators for edge cases to avoid them turning negative 
- dropped the `"x"` output element from a `sentomodel` object (for large samples, this became too memory consuming)
- dropped the `"howWithin"` output element from a `sentomeasures` object, and simplified a `sentiment` object into a `data.table` directly instead of a `list`
- expanded the `do.shrinkage.x` argument in the `ctr_model()` function to a vector argument
- added a `do.lags` argument to the `attributions()` function, to be able to circumvent the most time-consuming part of the computation 
- imposed a check in the `sento_measures()` function on the uniqueness of the names within and across the lexicons, features and time weighting schemes
- solved a bug in the `measures_merge()` function that made full merging not possible
- the `n` argument in the `peakdocs()` function can now also be specified as a quantile

## sentometrics 0.5.1

- minor modifications to resolve few CRAN issues
- set default value of `nCore` argument in the `compute_sentiment()` and `ctr_agg()` functions to 1
- classed the output of the `compute_sentiment.sentocorpus()` function as a `sentiment` object, and modified the `aggregate()` function to `aggregate.sentiment()`

## sentometrics 0.5.0

- new functions: `weights_beta()`, `get_dates()`, `get_dimensions()`, `get_measures()`, and `get_loss_data()`
- renamed following functions: `to_global()` to `measures_global()`, `perform_agg()` to `aggregate()`, `almons()` to `weights_almon()`, `exponentials()` to `weights_exponential()`, `setup_lexicons()` to `sento_lexicons()`, `retrieve_attributions()` to `attributions()`, `plot_attributions()` to `plot.attributions()`
- defunct the `ctr_merge()` function, so that all merge parameters have to be passed on directly to the `measures_merge()` function
- expanded the use of the `center` and `scale` arguments in the `scale()` function
- added the `dateBefore` and `dateAfter` arguments to the `measures_fill()` function, and dropped `NA` option of its `fill` argument
- added a `"beta"` time aggregation option (see associated `weights_beta()` function)
- corrected update of `"attribWeights"` element of output `sentomeasures` object in required `measures_xyz()` functions
- added a new attribution dimension (`"lags"`) to the `attributions()` function, and corrected some edge cases
- made a slight correction to the information criterion estimators
- added a `lambdas` argument to the `ctr_model()` function, directly passed on to the `glmnet::glmnet()` function if used
- omitted `do.combine` argument in `measures_delete()` and `measures_select()` functions to simplify
- expanded set of unit tests, included a coverage badge, and added **`covr`** to Suggests
- reimplementation (and improved documentation) of the sentiment calculation in the `compute_sentiment()` function, by writing part of the code in **`Rcpp`** relying on **`RcppParallel`** (added to Imports); there are now three approaches to computing sentiment (unigrams, bigrams and clusters)
- replaced the `dfm` argument in the `compute_sentiment()` and `ctr_agg()` functions by a `tokens` argument, and altered the input and behaviour of the `nCore` argument in these same two functions
- switched from the **`quanteda`** package to the **`stringi`** package for more direct tokenization
- trimmed the `list_lexicons` and `list_valence_shifters` built-in word lists by keeping only unigrams, and included same trimming procedure in the `sento_lexicons()` function
- added a type column `"t"` to the `list_valence_shifters` built-in word list, and reset values of the `"y"` column from 2 to 1.8 and from 0.5 to 0.2
- updated the `epu` built-in dataset with the newest available series, up to July 2018
- corrected the word 'sparesly' to 'sparsely' in `list_valence_shifters[["en"]]`
- further shortened project page to the bare essence
- omitted statement printed ('Compute sentiment... Done.') in the `compute_sentiment()` function
- slightly modified `print()` generic for a `sentomeasures` object 
- dropped the `"tf-idf"` option for within-document aggregation in the `ctr_agg()` function
- the `sento_lexicons()` function outputs a `sentolexicons` object, which the `compute_sentiment()` function specifically requires as an input; a `sentolexicons` object also includes a `"["` class-preserving extractor function
- the `attributions()` function outputs an `attributions` object; the `plot_attribtutions()` function is therefore replaced by the `plot()` generic
- defunct the `perform_MCS()` function, but the output of the `get_loss_data()` function can easily be used as an input to the `MCSprocedure()` function from the **`MCS`** package (discarded from Imports)
- moved the **`parallel`** and **`doParallel`** packages to Suggests, as only needed (if enacted) in the `sento_model()` function
- sligthly modified appearance of plotting functions, to drop **`ggthemes`** from Imports

## sentometrics 0.4.0

- new functions: `measures_delete()`, `nmeasures()`, `nobs()`, and `to_sentocorpus()`
- renamed following functions: any `xyz_measures()` to `measures_xyz()`, `extract_peakdocs()` to `peakdocs()`
- dropped `do.normalizeAlm` argument in the `ctr_agg()` function (but kept in the `almons()` function)
- inverted order of rows in output of the `almons()` function to be consistent with Ardia et al. (2017) paper
- renamed `lexicons` to `list_lexicons`, and `valence` to `list_valence_shifters` 
- the `stats` element of a `sentomeasures` object is now also updated in `measures_fill()`
- changed `"_eng"` to `"_en"`' in `list_lexicons` and `list_valence_shifters` objects, to be in accordance with two-letter ISO language naming
- changed `"valence_language"` naming to `"language"` in `list_valence_shifters` object
- the `compute_sentiment()` function now also accepts a **`quanteda`** `corpus` object and a `character` vector
- the `add_features()` function now also accepts a **`quanteda`** `corpus` object
- added an `nCore` argument to the `compute_sentiment()`, `ctr_agg()`, and `ctr_model()` functions to allow for (more straightforward) parallelized computations, and omitted the `do.parallel` argument in the `ctr_model()` function
- added a `do.difference` argument to the `ctr_model()` function and expanded the use of the already existing `oos` argument
- brought **`ggplot2`** and **`foreach`** to Imports

## sentometrics 0.3.5

- faster `to_global()`
- set `tolower = FALSE` of `quanteda::dfm()` constructor in `compute_sentiment()`
- changed `intercept` argument in `ctr_model()` to `do.intercept` for consistency
- proper checks on values of feature columns in `sento_corpus()` and `add_features()`

## sentometrics 0.3.0

- new functions: `diff()`, `extract_peakdocs()`, and `subset_measures()` 
- modified R Depends from 3.4.2 to 3.3.0, and omitted import of **`sentimentr`**
- word count per document now determined based on a separate tokenization
- improved valence shifters search (modified `incluce_valence()` helper function)
- new option added for within-document aggregation (`"proportionalPol"`)
- now correct pass-through of `dfm` argument in `ctr_agg()`
- `select_measures()` simplified, but `toSelect` argument expanded
- calculation in `to_global()` changed (see vignette)
- improved `add_features()`: regex and non-binary (between 0 and 1) allowed
- all texts and lexicons now automatically to lowercase for sentiment calculation
- (re)translation of built-in lexicons and valence word lists
- small documentation clarifications and fixes
- new vignette and run_vignette.R script
- shortened project page (no code example anymore)

## sentometrics 0.2.0

- first public release

## sentometrics 0.1.0

- Google Summer of Code 2017 "release" (unstable)

