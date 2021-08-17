
## sentometrics 1.0.0

- Version bump associated with publication of vignette in the Journal of Statistical Software.

## sentometrics 0.8.4

- Alignment with released **`quanteda`** v3.0.

## sentometrics 0.8.3

- Features (or docvars) with names `"id"`, `"sentence_id"`, `"date"`, `"word_count"` or `"texts"` will not be accepted even when `numeric`, to avoid duplicate column names down the line. A clear error message is issued to alert users.
- Replacement of `order()` calls on `data.frame`s where needed to avoid CRAN complaints.
- Some small documentation fixes.

## sentometrics 0.8.2

- Some documentation fixes.
- Release of a **`pkgdown`** website.
- Fixed bug in `sento_corpus()` function that did not always order input correctly by date.
- Fixed two minor bugs in `summary.sento_measures()`; the first one prevented printing of document-level weighting schemes, the second one did not remove `NA`s when averaging over correlations.
- Small bug fix in yearly aggregation (it did not account for the fact that `1970-01-01` is considered day zero).
- Dropped horizontal 0-line automatically added in the `plot.sento_measures()` function as it distorts graphs of time series with values far away from zero.
- Stopped exporting all defunct functions to clean up namespace.
- The function `print.sento_corpus()` now shows when corpus is multi-lingual.

## sentometrics 0.8.1

- Alignment with released **`quanteda`** v2.0.

## sentometrics 0.8.0

- New function: `print.sento_corpus()`.
- Package update followed by release of a substantial update of the vignette (see https://doi.org/10.2139/ssrn.3067734).
- Changed some `warning()` calls to `message()` calls to be more kind to the user.
- Altered internal code to comply with the `corpus` object from **`quanteda`** >= v2.0.
- Dropped all `"TF"`-inspired weights for within-document aggregation except for `"TFIDF"`, and made this option return the same sentiment scores as would when using the **`quanteda`** package (see the example on https://sentometrics-research.com/sentometrics/articles/examples/sentiment.html).

## sentometrics 0.7.6

- Fixed memory allocation issue in the `compute_sentiment()` function.

## sentometrics 0.7.5

- New functions: `as.data.table.sento_corpus()`, `as.data.frame.sento_corpus()`, and `as.data.frame.sento_measures()`.
- Embedded a small workaround in `plot.attributions()` to guaranty same plotting behaviour after update of **`ggplot2`** package that gave buggy output for the `geom_area()` layer.
- Integrated for overall consistency `measures_global()` into the `aggregate.sento_measures()` function, adding a `do.global` argument to enact it.
- Slightly changed the clusters-based sentence-level sentiment computation (different weighting of
adversative conjunctions).
- Clarified the documentation for the `peakdates()` and `peakdocs()` functions.
- Put the Shiny application made available in previous package update (i.e., the `sento_app()` function) in a separate sole-purpose package **`sentometrics.app`** (see https://github.com/sborms/sentometrics.app). 
- Moved the **`data.table`** package from Depends to Imports (see https://github.com/Rdatatable/data.table/issues/3076).
- No change by reference of input sentiment objects in the `merge.sentiment()` function anymore, and modified the merging to give for instance a simple column binding of sentment methods when all else is equal.
- Correct pass-through of default `how` argument in the `compute_sentiment()` function.
- Added a few adversative conjunctions to all word lists in `list_valence_shifters`.
- Added a `do.normalize` option to the `weights_beta()` and `weights_exponential()` functions.
- Added a `do.inverse` option to the `weights_exponential()` function and associated `do.inverseExp` argument in the `ctr_agg()` function.
- Modified some names of options for within-document or within-sentence aggregation (i.e., across tokens): `"squareRootCounts"` into `"proportionalSquareRoot"`, `"invertedExponential"` into `"inverseExponential"`, and `"invertedUShaped"` into `"inverseUShaped"`.
- Corrected the numerator (number of documents or sentences instead of token frequency) in all weighting schemes involving the inverse document frequency (IDF).
- Aligned all formulas concerning the exponential weighting curves.
- The `compute_sentiment()` function now also can do a sentence-level calculation using the bigrams valence shifting approach.
- Fixed a small bug that did not allow to have different valence shifters lists for a multi-language sentiment calculation.

## sentometrics 0.7.0

- New functions: `measures_update()`, `subset.sento_measures()`, `as.sentiment()`, `as.sento_measures()`, `as.data.table.sentiment()`, `corpus_summarize()`, `sento_app()`, and `aggregate.sento_measures()`.
- Defunct all deprecated functions as well as the functions replaced by the new functions (_wiping the slate clean..._). 
- Handled reverse dependency issue raised by **`quanteda`** developers regarding their new corpus object.
- Renamed the class objects coming from any `sento_xyz()` function into the name of the function (e.g., the `sento_measures()` function now gives a `sento_measures` object instead of a `sentomeasures` object).
- Fixed a small bug in the `aggregate.sento_measures()` (previously `measures_merge()`) function to take the mean instead of the sum in a particular case.
- Added many more within- and across-document weighting schemes (see the `get_hows()` function for an overview).
- Added the flexibility to do an explicit sentence-by-sentence sentiment computation (see `do.sentence` argument in the `compute_sentiment()` function).
- Added the flexibility to create a multi-language `sento_corpus` object to do a multi-language sentiment computation (applying different lexicons to texts written in different languages).
- Expanded the `compute_sentiment()` function to also take **`tm`** `SimpleCorpus` and `VCorpus` objects.
- Added the **`tm`** and **`NLP`** packages to Suggests.

## sentometrics 0.5.6

- New function: `peakdates()`.
- Modified the purpose of the `peakdocs()` function and added a `peakdates()` function to properly handle the entire functionality of extracting peaks. 
- A series of documentation fixes.

## sentometrics 0.5.5

- New functions: `sentiment_bind()`, and `to_sentiment()`.
- Defined replacement (of lexicons and names) for a `sentolexicons` object.
- Properly handled `lag = 1` in the `ctr_agg()` function, and set weights to 1 by default for `n = 1` in the `weights_beta()` function.
- Solved single failing test for older R version (3.4.4).
- Removed the **`abind`** package from Imports.
- Removed the **`zoo`** package from Imports, by replacing the single occurrence of the `zoo::na.locf()` function by the `fill_NAs()` helper function (written in **`Rcpp`**).
- Extended the `quanteda::docvars()` replacement method to a `sentocorpus` object.
- Modified information criterion estimators for edge cases to avoid them turning negative.
- Dropped the `"x"` output element from a `sentomodel` object (for large samples, this became too memory consuming).
- Dropped the `"howWithin"` output element from a `sentomeasures` object, and simplified a `sentiment` object into a `data.table` directly instead of a `list`.
- Expanded the `do.shrinkage.x` argument in the `ctr_model()` function to a vector argument.
- Added a `do.lags` argument to the `attributions()` function, to be able to circumvent the most time-consuming part of the computation .
- Imposed a check in the `sento_measures()` function on the uniqueness of the names within and across the lexicons, features and time weighting schemes.
- Solved a bug in the `measures_merge()` function that made full merging not possible.
- The `n` argument in the `peakdocs()` function can now also be specified as a quantile.

## sentometrics 0.5.1

- Minor modifications to resolve few CRAN issues.
- Set default value of `nCore` argument in the `compute_sentiment()` and `ctr_agg()` functions to 1.
- Classed the output of the `compute_sentiment.sentocorpus()` function as a `sentiment` object, and modified the `aggregate()` function to `aggregate.sentiment()`.

## sentometrics 0.5.0

- New functions: `weights_beta()`, `get_dates()`, `get_dimensions()`, `get_measures()`, and `get_loss_data()`.
- Renamed following functions: `to_global()` to `measures_global()`, `perform_agg()` to `aggregate()`, `almons()` to `weights_almon()`, `exponentials()` to `weights_exponential()`, `setup_lexicons()` to `sento_lexicons()`, `retrieve_attributions()` to `attributions()`, `plot_attributions()` to `plot.attributions()`.
- Defunct the `ctr_merge()` function, so that all merge parameters have to be passed on directly to the `measures_merge()` function.
- Expanded the use of the `center` and `scale` arguments in the `scale()` function.
- Added the `dateBefore` and `dateAfter` arguments to the `measures_fill()` function, and dropped `NA` option of its `fill` argument.
- Added a `"beta"` time aggregation option (see associated `weights_beta()` function).
- Corrected update of `"attribWeights"` element of output `sentomeasures` object in required `measures_xyz()` functions.
- Added a new attribution dimension (`"lags"`) to the `attributions()` function, and corrected some edge cases.
- Made a slight correction to the information criterion estimators.
- Added a `lambdas` argument to the `ctr_model()` function, directly passed on to the `glmnet::glmnet()` function if used.
- Omitted `do.combine` argument in `measures_delete()` and `measures_select()` functions to simplify.
- Expanded set of unit tests, included a coverage badge, and added **`covr`** to Suggests.
- Reimplementation (and improved documentation) of the sentiment calculation in the `compute_sentiment()` function, by writing part of the code in **`Rcpp`** relying on **`RcppParallel`** (added to Imports); there are now three approaches to computing sentiment (unigrams, bigrams and clusters).
- Replaced the `dfm` argument in the `compute_sentiment()` and `ctr_agg()` functions by a `tokens`. argument, and altered the input and behaviour of the `nCore` argument in these same two functions.
- Switched from the **`quanteda`** package to the **`stringi`** package for more direct tokenization.
- Trimmed the `list_lexicons` and `list_valence_shifters` built-in word lists by keeping only unigrams, and included same trimming procedure in the `sento_lexicons()` function.
- Added a column type `"t"` to the `list_valence_shifters` built-in word list, and reset values of the `"y"` column from 2 to 1.8 and from 0.5 to 0.2.
- Updated the `epu` built-in dataset with the newest available series, up to July 2018.
- Corrected the word 'sparesly' to 'sparsely' in `list_valence_shifters[["en"]]`.
- Further shortened project page to the bare essence.
- Omitted statement printed ('Compute sentiment... Done.') in the `compute_sentiment()` function.
- Slightly modified `print()` generic for a `sentomeasures` object.
- Dropped the `"tf-idf"` option for within-document aggregation in the `ctr_agg()` function.
- The `sento_lexicons()` function outputs a `sentolexicons` object, which the `compute_sentiment()`. function specifically requires as an input; a `sentolexicons` object also includes a `"["` class-preserving extractor function.
- The `attributions()` function outputs an `attributions` object; the `plot_attribtutions()` function is therefore replaced by the `plot()` generic.
- Defunct the `perform_MCS()` function, but the output of the `get_loss_data()` function can easily be used as an input to the `MCSprocedure()` function from the **`MCS`** package (discarded from Imports).
- Moved the **`parallel`** and **`doParallel`** packages to Suggests, as only needed (if enacted) in the `sento_model()` function.
- Sligthly modified appearance of plotting functions, to drop **`ggthemes`** from Imports.

## sentometrics 0.4.0

- New functions: `measures_delete()`, `nmeasures()`, `nobs()`, and `to_sentocorpus()`.
- Renamed following functions: any `xyz_measures()` to `measures_xyz()`, `extract_peakdocs()` to `peakdocs()`.
- Dropped `do.normalizeAlm` argument in the `ctr_agg()` function (but kept in the `almons()` function).
- Inverted order of rows in output of the `almons()` function to be consistent with Ardia et al. (IJF, 2019) paper.
- Renamed `lexicons` to `list_lexicons`, and `valence` to `list_valence_shifters`. 
- The `stats` element of a `sentomeasures` object is now also updated in `measures_fill()`.
- Changed `"_eng"` to `"_en"`' in `list_lexicons` and `list_valence_shifters` objects, to be in accordance with two-letter ISO language naming.
- Changed `"valence_language"` naming to `"language"` in `list_valence_shifters` object.
- The `compute_sentiment()` function now also accepts a **`quanteda`** `corpus` object and a `character` vector.
- The `add_features()` function now also accepts a **`quanteda`** `corpus` object.
- Added an `nCore` argument to the `compute_sentiment()`, `ctr_agg()`, and `ctr_model()` functions to allow for (more straightforward) parallelized computations, and omitted the `do.parallel` argument in the `ctr_model()` function.
- Added a `do.difference` argument to the `ctr_model()` function and expanded the use of the already existing `oos` argument.
- Brought **`ggplot2`** and **`foreach`** to Imports.

## sentometrics 0.3.5

- Faster `to_global()`.
- Set `tolower = FALSE` of `quanteda::dfm()` constructor in `compute_sentiment()`.
- Changed `intercept` argument in `ctr_model()` to `do.intercept` for consistency.
- Proper checks on values of feature columns in `sento_corpus()` and `add_features()`.

## sentometrics 0.3.0

- New functions: `diff()`, `extract_peakdocs()`, and `subset_measures()`.
- Modified R Depends from 3.4.2 to 3.3.0, and omitted import of **`sentimentr`**.
- Word count per document now determined based on a separate tokenization.
- Improved valence shifters search (modified `incluce_valence()` helper function).
- New option added for within-document aggregation (`"proportionalPol"`).
- Now correct pass-through of `dfm` argument in `ctr_agg()`.
- Simplified `select_measures()`, but `toSelect` argument expanded.
- Calculation in `to_global()` changed (see vignette).
- Improved `add_features()`: regex and non-binary (between 0 and 1) allowed.
- All texts and lexicons now automatically to lowercase for sentiment calculation.
- (Re)translation of built-in lexicons and valence word lists.
- Small documentation clarifications and fixes.
- New vignette and run_vignette.R script.
- Shortened project page (no code example anymore).

## sentometrics 0.2.0

- First public release.

## sentometrics 0.1.0

- Google Summer of Code 2017 "release" (unstable).

