
## sentometrics 0.5.0 (development)

- new functions: `betas()`, `get_dates()`, `get_dimensions()`, and `get_measures()`
- defunct the `ctr_merge()` function, so that all merge parameters are to be passed on directly to the `measures_merge()` function
- expanded the use of the `center` and `scale` arguments in the `scale()` function
- renamed the function `to_global()` to `measures_global()` for consistency
- added the `dateBefore` and `dateAfter` arguments to the `measures_fill()` function
- added a `"beta"` time aggregation option and an associated `betas()` function
- corrected update of `attribWeights` element of output `sentomeasures` object in required `measures_xyz()` functions
- added a new attribution dimension (`"lags"`) to the `retrieve_attributions()` function, and corrected some edge cases
- dropped `NA` option in `fill` argument of `measures_fill()` function

## sentometrics 0.4.0

- new functions: `measures_delete()`, `nmeasures()`, `nobs()`, and `to_sentocorpus()`
- dropped `do.normalizeAlm` argument in `ctr_agg()`, but kept in the `almons()` function
- inverted order of rows in output of `almons()` function to be consistent with Ardia et al. (2017) paper
- renamed the functions `subset_measures()`, `select_measures()`, `merge_measures()`, and `fill_measures()` to `measures_xyz()`
- renamed the function `extract_peakdocs()` to `peakdocs()` for brevity
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
- set `tolower = FALSE` of `dfm()` constructor in `compute_sentiment()`
- changed `intercept` argument in `ctr_model()` to `do.intercept` for consistency
- proper checks on values of feature columns in `sento_corpus()` and `add_features()`

## sentometrics 0.3.0

- new functions: `diff()`, `extract_peakdocs()`, and `subset_measures()` 
- modified R depends from 3.4.2 to 3.3.0, and import of **`sentimentr`** omitted
- word count per document now determined based on a separate tokenisation
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
- CITATION file modified (year to 2018)

## sentometrics 0.2.0

- first public release

## sentometrics 0.1.0

- Google Summer of Code 2017 "release" (unstable)

