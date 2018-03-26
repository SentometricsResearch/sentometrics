
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

