
# Welcome to the **sentometrics** project page!

You collected a large number of texts and think it is a good idea to summarize your corpus into several textual sentiment time series, which you ponder could help predicting some variable you are interested in. However, you do not really know how to proceed next... Fortunately, you come across the **`sentometrics`** package, which does exactly what you need! Great!

## The R package **sentometrics**: What?

The **`sentometrics`** package is designed to do time series analysis based on textual sentiment. Put differently, it is **an integrated framework for textual sentiment time series aggregation and prediction**. It accounts for the intrinsic challenge that, for a given text, sentiment can be computed in many different ways, as well as the large number of possibilities to pool sentiment across texts and time. This additional layer of manipulation does not exist in standard text mining and time series analysis packages. As a final outcome, the package provides an automated means to econometrically model the impact of sentiment in texts on a given variable, by first computing a wide range of textual sentiment time series and then selecting those that are most informative. The package therefore integrates the _qualification_ of sentiment from texts, the _aggregation_ into different sentiment measures and the optimized _prediction_ based on these measures.

The package was first created during [Google Summer of Code 2017](https://github.com/rstats-gsoc/gsoc2017/wiki/Sentometrics:-An-integrated-framework-for-text-based-multivariate-time-series-modeling-and-forecasting). So far, the package implements the main methodology developed in the paper "[Questioning the news about economic growth: Sparse forecasting using thousands of news--based sentiment values](https://ssrn.com/abstract=2976084)" (Ardia, Bluteau and Boudt, 2017). We do believe the package in its current state may already be of strong interest to many people.

## The functionalities of **sentometrics**

In what follows, we present an overview of the different functionalities of the package. A hands--on methodological introduction is given in our [vignette](https://ssrn.com/abstract=3067734), with an accompanying code example (see the `run_vignette.R` script under the _/examples_ folder on our GitHub repository). 

### Typical workflow

Below series of steps represent a typical **`sentometrics`** package workflow, and the associated output objects and functions. The steps are generally carried out one after the other, but not necessarily so. For example, the model information from steps 4 and 5 can be exploited to further merge the sentiment measures in step 3. This serves as a broad taxonomy of what can be done and which functions are available. The functions are ordered roughly in terms of importance.

**Step 1**: Acquire and pre--process a selection of texts and generate relevant features
    
**_Functions:_** `sento_corpus()`, and `add_features()`.

**_Output:_** a `sentocorpus` object.

**Step 2**: Choose lexicons and compute document--level textual sentiment
    
**_Functions:_** `setup_lexicons()`, and `compute_sentiment()`.

**Step 3**: Aggregate the sentiment into multiple textual sentiment time series
    
**_Functions:_** `ctr_agg()`, `sento_measures()`, `ctr_merge()`, `merge_measures()`, `plot()`, `to_global()`, `subset_measures()`, `select_measures()`, `fill_measures()`, `perform_agg()`, `diff()`, `scale()`, and `summary()`.

**_Output:_** a `sentomeasures` object.

**Step 4**: Calibrate (sparse) regression model and perform (out--of--sample) predictions
    
**_Functions:_** `ctr_model()`, and `sento_model()`.

**_Output:_** a `sentomodel` or a `sentomodeliter` object.

**Step 5**: Evaluate prediction performance and retrieve sentiment attributions
    
**_Functions:_** `predict()`, `retrieve_attributions()`, `plot_attributions()`, `extract_peakdocs()`, `perform_MCS()`, `summary()`, and `plot()`.

### Textual sentiment aggregation

We impose texts and the features metadata to be structured as a `data.frame`. The texts are accompanied by columns that indicate so--called features and how they belong to the texts. For example, a feature can be termed _economy_. Indicating which texts are relevant to the economy is usually done in a binary way (1 for relevant, 0 for not relevant), or by a score between 0 and 1 (the higher, the more relevant). There are news providers that enrich their texts with such information. Otherwise, some preparatory work is needed to come up with a good mapping from texts to features. The art of topic modelling or entity recognition might help with that, but the art of common sense (i.e. human classification) may serve your purpose equally well. In the **`sentometrics`** package, the `add_features()` function is fit to to add features to your corpus, through keywords (or a regex pattern) occurrence search. 

An example corpus is present in the package and can be loaded with `data("usnews")`, to have an idea about the required textual input data. Such data structure of texts needs to be plugged into the `sento_corpus()` function, to achieve a more formal corpus data structure. We rely on the **`quanteda`** package which has a fast and well supported corpus mechanism, on which the **`sentometrics`** corpus constructor is based.

The computation of a sentiment score per document is based on the **bag--of--words** model. This approach looks for words in a text that are included in a predefined word list, called a lexicon, assigns a score to these words as given by the lexicon, and then aggregates it into one score per text. We also account for valence shifters, such as negators or amplifiers, one word away from a polarized word. Applying valence word lists in combination with lexicons gives already an accurate picture of the sentiment embedded in texts, especially when aggregated. The **`sentometrics`** package includes four well--known lexicons and a negators valence word list, available in English, French and Dutch, the latter two often as a result of a translation from English. Textual sentiment analysis gets increasingly complex if one wants to account for ambiguous word sequences, let alone entire sentences or paragraphs. We currently refrain from this complexity due to its cost in efficiency with respect to the large dimensionality of the task we face, being computing sentiment and aggregating scores in one go for thousands of texts. _A future version of the package will integrate more complex and faster sentiment analysis computation algorithms._

Word lists should be passed on to the `setup_lexicons()` function to specify which lexicons and valence shifters to use for the sentiment computation. The overall structure and validation procedure of the word lists is inspired by the **`sentimentr`** package.

We allow for many ways to aggregate sentiment within documents, across documents and across time, to arrive at fully fledged textual sentiment time series. Combining all input features, lexicons and time aggregation schemes, referred to as the three _dimensions_, gives effectively a large number of time series of textual sentiment. The sentiment aggregation specification is chosen through the `ctr_agg()` control function. The reference paper, vignette and documentation manual explain more in detail what is meant by each main aggregation argument.

All the hard work in setting up a corpus, deciding on the right lexicons (and valence word list) to include and thinking about how to aggregate sentiment bears fruit when the `sento_measures()` function is called. This function outputs a `sentomeasures` object, a list composed of several elements, including all the sentiment measures as a `data.table`, and the original document--level sentiment scores. To manipulate a `sentomeasures` object, you can use for example the `select_measures()` or `subset_measures()` functions. To plot the sentiment measures, simply use `plot()` and specify the dimension. The series are then shown as the average of all sentiment measures pertaining to each dimension's component (for instance, by each feature).

### Further sentiment merging

There are two means to further merge the sentiment measures. This can be done either before the modelling step, or after, based on an estimated model's information about the importance of each dimension and their components related to a specific target variable. 

A first means is to merge selected time series with each other as an average through the `ctr_merge()` and `merge_measures()` functions. Secondly, one can also merge all sentiment measures into single (global) dimension--specific sentiment measures. This requires to set weights to the components in the lexicons, features and time weighting dimensions; higher weights for some components will make the global sentiment measure tilt more towards the measures composed of these components. See `help("to_global")` and the vignette for an example.

### Modelling and prediction

The next step is to use the previously obtained sentiment measures as explanatory variables to predict any other variable. An example of a target variable is loaded via `data("epu")`. The underlying question is: "Does sentiment from texts achieve good (or improve) prediction performance?". 

We provide the possibility of three types of regressions: linear, binomial (logistic) and multinomial (logistic). To select the most important sentiment variables, current model implementations are all in the form of an **elastic net** regularized regression. There is salient correlation between the different sentiment variables, inherently due to similarities in aggregation schemes. We heavily rely on the **`glmnet`** package to carry out the estimation. Model calibration (meaning selection of the optimal elastic net _alpha_ and _lambda_ parameters), can be done through cross--validation or on the basis of one of three information criteria (AIC, BIC and Mallows's Cp). The information criteria are, _to date_, only available for linear models. The cross--validation is performed with the **`caret`** package. Model calibration and estimation can be speed up using parallel computation, see `help("sento_model")`.

The output of a single model run is a `sentomodel` object, and the model results are most easily displayed through the generic `summary()` function. The object contains, amongst other useful information, the fitted **`glmnet`** object. Instead of estimating the model once for the entire sample, the same analysis can be performed several times with time rolling forward for a smaller sample size. This is enacted by setting `do.iter = TRUE` in the model control function. The output is then an object of class `sentomodeliter`, in which you can find the repeated model estimations, and performance measures with respect to one--step ahead out--of--sample prediction errors.

### Post--modelling

There are two interesting post--modelling functions. The first one is inherent to the aggregation framework and allows to decompose the **attribution** to predictions of lexicons, features, time weighting schemes, and individual documents, for any provided model and its coefficients. This can be done with the `retrieve_attributions()` function.

The second function constructs a _model confidence set_ with the best models remaining, in case you want to compare the performance of several sentiment--based models with a selection of key benchmark models. For this, we deploy the **`MCS`** package and make available the simple wrapper function `perform_MCS()`.

## What's next?

We already have a few advances in mind. These mainly comprise better alignment with existing text mining packages, more advanced sentiment calculation engines, and more speedy and flexible modelling. We will continue the efforts put into this package, such that you can analyse the role of sentiment in texts on your variable(s) of interest even faster and better!

## Installation

To install the package from CRAN, simply do:

```R
install.packages("sentometrics")
```

The latest development version of **`sentometrics`** resides at [https://github.com/sborms/sentometrics](https://github.com/sborms/sentometrics). To install this version (which may still contain bugs!), execute:

```R
devtools::install_github("sborms/sentometrics")
```

When installed, you are ready to load the package...

```R
library("sentometrics")
```

... and have some fun!

## Contact

Reach out to the development team if you have questions, suggestions or anything else: [Samuel Borms](mailto:samuel.borms@unine.ch), [Keven Bluteau](mailto:keven.bluteau@unine.ch), [David Ardia](mailto:david.ardia@unine.ch) or [Kris Boudt](mailto:kris.boudt@vub.be).

