
# Welcome to the sentometrics project page!

## The sentometrics package: What?

The **`sentometrics`** package is designed to do time series analysis based on textual sentiment. Put differently, it is **an integrated framework for textual sentiment time series aggregation and prediction**. It accounts for the intrinsic challenge that, for a given text, sentiment can be computed in many different ways, as well as the large number of possibilities to pool sentiment across text and time. This additional layer of manipulation does not exist in standard text mining and time series analysis packages. As a final outcome, the package provides an automated means to econometrically model the impact of sentiment in texts on a given variable, by first computing a wide range of textual sentiment time series and then selecting those that are most informative. The package created therefore integrates the _qualification_ of sentiment from texts, the _aggregation_ into different sentiment measures and the optimized _prediction_ based on these measures.

The sentometrics R package was created during [Google Summer of Code 2017](https://github.com/rstats-gsoc/gsoc2017/wiki/Sentometrics:-An-integrated-framework-for-text-based-multivariate-time-series-modeling-and-forecasting). So far, the package implements the main methodology developed in the paper "[Questioning the news about economic growth: Sparse forecasting using thousands of news-based sentiment values](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2976084)" (Ardia, Bluteau and Boudt, 2017). We do believe the package in its current state may already be of strong interest to many people. Below, we present an overview of the different functionalities of the package, largely by means of a step-by-step example. The associated code can be found in the `run_project_page.R` script under the _/examples_ folder on the dedicated GitHub repository.

## The functionalities of sentometrics

Assume you have a collection of texts available and you have mapped the relevance of each text to a given number of metadata features. You think it might be a good idea to summarize the texts into several sentiment time series across these features, which you ponder could help predicting some variable you are interested in. However, you do not really know how to proceed next... Fortunately, you come across the `sentometrics` package, which does exactly what you need! Great! Go on and load the package to begin with, together with some plotting devices.

```R
require(sentometrics)
require(ggplot2)
require(gridExtra)
```

### Textual sentiment aggregation

We impose texts and the features metadata to be structured as a `data.frame`. The texts are accompanied by columns that indicate which feature(s) belong(s) to a certain text. For example, a feature can be termed _economy_. Indicating which texts are relevant to the economy is usually done in a binary way (1 for relevant, 0 for not relevant), or by a score (the higher, the more relevant). There are news providers that enrich their texts with such information. Otherwise, some preparatory work is needed to come up with a good mapping from texts to features. The art of topic modelling or entity recognition might help with that, but the art of common sense (i.e. human classification) may serve your purpose equally well. There is also another way to add features to your corpus, once it is already created, as we'll see later.

Back to your collection of texts. You have a few thousand articles between 1995 and 2014 from two major US journals: The Wall Street Journal and The Washington Post. Some of these articles are effectively relevant to the US economy, and by contrast the others are not. This information is captured into the four features _wsj_, _wapo_, _economy_ and _noneconomy_ respectively. The data can be accessed as shown below.

```R
data("usnews")
colnames(usnews) # id, date, text, wsj, wapo, economy, noneconomy
```

This data structure of texts needs to be plugged into a more formal corpus data structure to make it more easy to manipulate the corpus and compute sentiment. Luckily, we were able to rely on the **`quanteda`** package which has a fast and well supported corpus mechanism, on which the `sentometrics` corpus constructor is based. It checks that the original structure is respected and prepares the corpus specifically for further sentiment computation and aggregation analysis. Below code excerpt summarizes the corpus construction, and shows that (most) `quanteda` corpus routines are also applicable.

```R
corpusAll <- sento_corpus(usnews)
corpus <- quanteda::corpus_subset(corpusAll, date < "2014-10-01")
```

Coming back to the feature columns one last time, the `add_features()` function gives you the option to add in features after the creation of a corpus. It simply looks for texts that mention a specified keyword at least once and denote these texts with a value of 1. If you have no knowledge about any features, a features-less corpus input `data.frame` can be provided. In that case, a dummy feature valued at 1 throughout is added automatically to the corpus. Here, we define three new features related to politics and potential sources of uncertainty, based on the words 'war', 'election', 'president' and 'crisis'. For example, 488 texts mention the words 'election' or 'president'. This finalizes the corpus we are about to analyse.

```R
corpus <- add_features(corpus,
                       keywords = list(war = "war", pol = c("election", "president"), crisis = "crisis"))
sum(corpus$documents$pol) # 488
```

The assignment of textual sentiment is based on the **bag-of-words** model. This approach looks for words in a text that are included in a predefined word list, called a lexicon, and then assigns a score to these words as also given by the lexicon. Typically, the word 'good' carries with it a positive connotation, thus it is given a score of 1. On the other hand, 'ugly' will most likely have a negative connotation, equivalent to a score of -1. What if you have something like 'not good'? This is where valence shifters kick in. The word 'not' is a classical examply of a negator, which inverses the sentiment of the word that it precedes. _Textual sentiment analysis gets increasingly complex if one wants to account for word sequences such as 'not very ugly', let alone entire sentence or paragraph structures. We currently refrain from this complexity due to its cost in efficiency with respect to the large dimensionality of the task we face, being computing sentiment and aggregating scores in one go for a lot of texts. Future versions of the package may integrate more complex sentiment analysis computation algorithms._ Applying valence word lists in combination with lexicons gives an accurate enough picture of the sentiment embedded in a text. The `sentometrics` package includes four well-known default lexicons ([FEEL](http://www.lirmm.fr/~abdaoui/FEEL), [GI](http://www.wjh.harvard.edu/~inquirer/spreadsheet_guide.htm), [HENRY](https://study.sagepub.com/sites/default/files/1\%20Henry\%202008_0.pdf) and [LM](https://www3.nd.edu/~mcdonald/Word_Lists.html)) and a negators valence word list. The word lists are available in English (_eng_), French (_fr_) and Dutch (_nl_), the latter two often as a result of a translation from English.

```R
# loading the built-in lexicons and valence word lists
data("lexicons")
data("valence")
```

As long as you stick to the format from above built-in lexicons and valence word lists (and as explicited in the documentation), any other word list can be passed on to the `setup_lexicons()` function to specify which lexicons and valence shifters to use in the sentiment computation. This function validates the input, applies the valence word list (if included) to the lexicons, and optionally splits out the lexicons into a positive and a negative counterpart. The overall structure and validation procedure of the word lists is inspired by the **`sentimentr`** package. We continue with the Loughran & McDonald and Henry lexicons, include the built-in negators and split out each lexicon.

```R
# prepare the lexicons
lexIn <- setup_lexicons(lexiconsIn = lexicons[c("LM_eng", "HENRY_eng")],
                        valenceIn = valence[["valence_eng"]],
                        do.split = TRUE)
```

We allow for many ways to aggregate sentiment within documents, across documents and across time, to arrive at fully fledged textual sentiment time series. Combining all the input features, lexicons and aggregation options gives effectively multiple time series of textual sentiment. The sentiment aggregation specifications is chosen through the `ctr_agg()` control function. For example, if you know the variable you ultimately want to predict with sentiment is available at a monthly frequency, set `by = "month"`. Else, you can also aggregate at a daily, weekly or yearly frequency; we'll take care of it.

```R
ctrIn <- ctr_agg(howWithin = "tf-idf", 
                 howDocs = "proportional", 
                 howTime = c("equal_weight", "linear", "almon"),
                 do.ignoreZeros = TRUE,
                 by = "month", # change this if you want a time series at a different frequency
                 fill = "zero",
                 lag = 12, 
                 ordersAlm = 1:3, 
                 do.inverseAlm = TRUE, 
                 do.normalizeAlm = TRUE)
```

The reference paper, vignette and manual explain more in detail what is meant by each main aggregation argument. Here we proceed by noting that the choice of `howWithin = "tf-idf"` aggregates the word scores within documents based on the term frequency-inverse document frequency statistic, which downweights words that appear very often across all documents. The choice of `howDocs = "proportional"` considers documents with more words as more important in the pooling of sentiment across documents at the same date. Finally, the `howTime` option sets out several weighting schemes to smooth sentiment across time. To give an example of an Almon polynomial, a linear and an exponential weighting curve, see below bit of code and graph. The choice is yours! Or actually it is not, since we can compute all of the time weighting aggregation schemes you select at once, and give you the various sentiment measures.

```R
lag <- 30
a <- almons(n = lag, orders = 1:3, do.inverse = TRUE, do.normalize = FALSE)
e <- exponentials(n = lag, alphas = c(0.1))
lin <- (1:lag)/sum(1:lag)
weights <- data.frame(id = 1:lag, almon = a[, 6], exponential = e[, 1],
                      linear = lin, equal_weight = 1/lag)

p <- ggplot(data = melt(weights, id.vars = "id"), aes(x = id, y = value, color = variable)) +
  geom_line() +
  scale_x_continuous(name = "lag") +
  scale_y_continuous(trans = "reverse", name = "weight")
  ggthemes::theme_tufte(base_size = 11) +
  theme(legend.title = element_blank(), legend.position = "top")
p
```

<p align="center">
  <img src="https://raw.githubusercontent.com/ArdiaD/Sentometrics/master/docs/plots/time.png">
</p>

On to the actual creation of many sentiment measures. All the hard work in setting up your corpus, deciding on the right lexicons (and valence word list) to include and thinking about how to aggregate sentiment will soon bear fruit, because all you need is...

```R
sentMeas <- sento_measures(corpus, lexicons = lexIn, ctr = ctrIn)
summary(sentMeas)

# you can also achieve the same output in these two steps
sent <- compute_sentiment(corpus, lexicons = lexIn, how = ctrIn$howWithin)
sentMeasAlt <- perform_agg(sent, ctr = ctrIn)
```

The sentiment measures are found within a `sentomeasures` object, which is a list composed of several elements. In our example, the main element is accessed through `sentMeas$measures`, being all the sentiment measures, each of them in a separate column. Under `sentMeas$sentiment`, you find the original sentiment scores, and under `sentMeas$stats` there are a few key statistics, including the average correlation of one series with all the others. The other list elements are merely for informational purposes, such that you can easily retrieve back how the measures were computed. To select particular measures from a `sentomeasures` object, you can use the `select_measures()` function as below.

```R
# select all measures defined by the 'wsj' feature
sentMeasWSJ <- select_measures(sentMeas, toSelect = "wsj")

# select all measures defined by a linear or equally weighted time aggregation scheme
sentMeasLinEw <- select_measures(sentMeas, toSelect = c("linear", "equal_weight"),
                                 do.combine = FALSE)
```

All `sentomeasures` objects can be plotted instantly, most handily according to a particular dimension of the textual sentiment time series. The series are then shown as the average of all sentiment measures pertaining to each dimension's component (e.g. by each feature). In general, these plots can be used to compare the average evolution of all sentiment measures pertaining to each dimension. Sneak peak: if you want more detailed information about the added value of individual lexicons, features, time-weighting schemes or even documents in terms of a prediction model, this is possible too through what we call _attribution_. Read on!

```R
# plotting is very easy...
p1 <- plot(sentMeasWSJ, group = "lexicons")
p2 <- plot(scale(sentMeasLinEw), group = "time")

grid.arrange(p1, p2, nrow = 2, ncol = 1)
```

<p align="center">
  <img src="https://raw.githubusercontent.com/ArdiaD/Sentometrics/master/docs/plots/examples.png">
</p>

There are two more ways to alter the sentiment measures. A first one is to merge selected time series with each other as an average through the `ctr_merge()` and `merge_measures()` functions. In the example, two time weighting schemes, two lexicons and two features are collapsed into one by taking the average across the relevant sentiment measures. 

```R
ctrMerge <- ctr_merge(sentMeas,
                      time = list(W = c("equal_weight", "linear")),
                      lexicons = list(LEX = c("LM_eng_POS", "HENRY_eng_POS")),
                      features = list(journals = c("wsj", "wapo")),
                      do.keep = FALSE) # set to TRUE to not discard the merged individual measures
sentMeasMerged <- merge_measures(ctrMerge)
sentMeasMerged[c("features", "lexicons", "time")] # the new names replace the collapsed components
```

Last but not least, one could also merge all sentiment measures into one, global, sentiment measure. To do so, you have to give weights to the different lexicons, features and time weighting schemes; higher weights for some components will make the global sentiment measure tilt more towards the measures composed of these components. This gives a nice picture of the overall evolution of sentiment present in a set of texts on different topics, as presented in the figure.

```R
# making a global sentiment measure
glob <- to_global(sentMeas,
                  lexicons = c(0.50, 0, 0.50, 0),
                  features = c(0.10, 0.10, 0.10, 0.10, 0.20, 0.20, 0.20),
                  time = 1)
                  
g <- ggplot(data = glob, aes(x = as.Date(row.names(glob)), y = global)) +
  geom_line() +
  scale_x_date(name = "Date") +
  scale_y_continuous(name = "Sentiment") +
  ggthemes::theme_tufte(base_size = 11) +
  theme(legend.title = element_blank())
g
```

<p align="center">
  <img src="https://raw.githubusercontent.com/ArdiaD/Sentometrics/master/docs/plots/global.png">
</p>

We now have a large number of aggregated sentiment time series, encapsulated in the `sentomeasures` object named `sentMeas`. But how performant are these measures in a prediction framework? And which dimensions are most important?

### Modelling and prediction

The other main purpose of this package is to use the previously obtained sentiment measures as explanatory variables to predict any other variable. The underlying question is: "Does sentiment from texts achieve good (or improve) prediction performance?". 

We provide the possibility of three types of regressions: linear, binomial and multinomial. To select the most important sentiment variables, the models are all in the form of an elastic net regularized regression. There is salient correlation between the different sentiment variables, inherently due to similarities in aggregation schemes. A penalized regression is thus the most designated route to incorporate these variables into prediction models, shrinking the coefficients of the sentiment indices that are not important in explaining the independent variable. We heavily rely on the **`glmnet`** package to carry out this part of the analysis. Check out their online [vignette](https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html#intro) for an introduction to the models we apply, amongst others. Model calibration (meaning selection of the optimal elastic net _alpha_ and _lambda_ parameters), can be done through cross-validation or on the basis of one of three information criteria (AIC, BIC and Mallows's Cp).  

The following example displays the workflow. The `ctr_model()` function establishes the model type and the estimation strategy. Your target independent variable is the [Economic Policy Uncertainty](http://www.policyuncertainty.com/index.html) index, hereafter referred to as EPU. The EPU data can be found in the built-in `epu` dataset. 

```R
data("epu")
```

We study the explanatory power of the sentiment variables in forecasting next-month's EPU value. We first test a linear model, carrying out parameter calibration based on the Bayesian information criterion. If the target variable, the sentiment measures and potentially other explanatory variables are aligned, the `h` parameter can be adjusted to reflect the forecasting horizon. The output of a single model run is a `sentomodel` object, and the model results are most easily displayed through the generic `summary()` function. Other than that, the object contains the input values, the calibrated _alpha_ and _lambda_, other information, as well as the fitted `glmnet` object.

```R
y <- epu[epu$date >= sentMeas$measures$date[1], ]$index
length(y) == nrow(sentMeas$measures) # TRUE

# model estimation based on Bayesian information criterion (BIC)
ctrIC <- ctr_model(model = "gaussian",
                   type = "BIC",
                   h = 1, # induces one-month forecasting
                   alphas = seq(0.2, 0.8, by = 0.1), # one alpha is picked for the final regression
                   do.iter = FALSE)
outIC <- sento_model(sentMeas, y, ctr = ctrIC)
summary(outIC)
```

Binomial and multinomial logistic regressions are estimated equally easily. Have a look at below example for a binomial EPU variable, coded as being either above or below the historical average. The logic for a multinomial response variable is exactly the same, and can be tested using the series in `epu$aboveMulti`. _Currently, logistic regressions can only be calibrated through cross-validation. We will add in a future release information criteria for logistic regressions appropriate to the elastic net context, similar to what we did for the linear regression._ Doing parameter calibration by cross-validation requires only a few changes in the control function, in particular the inclusion of a training window and test window size. The cross-validation setup is as such that the model is estimated at a sample of size `trainWindow` for all possible _alpha_ and _lambda_ combinations, and prediction performance is measured for the subsequent `testWindow` out-of-sample values. The procedure is repeated in a rolling-forward way until the total input sample is exhausted, which is called _training the model_. The optimal _alpha_ and _lambda_ values are then those that minimize prediction errors across all the subsamples. The cross-validation is performed with the **`caret`** package. It may take a while to run, due to the nature of the calibration approach (however, it can be speed up using parallel computation, as explained in the package's manual).

```R
yb <- epu[epu$date >= sentMeas$measures$date[1], ]$above

# model estimation with a binomial target variable and cross-validation
ctrCVBi <- ctr_model(model = "binomial", # change this
                     type = "cv",
                     h = 1,
                     do.iter = FALSE,
                     trainWindow = 200,
                     testWindow = 20)
outBi <- sento_model(sentMeas, yb, ctr = ctrCVBi)
summary(outBi)
```

Instead of estimating the model once for the entire sample, it might be more interesting to perform the analysis several times with time rolling forward for a smaller sample size. This is enacted by setting `do.iter = TRUE`. At the same time, this will perform one-step ahead forecasts and provide an assessment of out-of-sample model performance across all iterations, both numerically and visually. Trying this out for a sample size of 5 years and only taking interest in the last 50 out-of-sample predictions, you run the code below. We also add the lag of the target variable as an explanatory variable. The output is an object of class `sentomodeliter`, in which you can find the repeated model estimations, but most importantly an overview of performance measures with respect to prediction errors. The performance measures obviously depend on whether you run a linear or a logistic regression.

```R
# adjust data to incorporate ESU's lag into the model
x <- data.frame(lag = y[-length(y)])
y <- y[-1]
datesIn <- sentMeas$measures$date[-1]
sentMeasShift <- select_measures(sentMeas, dates = datesIn)
all(c(all.equal(length(y), nrow(x)), all.equal(nrow(x), nrow(sentMeasShift$measures)))) # TRUE

# this will run 100 different model estimations and one-step ahead out-of-sample forecasts
ctrIter <- ctr_model(model = "gaussian",
                     type = "BIC",
                     h = 1,
                     alphas = seq(0.10, 0.90, by = 0.20),
                     do.iter = TRUE,
                     nSample = 60, 
                     start = 115) # iterations: length(y) - nSample - abs(h)  (- oos) - start + 1
outIter <- sento_model(sentMeasShift, y, x = x, ctr = ctrIter)
summary(outIter)
```

The list element `"performance"` has all performance measures. To inspect the out-of-sample prediction performance visually, call the generic plot function on the modelling object.

```R
r <- plot(outIter)
r
```

<p align="center">
  <img src="https://raw.githubusercontent.com/ArdiaD/Sentometrics/master/docs/plots/for.png">
</p>

Not that bad of a fit, isn't it? One could suspect it is mainly due to the lagged response variable, but re-running the exercise without this variable shows in fact it is not. Sentiment in the texts you have at hand seems to be partly driving the EPU index.

### Post-analysis

There are two interesting post-analysis functions. The first one is inherent to the aggregation framework and allows to pinpoint the attribution to predictions of lexicons, time weighting schemes, features and individual documents, for any provided model and its coefficients. This can be done by one simple function call, which outputs a list with all attributions at the respective dimensions.

```R
# to retrieve all attributions, input the corresponding modelling and sentiment measures objects
attributions <- retrieve_attributions(outIter, sentMeasShift, do.normalize = TRUE)

f <- plot_attributions(attributions, group = "features") +
  guides(fill = guide_legend(nrow = 1))
l <- plot_attributions(attributions, group = "lexicons")
t <- plot_attributions(attributions, group = "time") +
  guides(fill = guide_legend(nrow = 1))

grid.arrange(f, l, t, ncol = 1, nrow = 3)
```
 
<p align="center">
  <img src="https://raw.githubusercontent.com/ArdiaD/Sentometrics/master/docs/plots/attr.png">
</p>

As can be seen from the plots, some components are more important than others at specific prediction dates. Document-level attribution, not plotted, can be inspected to determine whether there are documents that seem more meaningful than others. An attribution analysis can give you quick insights into what is steering forecasts, which can then be used as a basis for an analysis into why. 

The second post-analysis function allows to compare several models and construct what is called a _model confidence set_ with the best models remaining. For this, we deploy the **`MCS`** package and make available the simple wrapper function `perform_MCS()`. If you want to compare different sentiment-based models or compare the performance of several sentiment-based models with a selection of key benchmark models, this is the function you should use. 

## What's next?

We already highlighted a few advances we have in mind. These mainly comprise better alignment with existing text mining packages, more advanced sentiment calculation engines, and more speedy and flexible modelling. We will continue the efforts put into this package, such that you can analyse the role of sentiment in texts on your variable(s) of interest even faster and better!

## Installation

To install the package from CRAN, simply do:

```R
install.packages("sentometrics")
```

The latest development version of `sentometrics` resides at [https://github.com/ArdiaD/Sentometrics](https://github.com/ArdiaD/Sentometrics). To install this version (which may still contain bugs!), execute:

```R
devtools::install_github("ArdiaD/Sentometrics")
```

The most up-to-date manual can be found on the package's GitHub repository.

## Contact

Reach out to the development team if you have questions, suggestions or anything else: [Samuel Borms](mailto:samuel.borms@unine.ch), [Keven Bluteau](mailto:keven.bluteau@unine.ch), [David Ardia](mailto:david.ardia@unine.ch) or [Kris Boudt](mailto:kris.boudt@vub.be).

