
#' Set up control for sentiment measures-based regression modelling
#'
#' @author Samuel Borms, Keven Bluteau
#'
#' @description Sets up control object for linear or nonlinear modelling of a response variable onto a large panel of
#' textual sentiment measures (and potentially other variables). See \code{\link{sento_model}} for details on the
#' estimation and calibration procedure.
#'
#' @param model a \code{character} vector with one of the following: "\code{gaussian}" (linear regression), "\code{binomial}"
#' (binomial logistic regression), or "\code{multinomial}" (multinomial logistic regression).
#' @param type a \code{character} vector indicating which model calibration approach to use. Supports "\code{BIC}",
#' "\code{AIC}" and "\code{Cp}" (Mallows's Cp) as sparse regression adapted information criteria (cf., ``On the `degrees of
#' freedom' of the LASSO''; Zou, Hastie, Tibshirani et al., 2007), and "\code{cv}" (cross-validation based on the
#' \code{\link[caret]{train}} function from the \pkg{caret} package). The adapted information criteria are currently
#' only available for a linear regression.
#' @param do.intercept a \code{logical}, \code{TRUE} by default fits an intercept.
#' @param h an \code{integer} value that shifts the time series to have the desired prediction setup; \code{h = 0} means
#' no change to the input data (nowcasting assuming data is aligned properly), \code{h > 0} shifts the dependent variable by
#' \code{h} periods (i.e. rows) further in time (forecasting), \code{h < 0} shifts the independent variables by \code{h}
#' periods.
#' @param alphas a \code{numeric} vector of the different alphas to test for during calibration, between 0 and 1. A value of
#' 0 pertains to Ridge regression, a value of 1 to LASSO regression; values in between are pure elastic net. The lambda
#' values tested for are chosen by the \code{\link[glmnet]{glmnet}} function or set to \code{10^seq(2, -2, length.out = 100)}
#' in case of cross-validation.
#' @param trainWindow a positive \code{integer} as the size of the training sample in cross-validation (ignored if
#' \code{type != } "\code{cv}").
#' @param testWindow a positive \code{integer} as the size of the test sample in cross-validation (ignored if \code{type != }
#' "\code{cv}").
#' @param oos a non-negative \code{integer} to indicate the number of periods to skip from the end of the cross-validation
#' training sample (out-of-sample) up to the test sample (ignored if \code{type != } "\code{cv}").
#' @param do.iter a \code{logical}, \code{TRUE} induces an iterative estimation of models at the given \code{nSample} size and
#' performs the associated one-step ahead out-of-sample prediction exercise through time.
#' @param do.progress a \code{logical}, if \code{TRUE} progress statements are displayed during model calibration.
#' @param nSample a positive \code{integer} as the size of the sample for model estimation at every iteration (ignored if
#' \code{iter = FALSE}).
#' @param start a positive \code{integer} to indicate at which point the iteration has to start (ignored if
#' \code{iter = FALSE}). For example, given 100 possible iterations, \code{start = 70} leads to model estimations
#' only for the last 31 samples.
#' @param do.parallel a \code{logical}, if \code{TRUE} the \code{\%dopar\%} construct from the \pkg{foreach} package is
#' applied for iterative model estimation. A proper parallel backend needs to be set up to make it work. No progress statements
#' are displayed whatsoever when \code{TRUE}. For cross-validation models, parallelization can also be carried out for
#' single-run models, whenever a parallel backend is set up. See the examples in \code{\link{sento_model}}.
#'
#' @return A \code{list} encapsulating the control parameters.
#'
#' @seealso \code{\link{sento_model}}
#'
#' @examples
#' # information criterion based model control functions
#' ctrIC1 <- ctr_model(model = "gaussian", type = "BIC", do.iter = FALSE, h = 0,
#'                     alphas = seq(0, 1, by = 0.10))
#' ctrIC2 <- ctr_model(model = "gaussian", type = "AIC", do.iter = TRUE, h = 0, nSample = 100)
#'
#' # cross-validation based model control functions
#' ctrCV1 <- ctr_model(model = "gaussian", type = "cv", do.iter = FALSE, h = 0,
#'                     trainWindow = 250, testWindow = 4, oos = 0, do.progress = TRUE)
#' ctrCV2 <- ctr_model(model = "binomial", type = "cv", h = 0, trainWindow = 250,
#'                     testWindow = 4, oos = 0, do.progress = TRUE)
#' ctrCV3 <- ctr_model(model = "multinomial", type = "cv", h = 0, trainWindow = 250,
#'                     testWindow = 4, oos = 0, do.progress = TRUE)
#' ctrCV4 <- ctr_model(model = "gaussian", type = "cv", do.iter = TRUE, h = 0, trainWindow = 45,
#'                     testWindow = 4, oos = 0, nSample = 70, do.progress = TRUE)
#'
#' @export
ctr_model <- function(model = c("gaussian", "binomial", "multinomial"), type = c("BIC", "AIC", "Cp", "cv"),
                      do.intercept = TRUE, do.iter = FALSE, h = 0, alphas = seq(0, 1, by = 0.20),
                      nSample = NULL, trainWindow = NULL, testWindow = NULL, oos = 0, start = 1,
                      do.progress = TRUE, do.parallel = FALSE) {

  if (length(model) > 1) model <- model[1]
  else if (!(model %in% c("gaussian", "binomial", "multinomial")))
    stop("Provide a proper modelling type.")

  if (!is.logical(do.intercept))
    stop("The argument 'do.intercept' should be a logical.")

  if (length(type) > 1) type <- type[1]
  else if (!(type %in% c("BIC", "AIC", "Cp", "cv")))
    stop("Provide a proper calibration type.")

  if (model != "gaussian" & type != "cv")
    stop("Elastic net-specific information criteria are currently only supported for linear models, please opt for 'cv'.")

  if (oos < 0 | start <= 0)
    stop("Make sure all integer inputs are non-negative or positive for those required.")

  if (min(alphas) < 0 | max(alphas) > 1)
    stop("Each alpha value in alphas must be between 0 and 1, inclusive.")

  if (do.iter == FALSE) nSample <- start <- NULL

  if (do.iter == TRUE & is.null(nSample))
    stop("Iterative modelling requires a non-NULL sample size given by nSample.")

  if (!is.null(nSample))
    if (nSample <= 0)
      stop("Make sure all integers are non-negative or positive as required.")

  if (type == "cv" & (is.null(trainWindow) | is.null(testWindow)))
    stop("Cross-validation requires a non-NULL training and test window size give by trainWindow and testWindow.")

  if (!is.null(trainWindow))
    if (trainWindow <= 0)
      stop("Make sure trainWindow is positive as required.")

  if (!is.null(testWindow))
    if (testWindow <= 0)
      stop("Make sure testWindow is positive as required.")

  if (!is.null(nSample) & !is.null(trainWindow) & !is.null(testWindow))
    if ((trainWindow + oos + testWindow) >= nSample)
      stop("(trainWindow + oos + testWindow) >= nSample. Adjust windows selection accordingly.")

  ctr_model <- list(model = model,
                    type = type,
                    intercept = do.intercept,
                    do.iter = do.iter,
                    h = h,
                    nSample = nSample,
                    start = start,
                    oos = oos,
                    alphas = alphas,
                    trainWindow = trainWindow,
                    testWindow = testWindow,
                    do.progress = do.progress,
                    do.parallel = do.parallel)

  return(ctr_model)
}

#' Optimized and automated sparse regression
#'
#' @author Samuel Borms, Keven Bluteau
#'
#' @description Linear or nonlinear penalized regression of any dependent variable on the wide number of sentiment measures and
#' potentially other explanatory variables. Either performs a regression given the provided variables at once, or computes
#' regressions sequentially for a given sample size over a longer time horizon, with associated one-step ahead prediction
#' performance metrics.
#'
#' @details Models are computed using the elastic net regularization as implemented in the \pkg{glmnet} package, to account for
#' the multidimensionality of the sentiment measures. Additional explanatory variables are not subject to shrinkage. Independent
#' variables are normalized in the regression process, but coefficients are returned in their original space. For a helpful
#' introduction to \pkg{glmnet}, we refer to their
#' \href{https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html#lin}{vignette}. The optimal elastic net parameters
#' \code{lambda} and \code{alpha} are calibrated either through a to specify information criterion or through
#' cross-validation (based on the "rolling forecasting origin" principle, using the \code{\link[caret]{train}} function).
#' In the latter case, the training metric is automatically set to \code{"RMSE"} for a linear model and to \code{"Accuracy"}
#' for a logistic model. We suppress many of the details that can be supplied to the \code{\link[glmnet]{glmnet}} and
#' \code{\link[caret]{train}} functions we rely on, for the sake of user-friendliness.
#'
#' @param sentomeasures a \code{sentomeasures} object created using \code{\link{sento_measures}}. There should be at least
#' two explanatory variables including the ones provided through the \code{x} argument.
#' @param y a one-column \code{data.frame} or a \code{numeric} vector capturing the dependent (response) variable. In case of
#' a logistic regression, the response variable is either a \code{factor} or a \code{matrix} with the factors represented by
#' the columns as binary indicators, with the second factor level or column as the reference class in case of a binomial
#' regression. No \code{NA} values are allowed.
#' @param x a named \code{data.frame} with other explanatory variables as \code{numeric}, by default set to \code{NULL}.
#' @param ctr output from a \code{\link{ctr_model}} call.
#'
#' @return If \code{ctr$do.iter = FALSE}, a \code{sentomodel} object which is a \code{list} containing:
#' \item{reg}{optimized regression, i.e. a model-specific \code{glmnet} object.}
#' \item{model}{the input argument \code{ctr$model}, to indicate the type of model estimated.}
#' \item{x}{a \code{matrix} of the values used in the regression for all explanatory variables.}
#' \item{alpha}{calibrated alpha.}
#' \item{lambda}{calibrated lambda.}
#' \item{trained}{output from \code{\link[caret]{train}} call (if \code{ctr$type =} "\code{cv}").}
#' \item{ic}{a \code{list} composed of two elements: the information criterion used in the calibration under
#' \code{"criterion"}, and a vector of all minimum information criterion values for each value in \code{alphas}
#' under \code{"opts"} (if \code{ctr$type !=} "\code{cv}").}
#' \item{dates}{sample reference dates as a two-element \code{character} vector, being the earliest and most recent date from
#' the \code{sentomeasures} object accounted for in the estimation window.}
#' \item{nVar}{the sum of the number of sentiment measures and other explanatory variables inputted.}
#' \item{discarded}{a named \code{logical} vector of length equal to the number of sentiment measures, in which \code{TRUE}
#' indicates that the particular sentiment measure has not been considered in the regression process. A sentiment measure is
#' not considered when it is a duplicate of another, or when at least 25\% of the observations are equal to zero.}
#'
#' @return If \code{ctr$do.iter = TRUE}, a \code{sentomodeliter} object which is a \code{list} containing:
#' \item{models}{all sparse regressions, i.e. separate \code{sentomodel} objects as above, as a \code{list} with as names the
#' dates from the perspective of the sentiment measures at which predictions for performance measurement are carried out (i.e.
#' one date step beyond the date \code{sentomodel$dates[2]}).}
#' \item{alphas}{calibrated alphas.}
#' \item{lambdas}{calibrated lambdas.}
#' \item{performance}{a \code{data.frame} with performance-related measures, being "\code{RMSFE}" (root mean squared
#' forecasting error), "\code{MAD}" (mean absolute deviation), "\code{MDA}" (mean directional accuracy, in which's calculation
#' zero is considered as a positive; in percentage points), "\code{accuracy}" (proportion of correctly predicted classes in case
#' of a logistic regression; in percentage points), and each's respective individual values in the sample. Directional accuracy
#' is measured by comparing the change in the realized response with the change in the prediction between two consecutive time
#' points (omitting the very first prediction, resulting in \code{NA}). Only the relevant performance statistics are given
#' depending on the type of regression. Dates are as in the \code{"models"} output element, i.e. from the perspective of the
#' sentiment measures.}
#'
#' @seealso \code{\link{ctr_model}}, \code{\link[glmnet]{glmnet}}, \code{\link[caret]{train}},
#' \code{\link{retrieve_attributions}}
#'
#' @examples
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#' data("epu", package = "sentometrics")
#'
#' # construct a sentomeasures object to start with
#' corpusAll <- sento_corpus(corpusdf = usnews)
#' corpus <- quanteda::corpus_subset(corpusAll, date >= "2004-01-01" & date < "2014-10-01")
#' l <- setup_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#' ctr <- ctr_agg(howWithin = "tf-idf", howDocs = "proportional",
#'                howTime = c("equal_weight", "linear"),
#'                by = "month", lag = 3)
#' sentomeasures <- sento_measures(corpus, l, ctr)
#'
#' # prepare y and other x variables
#' y <- epu[epu$date >= sentomeasures$measures$date[1], ]$index
#' length(y) == nobs(sentomeasures) # TRUE
#' x <- data.frame(runif(length(y)), rnorm(length(y))) # two other (random) x variables
#' colnames(x) <- c("x1", "x2")
#'
#' # a linear model based on the Akaike information criterion
#' ctrIC <- ctr_model(model = "gaussian", type = "AIC", do.iter = FALSE, h = 0)
#' out1 <- sento_model(sentomeasures, y, x = x, ctr = ctrIC)
#'
#' # some post-analysis (attribution and prediction)
#' attributions1 <- retrieve_attributions(out1, sentomeasures,
#'                                        refDates = sentomeasures$measures$date[20:40])
#'
#' nx <- nmeasures(sentomeasures) + ncol(x)
#' newx <- runif(nx) * cbind(sentomeasures$measures[, -1], x)[30:40, ]
#' preds <- predict(out1, newx = as.matrix(newx), type = "link")
#'
#' \dontrun{
#' # a cross-validation based model
#' cl <- parallel::makeCluster(detectCores() - 2)
#' doParallel::registerDoParallel(cl)
#' ctrCV <- ctr_model(model = "gaussian", type = "cv", do.iter = FALSE,
#'                    h = 0, alphas = c(0.10, 0.50, 0.90), trainWindow = 70,
#'                    testWindow = 10, oos = 0, do.progress = TRUE)
#' out2 <- sento_model(sentomeasures, y, x = x, ctr = ctrCV)
#' parallel::stopCluster(cl)
#' summary(out2)
#'
#' # a cross-validation based model but for a binomial target
#' yb <- epu[epu$date >= sentomeasures$measures$date[1], ]$above
#' ctrCVb <- ctr_model(model = "binomial", type = "cv", do.iter = FALSE,
#'                     h = 0, alphas = c(0.10, 0.50, 0.90), trainWindow = 70,
#'                     testWindow = 10, oos = 0, do.progress = TRUE)
#' out3 <- sento_model(sentomeasures, yb, x = x, ctr = ctrCVb)
#' summary(out3)}
#'
#' \dontrun{
#' # an example of an iterative analysis
#' ctrIter <- ctr_model(model = "gaussian", type = "BIC", do.iter = TRUE,
#'                      alphas = c(0.25, 0.75), h = 0, nSample = 100, start = 21)
#' out4 <- sento_model(sentomeasures, y, x = x, ctr = ctrIter)
#' summary(out4)
#'
#' attributions2 <- retrieve_attributions(out4, sentomeasures)
#' plot_attributions(attributions2, "features")
#'
#' # a similar iterative analysis, parallelized
#' cl <- parallel::makeCluster(detectCores() - 2)
#' doParallel::registerDoParallel(cl)
#' ctrIter <- ctr_model(model = "gaussian", type = "Cp", do.iter = TRUE,
#'                      h = 0, nSample = 100, do.parallel = TRUE)
#' out5 <- sento_model(sentomeasures, y, x = x, ctr = ctrIter)
#' parallel::stopCluster(cl)
#' summary(out5)}
#'
#' @importFrom glmnet predict.glmnet predict.elnet predict.lognet predict.multnet
#' @import foreach
#' @export
sento_model <- function(sentomeasures, y, x = NULL, ctr) {
  check_class(sentomeasures, "sentomeasures")

  if (any(is.na(y))) stop("No NA values are allowed in y.")
  nrows <- c(nobs(sentomeasures), ifelse(is.null(nrow(y)), length(y), nrow(y)), nrow(x))
  if (length(unique(nrows)) != 1)
    stop("Number of rows or length for y, x and measures in sentomeasures must be equal.")
  if (sum(nmeasures(sentomeasures) + ifelse(is.null(x), 0, ncol(x))) < 2)
    stop("There should be at least two explanatory variables out of sentomeasures and x combined.")
  if (ctr$model == "binomial" && ifelse(is.factor(y), nlevels(y), NCOL(y)) > 2)
    stop("At maximum two classes allowed in 'y' for a binomial model.")
  if (ctr$model == "multinomial" && !(ifelse(is.factor(y), nlevels(y), NCOL(y)) > 2))
    stop("At least three classes needed in 'y' for a multinomial model.")

  family <- ctr$model
  type <- ctr$type
  intercept <- ctr$intercept
  do.iter <- ctr$do.iter
  h <- ctr$h
  alphas <- ctr$alphas
  do.progress <- ctr$do.progress
  trainWindow <- ctr$trainWindow # used when type is "cv"
  testWindow <- ctr$testWindow # used when type is "cv"
  oos <- ctr$oos # used when type is cv"
  nSample <- ctr$nSample # used when do.iter is TRUE
  start <- ctr$start # used when do.iter is TRUE
  do.parallel <- ctr$do.parallel # used when do.iter is TRUE

  if (do.iter == TRUE) {
    out <- sento_model_iter(sentomeasures, y = y, x = x, h = h, family = family, intercept = intercept,
                            alphas = alphas, type = type, nSample = nSample, start = start,
                            oos = oos, trainWindow = trainWindow, testWindow = testWindow,
                            do.progress = do.progress, do.parallel = do.parallel)
  } else {
    if (type == "cv") {
      out <- model_CV(sentomeasures, y = y, x = x, h = h, family = family, intercept = intercept,
                      alphas = alphas, trainWindow = trainWindow, testWindow = testWindow,
                      oos = oos, do.progress = do.progress)
    } else {
      out <- model_IC(sentomeasures, y = y, x = x, h = h, family = family, intercept = intercept,
                      alphas = alphas, ic = type, do.progress = do.progress)
    }
  }

  return(out)
}

.model_IC <- function(sentomeasures, y, x, h, family, intercept,
                      alphas, ic, do.progress, ...) {

  # inputs i and nSample are NULL if one-shot model (not iterative)
  dots <- list(...)
  i <- dots$i
  nSample <- dots$nSample

  alignedVars <- align_variables(y, sentomeasures, x, h, i = i, nSample = nSample)
  yy <- alignedVars$y
  xx <- alignedVars$x # changed x to include sentiment measures
  nVar <- ncol(xx) # original number of explanatory variables (i.e. before cleaning)
  nx <- ifelse(is.null(x), 0, ncol(x))
  cleaned <- clean_panel(xx, nx = nx) # get rid of duplicated or too sparse sentiment measures
  xx <- cleaned$xNew
  discarded <- cleaned$discarded
  sampleDates <- c(alignedVars$datesX[1], alignedVars$datesX[nrow(xx)])

  penalty <- rep(1, ncol(xx))
  if (!is.null(x)) penalty[(ncol(xx) - nx + 1):ncol(xx)] <- 0 # no shrinkage for original x variables

  minIC <- as.list(numeric(length(alphas)))
  names(minIC) <- as.character(alphas)
  lambdaVals <- numeric(length(alphas))
  for (i in seq_along(alphas)) {
    alpha <- alphas[i]
    if (do.progress == TRUE) {
      if (alpha == alphas[1]) cat("alphas run: ", alpha, ", ", sep = "")
      else if (alpha == alphas[length(alphas)]) cat(alpha, "\n")
      else cat(alpha, ", ", sep = "")
    }
    reg <- glmnet::glmnet(x = xx, y = yy, penalty.factor = penalty, intercept = intercept,
                          alpha = alpha, standardize = TRUE, family = family)
    IC <- compute_IC(reg, yy, xx, alpha, ic, family = family) # vector of ic values for lambdas sequence in reg
    minIC[[i]] <- suppressWarnings(min(IC, na.rm = TRUE))
    lambdaTmp <- ifelse(length(reg$lambda[which.min(IC)]) > 0, reg$lambda[which.min(IC)], Inf)
    lambdaVals[i] <- lambdaTmp
  }

  # retrieve optimal alphas and lambdas
  minLoc <- which.min(minIC)
  lambdaOpt <- lambdaVals[minLoc]
  alphaOpt <- alphas[minLoc]

  # actual elastic net optimization
  regOpt <- glmnet::glmnet(x = xx, y = yy, penalty.factor = penalty, lambda = lambdaOpt, alpha = alphaOpt,
                           standardize = TRUE, family = family, intercept = intercept)

  out <- list(reg = regOpt,
              model = family,
              x = xx,
              alpha = alphaOpt,
              lambda = lambdaOpt,
              ic = list(criterion = ic, opts = unlist(minIC)),
              dates = sampleDates,
              nVar = nVar,
              discarded = discarded)

  class(out) <- c("sentomodel")

  return(out)
}

#' @importFrom compiler cmpfun
model_IC <- compiler::cmpfun(.model_IC)

.model_CV <- function(sentomeasures, y, x, h, family, intercept, alphas,
                      trainWindow, testWindow, oos, do.progress, ...) {

  # inputs i and nSample are NULL if one-shot model (not iterative)
  dots <- list(...)
  i <- dots$i
  nSample <- dots$nSample

  alignedVars <- align_variables(y, sentomeasures, x, h, i = i, nSample = nSample)
  yy <- alignedVars$y
  xx <- alignedVars$x # changed x to include sentiment measures
  nVar <- ncol(xx) # original number of explanatory variables (i.e. before cleaning)
  nx <- ifelse(is.null(x), 0, ncol(x))
  cleaned <- clean_panel(xx, nx = nx) # get rid of duplicated or too sparse sentiment measures
  xx <- cleaned$xNew
  discarded <- cleaned$discarded
  sampleDates <- c(alignedVars$datesX[1], alignedVars$datesX[nrow(xx)])

  penalty <- rep(1, ncol(xx))
  if (!is.null(x)) penalty[(ncol(xx) - nx + 1):ncol(xx)] <- 0 # no shrinkage for original x variables

  # train model based on slices in sliced
  sliced <- create_cv_slices(1:nrow(yy), trainWindow, testWindow = testWindow, skip = oos, do.reverse = FALSE)
  ctrTrain <- caret::trainControl(index = sliced$train, indexOut = sliced$test, allowParallel = TRUE)
  tuneGrid <- expand.grid(alpha = alphas, lambda = 10^seq(2, -2, length.out = 100))

  # change y variable to format required in caret::train function
  if (family == "gaussian") yyy <- yy[, 1]
  else yyy <- as.factor(colnames(yy)[yy %*% 1:ncol(yy)])

  # align training metric based on whether family is a linear or classification model
  metric <- ifelse(family == "gaussian", "RMSE", "Accuracy")

  if (do.progress == TRUE) cat("Training model... ")
  trained <- caret::train(x = xx, y = yyy, method = "glmnet", family = family,
                          standardize = TRUE, penalty.factor = penalty,
                          trControl = ctrTrain, tuneGrid = tuneGrid, metric = metric)
  if (do.progress == TRUE) cat("Done.", "\n")

  # retrieve optimal alphas and lambdas
  alphaOpt <- as.numeric(trained$bestTune[1])
  lambdaOpt <- as.numeric(trained$bestTune[2])

  # actual elastic net optimization
  regOpt <- glmnet::glmnet(x = xx, y = yy, penalty.factor = penalty, intercept = intercept,
                           lambda = lambdaOpt, alpha = alphaOpt, standardize = TRUE, family = family)

  out <- list(reg = regOpt,
              model = family,
              x = xx,
              alpha = alphaOpt,
              lambda = lambdaOpt,
              trained = trained,
              dates = sampleDates,
              nVar = nVar,
              discarded = discarded)

  class(out) <- c("sentomodel")

  return(out)
}

#' @importFrom compiler cmpfun
model_CV <- compiler::cmpfun(.model_CV)

.sento_model_iter <- function(sentomeasures, y, x, h, family, intercept, alphas, type,
                              nSample, start, trainWindow, testWindow, oos,
                              do.progress, do.parallel) {

  nIter <- ifelse(is.null(nrow(y)), length(y), nrow(y)) - nSample - abs(h) - oos
  if (nIter <= 0 || start > nIter)
    stop("Data not sufficient to do at least one iteration for given sample size, horizon, out-of-sample skip and start.")

  if (type == "cv") fun <- model_CV
  else fun <- model_IC

  # perform all regressions
  if (do.parallel == TRUE) {
    regsOpt <- foreach::foreach(i = start:nIter) %dopar% {
      return(fun(sentomeasures, y, x, h, alphas, intercept = intercept, trainWindow = trainWindow, testWindow = testWindow,
                 oos = oos, ic = type, do.progress = FALSE, i = i, nSample = nSample, family = family))
      }
  } else {
    regsOpt <- lapply(start:nIter, function(i) {
      if (do.progress == TRUE) cat("iteration: ", (i - start + 1), " from ", (nIter - start + 1), "\n", sep = "")
      return(fun(sentomeasures, y, x, h, alphas, intercept = intercept, trainWindow = trainWindow, testWindow = testWindow,
                 oos = oos, ic = type, do.progress = do.progress, i = i, nSample = nSample, family = family))
      })
  }

  # get optimal alphas and lambdas
  alphasOpt <- sapply(regsOpt, function(x) return(x$alpha))
  lambdasOpt <- sapply(regsOpt, function(x) return(x$lambda))

  # prepare for and get all predictions
  alignedVarsAll <- align_variables(y, sentomeasures, x, h)
  xPred <- alignedVarsAll$x[(start + nSample):(nIter + nSample), , drop = FALSE]
  yReal <- alignedVarsAll$y[(start + nSample + oos):(nIter + nSample + oos), , drop = FALSE]
  datesX <- alignedVarsAll$datesX[(start + nSample):(nIter + nSample)] # dates from perspective of x at which forecast is made
  names(regsOpt) <- datesX
  if (family %in% c("binomial", "multinomial")) {
    n <- length(colnames(yReal)) # number of factor levels
    yEst <- matrix(rep(NA, n * (nIter - start + 1)), ncol = n)
    colnames(yEst) <- colnames(yReal)
    yEstClass <- rep(NA, nIter - start + 1)
  } else {
    yEst <- rep(NA, nIter - start + 1)
    yEstClass <- NULL
  }
  for (j in 1:(nIter - start + 1)) {
    reg <- regsOpt[[j]]
    newx <- xPred[j, , drop = FALSE]
    if (family == "gaussian") {
      yEst[j] <- stats::predict(reg, newx = newx, type = "link")
    } else if (family == "binomial") {
      yEst[j, 2] <- stats::predict(reg, newx = newx, type = "response") # second factor
      yEst[j, 1] <- 1 - yEst[j, 2]
      yEstClass[j] <- stats::predict(reg, newx = newx, type = "class")
    } else if (family == "multinomial") {
      yEst[j, ] <- stats::predict(reg, newx = newx, type = "response")[1, , ]
      yEstClass[j] <- as.character(stats::predict(reg, newx = newx, type = "class"))
    }
  }

  # compute model performance
  performance <- model_performance(yEst = yEst, yReal = yReal, family = family, dates = datesX, yEstClass = yEstClass)

  out <- list(models = regsOpt,
              alphas = alphasOpt,
              lambdas = lambdasOpt,
              performance = performance)

  class(out) <- c("sentomodeliter")

  return(out)
}

#' @importFrom compiler cmpfun
sento_model_iter <- compiler::cmpfun(.sento_model_iter)

compute_IC <- function(reg, y, x, alpha, ic, family) {
  beta <- reg$beta
  lambda <- reg$lambda
  if (family == "gaussian") type <- "link"
  else stop("Calibration via information criteria to implement for 'binomial' and 'multinomial'.")
  yEst <- stats::predict(reg, newx = x, type = type)
  # dfA <- compute_df_old(alpha, beta, lambda, x)
  xScaled <- scale(x)
  xA <- lapply(1:length(lambda), function(i) return(as.matrix(xScaled[, which(beta[, i] != 0)])))
  dfA <- compute_df(alpha, lambda, xA)
  RSS <- apply(yEst, 2, function(est) return(sum((y - est)^2)))
  # sigma2 <- RSS[length(RSS)] / (nrow(y) - dfA[length(RSS)])
  sigma2 <- mean(RSS, na.rm = TRUE) / (nrow(y) - mean(dfA, na.rm = TRUE)) # averaged, else bias towards high lambda and alpha
  if (ic == "BIC")
    return(compute_BIC(y, yEst, dfA, RSS, sigma2))
  else if (ic == "AIC")
    return(compute_AIC(y, yEst, dfA, RSS, sigma2))
  else if (ic == "Cp")
    return(compute_Cp(y, yEst, dfA, RSS, sigma2))
}

model_performance <- function(yEst, yReal, family, dates, ...) {

  dots <- list(...)

  if (family == "gaussian") {
    dirAcc <- c(NA, as.numeric(sign(diff(yReal)) == sign(diff(yEst))))
    error <- yEst - yReal
    error2 <- error^2
    absDev <- abs(error)
    errors <- data.frame(cbind(dirAcc, error, error2, absDev))
    colnames(errors) <- c("DA", "error", "errorSq", "AD")
    meanErrors <- colMeans(errors[, -2], na.rm = TRUE)

    raw <- data.frame(response = yReal, predicted = yEst, errors)
    row.names(raw) <- dates

    errorsAll <- list(raw = raw,
                      MDA = as.numeric(meanErrors["DA"]) * 100,
                      RMSFE = as.numeric(sqrt(meanErrors["errorSq"])),
                      MAD = as.numeric(meanErrors["AD"]))

  } else if (family %in% c("binomial", "multinomial")) {
    yRealClass <- as.factor(colnames(yReal)[yReal %*% 1:ncol(yReal)])
    yEstClass <- dots$yEstClass
    accuracy <- as.numeric(yRealClass == yEstClass)
    accuracyProb <- (sum(accuracy)/length(accuracy)) * 100

    raw <- data.frame(response = yRealClass, predicted = yEstClass, accuracy = accuracy)
    row.names(raw) <- dates

    errorsAll <- list(raw = raw, accuracy = accuracyProb)
  }

  return(errorsAll)
}

#' @export
summary.sentomodel <- function(object, ...) {
  sentomodel <- object
  reg <- sentomodel$reg
  if ("ic" %in% names(sentomodel)) {
    printCalib <- paste0("via ", sentomodel$ic[[1]], " information criterion")
  } else {
    printCalib <- paste0("via cross-validation; ",
                         "ran through ", nrow(sentomodel$trained$resample), " samples of size ",
                         length(sentomodel$trained$control$index[[1]]),
                         ", selection based on ", sentomodel$trained$metric, " metric")
  }
  cat("\n")
  cat("Model specifications \n")
  cat(rep("-", 20), "\n \n")
  cat("Model type:", sentomodel$model, "\n")
  cat("Calibration:", printCalib, "\n")
  cat("Number of observations:", reg$nobs, "\n")
  cat("Optimal elastic net alpha parameter:", round(sentomodel$alpha, 2), "\n")
  cat("Optimal elastic net lambda parameter:", round(reg$lambda, 2), "\n \n")
  if (sentomodel$model != "multinomial") {
    cat("Non-zero coefficients \n")
    cat(rep("-", 20), "\n")
    print(nonzero_coeffs(reg))
    cat("\n \n")
  } else {
    cat("Number of non-zero coefficients per level (excl. intercept, incl. non-sentiment variables) \n")
    cat(rep("-", 20), "\n")
    nonZeros <- as.data.frame(sentomodel$reg$dfmat)
    colnames(nonZeros) <- NULL
    print(nonZeros)
    cat("\n \n")
  }
  cat("In-sample performance \n")
  cat(rep("-", 20), "\n \n")
  cat("Fraction of deviance explained: ", round(reg$dev.ratio * 100, 2), "% \n \n")
}

#' @export
print.sentomodel <- function(x, ...) {
  cat("A sentomodel object.")
}

#' @export
summary.sentomodeliter <- function(object, ...) {
  sentomodeliter <- object
  sentomodel <- sentomodeliter$models[[1]] # first sentomodel object as representative object
  model <- sentomodel$model
  reg <- sentomodel$reg
  if ("ic" %in% names(sentomodel)) {
    printCalib <- paste0("via ", sentomodel$ic[[1]], " information criterion")
  } else {
    printCalib <- paste0("via cross-validation; ",
                         "Ran through ", nrow(sentomodel$trained$resample), " samples of size ",
                         length(sentomodel$trained$control$index[[1]]),
                         ", selection based on ", sentomodel$trained$metric, " metric")
  }
  cat("\n")
  cat("Model specifications \n")
  cat(rep("-", 20), "\n \n")
  cat("Model type:", sentomodel$model, "\n")
  cat("Calibration:", printCalib, "\n")
  cat("Sample size:", reg$nobs, "\n")
  cat("Total number of iterations/predictions:", length(sentomodeliter$models), "\n")
  cat("Optimal average elastic net alpha parameter:", round(mean(sentomodeliter$alphas, na.rm = TRUE), 2), "\n")
  cat("Optimal average elastic net lambda parameter:", round(mean(sentomodeliter$lambdas, na.rm = TRUE), 2), "\n \n")
  cat("Out-of-sample performance \n")
  cat(rep("-", 20), "\n \n")
  if (model == "gaussian") {
    cat("Mean directional accuracy:", round(sentomodeliter$performance$MDA, 2), "% \n")
    cat("Root mean squared prediction error:", round(sentomodeliter$performance$RMSFE, 2), "\n")
    cat("Mean absolute deviation:", round(sentomodeliter$performance$MAD, 2), "\n \n")

  } else {
    cat("Accuracy:", sentomodeliter$performance$accuracy, "% \n \n")
  }
  cat("In-sample performance \n")
  cat(rep("-", 20), "\n \n")
  cat("Average fraction of deviance explained:",
      round(mean(sapply(sentomodeliter$models, function(m) return(m$reg$dev.ratio)), na.rm = TRUE) * 100, 2), "% \n \n")
}

#' @export
print.sentomodeliter <- function(x, ...) {
  cat("A sentomodeliter object.")
}

#' Plot iterative predictions versus realized values
#'
#' @author Samuel Borms
#'
#' @method plot sentomodeliter
#'
#' @description Displays a plot of all predictions made through the iterative model computation as incorporated in the
#' input \code{sentomodeliter} object, as well as the corresponding true values.
#'
#' @param x a \code{sentomodeliter} object created using \code{\link{sento_model}}.
#' @param ... not used.
#'
#' @return Returns a simple \code{\link{ggplot}} object, which can be added onto (or to alter its default elements) by using
#' the \code{+} operator (see examples).
#'
#' @examples
#' \dontrun{
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#' data("epu", package = "sentometrics")
#'
#' # construct a sentomeasures object to start with
#' corpusAll <- sento_corpus(corpusdf = usnews)
#' corpus <- quanteda::corpus_subset(corpusAll, date >= "2007-01-01" & date < "2014-10-01")
#' l <- setup_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#' ctr <- ctr_agg(howWithin = "tf-idf", howDocs = "proportional",
#'                howTime = c("equal_weight", "linear"),
#'                by = "month", lag = 3)
#' sentomeasures <- sento_measures(corpus, l, ctr)
#'
#' # prepare y variable
#' y <- epu[epu$date >= sentomeasures$measures$date[1], ]$index
#' length(y) == nobs(sentomeasures) # TRUE
#'
#' # estimate regression iteratively based on a sample of 60, skipping first 25 iterations
#' ctr <- ctr_model(model = "gaussian", type = "AIC", do.iter = TRUE,
#'                  h = 0, nSample = 60, start = 26)
#' out <- sento_model(sentomeasures, y, ctr = ctr)
#' summary(out)
#'
#' # plotting
#' p <- plot(out)
#' p <- p +
#'   ggthemes::theme_few()
#' p}
#'
#' @import ggplot2
#' @export
plot.sentomodeliter <- function(x, ...) {
  sentomodeliter <- x
  mF <- sentomodeliter$models[[1]]$model
  if (mF == "gaussian") {
    plotter <- geom_line()
    scaleY <- scale_y_continuous(name = "Response")
  } else {
    plotter <- geom_point()
    scaleY <- scale_y_discrete(name = "Response")
  }
  data <- data.frame(date = row.names(sentomodeliter$performance$raw),
                     realized = sentomodeliter$performance$raw$response,
                     prediction = sentomodeliter$performance$raw$predicted)
  if (mF != "gaussian") data[, 2:3] <- lapply(data[, 2:3], as.character)
  melted <- melt(data, id.vars = "date")
  p <- ggplot(data = melted, aes(x = as.Date(date), y = value, color = variable)) +
    plotter +
    scale_x_date(name = "Date", date_labels = "%m-%Y") +
    scaleY +
    ggthemes::theme_tufte(base_size = 12) +
    theme(legend.title = element_blank(), legend.position = "top")

  return(p)
}

#' Make predictions from a sentomodel object
#'
#' @author Samuel Borms
#'
#' @description Prediction method for \code{sentomodel} class, with usage along the lines of
#' \code{predict.glmnet}, but simplified in terms of allowed parameters.
#'
#' @param object a \code{sentomodel} object created with \code{\link{sento_model}}.
#' @param newx a \code{matrix} of \code{numeric} values with all explanatory variables to be used for the prediction(s),
#' structured row-by-row; see documentation for \code{\link{predict.glmnet}}. The number of variables should be equal to
#' \code{sentomodel$nVar}, being the sum of the number of original sentiment measures and the number of additional explanatory
#' variables. Variables discarded in the regression process are discarded again here, based on \code{sentomodel$discarded}.
#' @param type type of prediction required, a value from \code{c("link", "response", "class")}, see documentation for
#' \code{\link{predict.glmnet}}.
#' @param offset not used. Any values here will be ignored.
#' @param ... not used.
#'
#' @return A prediction output depending on the \code{type} argument.
#'
#' @seealso \code{\link{predict.glmnet}}, \code{\link{sento_model}}
#'
#' @export
predict.sentomodel <- function(object, newx, type, offset = NULL, ...) {
  sentomodel <- object
  reg <- sentomodel$reg
  discarded <- sentomodel$discarded
  idx <- c(!discarded, rep(TRUE, (sentomodel$nVar - length(discarded)))) # TRUE means variable to be kept for prediction
  newx <- newx[, idx, drop = FALSE]
  pred <- stats::predict(reg, newx = newx, type = type, offset = offset)
  return(pred)
}

#' Apply model confidence set (MCS) procedure to a selection of models
#'
#' @author Samuel Borms
#'
#' @description Calculates the model confidence set (see ``The Model Confidence Set''; Hansen, Lunde and Nason, 2011) as
#' implemented in the \pkg{MCS} package, for a set of different \code{sentomodeliter} objects.
#'
#' @param models a named \code{list} of \code{sentomodeliter} objects. All models should be of the same family, being either
#' \code{"gaussian"}, \code{"binomial"} or \code{"multinomial"}, and have performance data of the same dimensions.
#' @param loss a single \code{character} vector, either \code{"DA"} (directional \emph{in}accuracy), \code{"errorSq"}
#' (squared errors), \code{"AD"} (absolute errors) or \code{"accuracy"} (\emph{in}accurate class predictions). This argument
#' defines on what basis the model confidence set is calculated. The first three options are available for \code{"gaussian"}
#' models, the last option applies only to \code{"binomial"} and \code{"multinomial"} models.
#' @param ... other parameters that can be supplied to the \code{\link[MCS]{MCSprocedure}} function. If empty, its default
#' values are used.
#'
#' @return An object as returned by the \code{\link[MCS]{MCSprocedure}} function.
#'
#' @seealso \code{\link{sento_model}}, \code{\link[MCS]{MCSprocedure}}
#'
#' @examples
#' \dontrun{
#' data("usnews", package = "sentometrics")
#' data("list_lexicons", package = "sentometrics")
#' data("list_valence_shifters", package = "sentometrics")
#' data("epu", package = "sentometrics")
#'
#' # construct two sentomeasures objects
#' corpusAll <- sento_corpus(corpusdf = usnews)
#' corpus <- quanteda::corpus_subset(corpusAll, date >= "1997-01-01" & date < "2014-10-01")
#' l <- setup_lexicons(list_lexicons[c("LM_en", "HENRY_en")], list_valence_shifters[["en"]])
#'
#' ctr1 <- ctr_agg(howWithin = "tf-idf", howDocs = "proportional",
#'                 howTime = c("equal_weight", "linear"), by = "month", lag = 3)
#' sentMeas1 <- sento_measures(corpus, l, ctr1)
#'
#' ctr2 <- ctr_agg(howWithin = "counts", howDocs = "equal_weight",
#'                 howTime = c("equal_weight", "linear"), by = "month", lag = 3)
#' sentMeas2 <- sento_measures(corpus, l, ctr2)
#'
#' # prepare y and other x variables
#' y <- epu[epu$date >= sentMeas1$measures$date[1], ]$index
#' length(y) == nobs(sentMeas1) # TRUE
#' x <- data.frame(runif(length(y)), rnorm(length(y))) # two other (random) x variables
#' colnames(x) <- c("x1", "x2")
#'
#' # estimate different type of regressions
#' ctr1 <- ctr_model(model = "gaussian", type = "AIC", do.iter = TRUE,
#'                   h = 0, nSample = 120, start = 50)
#' out1 <- sento_model(sentMeas1, y, x = x, ctr = ctr1)
#'
#' ctr2 <- ctr_model(model = "gaussian", type = "AIC", do.iter = TRUE,
#'                   h = 0, nSample = 120, start = 50)
#' out2 <- sento_model(sentMeas1, y, x = NULL, ctr = ctr2)
#'
#' ctr3 <- ctr_model(model = "gaussian", type = "AIC", do.iter = TRUE,
#'                   h = 0, nSample = 120, start = 50)
#' out3 <- sento_model(sentMeas2, y, x = x, ctr = ctr3)
#'
#' ctr4 <- ctr_model(model = "gaussian", type = "AIC", do.iter = TRUE,
#'                   h = 0, nSample = 120, start = 50)
#' out4 <- sento_model(sentMeas2, y, x = NULL, ctr = ctr4)
#'
#' mcs <- perform_MCS(models = list(m1 = out1, m2 = out2, m3 = out3, m4 = out4),
#'                    loss = "errorSq")}
#'
#' @export
perform_MCS <- function(models, loss = c("DA", "errorSq", "AD", "accuracy"), ...) {

  # check if input is consistent
  if (!is.list(models) || length(models) < 2) stop("Please provide a list of at least two models.")
  checkClass <- sapply(models, function(m) return(!inherits(m, "sentomodeliter")))
  if (any(checkClass)) stop("Not all elements of the 'models' list are sentomodeliter objects.")
  modelFamilies <- unlist(lapply(models, function(m) return(m$models[[1]]$model)))
  if (!(length(table(modelFamilies)) == 1)) stop("All models should come from the same family.")
  mF <- as.character(modelFamilies[1])
  if (length(loss) != 1) stop("The 'loss' argument should contain a single argument.")
  checkGaussian <- (mF == "gaussian" & (loss %in% c("DA", "errorSq", "AD")))
  checkLogistic <- (mF %in% c("binomial", "multinomial") & (loss == "accuracy"))
  if (!(checkGaussian | checkLogistic))
      stop("The 'loss' argument is not in line with the model families.")

  # extract loss data
  lossData <- matrix(unlist(lapply(models, function(m) m$performance$raw[[loss]]), use.names = FALSE), ncol = length(models))
  colnames(lossData) <- names(models)
  if (loss %in% c("DA", "accuracy")) lossData <- abs(lossData - 1) # from accuracy to inaccuracy
  if (loss == "DA") lossData <- lossData[-1, ] # get rid of first NA values
  # get parameters for MCS calculation, plug in package's default values if nothing provided
  dots <- list(...)
  alpha <- ifelse(is.null(dots$alpha), 0.15, dots$alpha)
  B <- ifelse(is.null(dots$B), 5000, dots$B)
  if (is.null(dots$cl)) {
    cl <- NULL
  } else cl <- dots$cl
  ram.allocation <- ifelse(is.null(dots$ram.allocation), TRUE, dots$ram.allocation)
  statistic <- ifelse(is.null(dots$statistic), "Tmax", dots$statistic)
  if (is.null(dots$k)) {
    k <- NULL
  } else k <- dots$k
  min.k <- ifelse(is.null(dots$min.k), 3, dots$min.k)
  verbose <- ifelse(is.null(dots$verbose), TRUE, dots$verbose)

  outMCS <- MCS::MCSprocedure(Loss = lossData, alpha = alpha, B = B, cl = cl, ram.allocation = ram.allocation,
                              statistic = statistic, k = k, min.k = min.k, verbose = verbose)

  return(outMCS)
}

