
#' Setup control for sentiment measures-based regression modelling
#'
#' @description Sets up control object for linear or nonlinear modelling of a response variable onto a sparse panel of
#' textual sentiment measures (and potentially other variables). Models are computed using the elastic-net regularization as
#' implemented in the \pkg{glmnet} package, to account for the sparsity of the sentiment measures. For a helpful introduction
#' to \pkg{glmnet}, we refer to their \href{https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html#lin}{vignette}.
#' The optimal elastic-net parameters \code{lambda} and \code{alpha} are calibrated either through a to specify information
#' criterion or through cross-validation (based on the "rolling forecasting origin" principle).
#'
#' @param model a \code{character} vector with one of the following: "\code{lm}" (linear regression), "\code{binomial}"
#' (binomial logistic regression), or "\code{multinomial}" (multinomial logistic regression).
#' @param type a \code{character} vector indicating which model selection criteria to use. Currently supports "\code{BIC}",
#' "\code{AIC}" and "\code{Cp}" (Mallows's Cp) as sparse-regression adapted information criteria (cf. Zou, Hastie, Tibshirani
#' et al. (2007). "On the 'degrees of freedom' of the LASSO."), and "\code{cv}" (cross-validation based on the \code{train}
#' function from the \pkg{caret} package). The adapted information criteria are currently only available for a linear
#' regression.
#' @param h an \code{integer} value to shift the time series to have the desired (forecasting) setup, \code{h == 0} means
#' no change to the input data (nowcasting assuming data is aligned properly), \code{h > 0} shifts the dependent variable by
#' \code{h} periods (i.e. rows) further in time (forecasting), \code{h < 0} shifts the independent variables by \code{h}
#' periods.
#' @param alphas a \code{numeric} vector of the different alphas to test for during optimization, between 0 and 1. A value of
#' 0 pertains to Ridge optimization, a value of 1 to LASSO optimization; values in between are pure elastic-net.
#' @param lambdas a \code{numeric} vector of the different lambdas to test for during optimization.
#' @param trainWindow a positive \code{integer} as the size of the training sample in cross-validation (ignored if
#' \code{type !=} "\code{cv}").
#' @param testWindow a positive \code{integer} as the size of the test sample in cross-validation (ignored if \code{type !=}
#' "\code{cv}").
#' @param oos a non-negative \code{integer} to indicate the number of periods to skip from the end of the cross-validation
#' training sample (out-of-sample) up to the test sample (ignored if \code{type !=} "\code{cv}").
#' @param do.iter a \code{logical}, \code{TRUE} induces an iterative optimization of models through time.
#' @param do.progress a \code{logical}, if \code{TRUE} progress statements are displayed during model calibration.
#' @param nSample a positive \code{integer} as the size of the sample for model calibration at every iteration (ignored if
#' \code{iter == FALSE}).
#' @param start a positive \code{integer} to indicate at which point the iteration has to start (ignored if
#' \code{iter == FALSE}).
#'
#' @return A list encapsulating the control parameters.
#'
#' @examples
#' # series of example information criterion based model control functions
#' ctrIC1 <- ctr_model(model = "lm", type = "BIC", do.iter = FALSE, h = 0,
#'                     alphas = seq(0, 1, by = 0.10))
#' ctrIC2 <- ctr_model(model = "lm", type = "BIC", do.iter = TRUE, h = 0, nSample = 100)
#'
#' # series of example cross-validation based model control functions
#' ctrCV1 <- ctr_model(model = "lm", type = "cv", do.iter = FALSE, h = 0, trainWindow = 250,
#'                     testWindow = 4, oos = 0, do.progress = TRUE)
#' ctrCV2 <- ctr_model(model = "binomial", type = "cv", h = 0, trainWindow = 250,
#'                     testWindow = 4, oos = 0, do.progress = TRUE)
#' ctrCV3 <- ctr_model(model = "multinomial", type = "cv", h = 0, trainWindow = 250,
#'                     testWindow = 4, oos = 0, do.progress = TRUE)
#' ctrCV4 <- ctr_model(model = "lm", type = "cv", do.iter = TRUE, h = 0, trainWindow = 45,
#'                     testWindow = 4, oos = 0, nSample = 70, do.progress = TRUE)
#'
#' @export
ctr_model <- function(model = c("lm", "binomial", "multinomial"), type = c("BIC", "AIC", "Cp", "cv"),
                      do.iter = FALSE, h = 1, lambdas = 10^seq(2, -2, length.out = 50), alphas = seq(0, 1, by = 0.20),
                      nSample = NULL, trainWindow = NULL, testWindow = NULL, oos = 0, start = 1, do.progress = TRUE) {

  if (length(model) > 1) model <- model[1]
  else if (!(model %in% c("lm", "binomial", "multinomial")))
    stop("Provide a proper modelling type.")

  if (length(type) > 1) type <- type[1]
  else if (!(type %in% c("BIC", "AIC", "Cp", "cv")))
    stop("Provide a proper calibration type.")

  if (model != "lm" & type != "cv")
    stop("Elastic net-specific information criteria are currently only supported for linear models, please opt for 'cv'.")

  if (oos < 0 | start <= 0)
    stop("Make sure all integer inputs are non-negative or positive for those required.")

  if (min(alphas) < 0 | max(alphas) > 1)
    stop("Each alpha value in alphas must be between 0 and 1, inclusive.")

  if (!do.iter) nSample <- start <- NULL

  if (do.iter & is.null(nSample))
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
                    do.iter = do.iter,
                    h = h,
                    nSample = nSample,
                    start = start,
                    oos = oos,
                    alphas = alphas,
                    lambdas = lambdas,
                    trainWindow = trainWindow,
                    testWindow = testWindow,
                    do.progress = do.progress)

  return(ctr_model)
}

#' Optimized and automated sparse regression
#'
#' @description Linear or nonlinear penalized regression of a dependent variable on the wide number of sentiment measures and
#' potentially other explanatory variables. Either performs a regression given the provided variables at once, or computes
#' regressions sequentially for a given sample size over a longer time horizon, with associated forecasting performance
#' metrics. Independent variables are normalized in the regression process, but coefficients are returned in their original
#' space.
#'
#' @param sentomeasures a \code{sentomeasures} object. There should be at least two explanatory variables including the ones
#' provided through the \code{x} argument.
#' @param y a one-column \code{data.frame} or a \code{numeric} vector capturing the dependent (response) variable. In case of
#' a logistic regression, the response variable is either a \code{factor} or a \code{matrix} with the factors represented by
#' the columns as binary indicators, with the last factor level or column as the reference class. No \code{NA} values are
#' allowed.
#' @param x a named \code{data.frame} with other explanatory variables, by default set to \code{NULL}.
#' @param ctr output from a \code{ctr_model()} call.
#'
#' @return If \code{ctr$do.iter == FALSE}, a \code{sentomodel} object which is a list containing:
#' \item{reg}{optimized regression, i.e. a model-specific \code{glmnet} object.}
#' \item{sentomeasures}{the input \code{sentomeasures} object.}
#' \item{alpha}{optimized calibrated alpha.}
#' \item{lambda}{optimized calibrated lambda.}
#' \item{trained}{output from \code{caret::train} call (if \code{ctr$type ==} "\code{cv}").}
#' \item{ic}{a \code{list} composed of two elements: the information criterion used in the calibration under
#' \code{"criterion"}, and a vector of all minimum information criterion values for each value in \code{alphas}
#' under \code{"opts"} (if \code{ctr$type !=} "\code{cv}").}
#'
#' @return If \code{ctr$do.iter == TRUE}, a list containing:
#' \item{regs}{optimized regressions, i.e. separate \code{sentomodel} objects as above, as a \code{list} with as names the
#' dates from the perspective of the sentiment measures at which predictions for performance measurement are carried out.}
#' \item{alphas}{optimized calibrated alphas.}
#' \item{lambdas}{optimized calibrated lambdas.}
#' \item{performance}{a \code{data.frame} with performance-related measures, being "\code{RMSFE}" (root mean squared
#' forecasting error), "\code{MAD}" (mean absolute deviation), "\code{MAPE}" (mean absolute percentage error),
#' "\code{DA}" (directional accuracy), "\code{accuracy}" (proportion of correctly predicted classes in case of a logistic
#' regression), and each's respective individual values in the sample. Only the relevant performance statistics are given
#' depending on the type of regression. Dates are similarly as with the \code{"regs"} output element from the perspective
#' of the sentiment measures.}
#'
#' @seealso \code{\link{ctr_model}}, \code{\link[glmnet]{glmnet}}, \code{\link[caret]{train}}
#'
#' @examples
#' # construct a sentomeasures object to start with
#' data("useconomynews")
#' useconomynews <- useconomynews[date >= "1988-01-01", ]
#' corpus <- sento_corpus(corpusdf = useconomynews)
#' l <- setup_lexicons(lexicons[c("LM_eng", "HENRY_eng")], valence[["valence_eng"]])
#' ctr <- ctr_agg(howWithin = "tf-idf", howDocs = "proportional",
#'                howTime = c("equal_weight", "linear", "almon"),
#'                by = "month", lag = 3, ordersAlm = 1:3,
#'                do.inverseAlm = TRUE, do.normalizeAlm = TRUE)
#' sentomeasures <- sento_measures(corpus, l, ctr)
#'
#' # prepare y and other x variables
#' data("sp500")
#' y <- sp500$return # convert to numeric vector
#' sentomeasures <- fill_measures(sentomeasures)
#' length(y) == nrow(sentomeasures$measures) # TRUE
#' x <- data.frame(runif(length(y)), rnorm(length(y))) # two other (random) x variables
#' colnames(x) <- c("x1", "x2")
#'
#' # a list with models based on the three implemented information criteria
#' out1 <- list()
#' for (ic in c("BIC", "AIC", "Cp")) {
#'    ctrIC <- ctr_model(model = "lm", type = ic, do.iter = FALSE, h = 0)
#'    out1[[ic]] <- sento_model(sentomeasures, y, x = x, ctr = ctrIC)
#' }
#'
#' # a (very) short iterative analysis of cross-validation based models
#' ctrCV <- ctr_model(model = "lm", type = "cv", do.iter = TRUE, h = 0, trainWindow = 250,
#'                    testWindow = 20, oos = 0, nSample = 320, do.progress = TRUE)
#' out2 <- sento_model(sentomeasures, y, x = x, ctr = ctrCV)
#'
#' # a similar iterative analysis of cross-validation based models but for a binomial target
#' yb <- sp500$up
#' ctrCV <- ctr_model(model = "binomial", type = "cv", do.iter = TRUE, trainWindow = 250,
#'                    h = 0, testWindow = 20, oos = 0, nSample = 320, do.progress = TRUE)
#' out3 <- sento_model(sentomeasures, yb, x = x, ctr = ctrCV)
#'
#' # post-analysis (summary, attribution and prediction)
#' out <- out1[["BIC"]]
#' summary(out)
#'
#' attribution <- retrieve_attributions(out)
#'
#' nx <- ncol(sentomeasures$measures) - 1 + ncol(x) # don't count date column
#' newx <- runif(nx) * cbind(sentomeasures$measures[, -1], x)[nrow(x), ]
#' preds <- predict(out, newx = as.matrix(newx), type = "link")
#'
#' @export
sento_model <- function(sentomeasures, y, x = NULL, ctr) {
  check_class(sentomeasures, "sentomeasures")

  if (any(is.na(y))) stop("No NA values are allowed in y.")
  nrows <- c(nrow(sentomeasures$measures), ifelse(is.null(nrow(y)), length(y), nrow(y)), nrow(x))
  if (length(unique(nrows)) != 1)
    stop("Number of rows or length for y, x and measures in sentomeasures must be equal.")
  if (sum(ncol(sentomeasures$measures) + ifelse(is.null(x), 0, ncol(x))) < 2)
    stop("There should be at least two explanatory variables out of sentomeasures and x combined.")

  family <- ctr$model
  if (family == "lm") family <- "gaussian"
  type <- ctr$type
  do.iter <- ctr$do.iter
  h <- ctr$h
  alphas <- ctr$alphas
  lambdas <- ctr$lambdas
  do.progress <- ctr$do.progress
  trainWindow <- ctr$trainWindow # used when type == "cv"
  testWindow <- ctr$testWindow # used when type == "cv"
  oos <- ctr$oos # used when type == "cv"
  nSample <- ctr$nSample # used when do.iter == TRUE
  start <- ctr$start # used when do.iter == TRUE

  if (do.iter) {
    out <- sento_model_iter(sentomeasures, y = y, x = x, h = h, family = family, alphas = alphas,
                            lambdas = lambdas, type = type, nSample = nSample, start = start, oos = oos,
                            trainWindow = trainWindow, testWindow = testWindow, do.progress = do.progress)
  } else {
    if (type == "cv") {
      out <- model_CV(sentomeasures, y = y, x = x, h = h, family = family, alphas = alphas, lambdas = lambdas,
                      trainWindow = trainWindow, testWindow = testWindow, oos = oos, do.progress = do.progress)
    } else {
      out <- model_IC(sentomeasures, y = y, x = x, h = h, family = family, alphas = alphas,
                      lambdas = lambdas, ic = type, do.progress = do.progress)
    }
  }

  return(out)
}

.model_IC <- function(sentomeasures, y, x, h, family, alphas, lambdas,
                      ic, do.progress, ...) {

  # inputs i and nSample are NULL if one-shot model (not iterative)
  dots <- list(...)
  i <- dots$i
  nSample <- dots$nSample

  # clean measures (e.g. get rid of duplicated or too sparse measures)
  sentomeasures <- clean_panel(sentomeasures)

  alignedVars <- align_variables(y, sentomeasures, x, h, i = i, nSample = nSample)
  yy <- alignedVars$y
  xx <- alignedVars$x # changed x to include sentiment measures
  refDate <- alignedVars$datesX[nrow(yy)]

  penalty <- rep(1, ncol(xx))
  if (!is.null(x)) penalty[ncol(sentomeasures$measures):ncol(xx)] <- 0 # no shrinkage for original x variables

  minIC <- as.list(numeric(length(alphas)))
  names(minIC) <- as.character(alphas)
  lambdaVals <- numeric(length(alphas))
  for (i in seq_along(alphas)) {
    alpha <- alphas[i]
    if (do.progress) {
      if (alpha == alphas[1]) cat("alphas run: ", alpha, ", ", sep = "")
      else if (alpha == alphas[length(alphas)]) cat(alpha, "\n")
      else cat(alpha, ", ", sep = "")
    }

    reg <- glmnet::glmnet(x = xx, y = yy, penalty.factor = penalty,
                          lambda = lambdas, alpha = alpha, standardize = TRUE, family = family)

    IC <- compute_IC(reg, yy, xx, alpha, ic, family = family) # vector of ic values for lambdas sequence in reg
    minIC[[i]] <- suppressWarnings(min(IC, na.rm = TRUE))
    lambdaTmp <- ifelse(length(lambdas[which.min(IC)]) > 0, lambdas[which.min(IC)], Inf)
    lambdaVals[i] <- lambdaTmp
  }

  # retrieve optimal alphas and lambdas
  minLoc <- which.min(minIC)
  lambdaOpt <- lambdaVals[minLoc]
  alphaOpt <- alphas[minLoc]

  # actual elastic net optimization
  regOpt <- glmnet::glmnet(x = xx, y = yy, penalty.factor = penalty,
                           lambda = lambdaOpt, alpha = alphaOpt, standardize = TRUE, family = family)

  out <- list(reg = regOpt,
              sentomeasures = sentomeasures,
              alpha = alphaOpt,
              lambda = lambdaOpt,
              ic = list(criterion = ic, opts = unlist(minIC)),
              date = refDate)

  class(out) <- c("sentomodel")

  return(out)
}
model_IC <- compiler::cmpfun(.model_IC)

.model_CV <- function(sentomeasures, y, x, h, family, alphas, lambdas,
                      trainWindow, testWindow, oos, do.progress, ...) {

  # inputs i and nSample are NULL if one-shot model (not iterative)
  dots <- list(...)
  i <- dots$i
  nSample <- dots$nSample

  # clean measures (e.g. get rid of duplicated or too sparse measures)
  sentomeasures <- clean_panel(sentomeasures)

  alignedVars <- align_variables(y, sentomeasures, x, h, i = i, nSample = nSample)
  yy <- alignedVars$y
  xx <- alignedVars$x # changed x to include sentiment measures
  refDate <- alignedVars$datesX[nrow(yy)]

  penalty <- rep(1, ncol(xx))
  if (!is.null(x)) penalty[ncol(sentomeasures$measures):ncol(xx)] <- 0 # no shrinkage for original x variables

  # train model based on slices in sliced
  sliced <- create_cv_slices(1:nrow(yy), trainWindow, testWindow = testWindow, skip = oos, do.reverse = FALSE)
  ctrTrain <- caret::trainControl(index = sliced$train, indexOut = sliced$test, allowParallel = TRUE)
  tuneGrid <- expand.grid(alpha = alphas, lambda = lambdas)

  # change y variable to format required in caret::train function
  if (family == "gaussian") yyy <- yy[, 1]
  else yyy <- as.factor(colnames(yy)[yy %*% 1:ncol(yy)])

  # align training metric based on whether family is a linear or classifcation model
  metric <- ifelse(family == "gaussian", "RMSE", "Accuracy")

  if (do.progress) cat("Training model... ")
  trained <- caret::train(x = xx, y = yyy, method = "glmnet", family = family,
                          standardize = TRUE, penalty.factor = penalty,
                          trControl = ctrTrain, tuneGrid = tuneGrid, metric = metric)
  if (do.progress) cat("Done.", "\n")

  # retrieve optimal alphas and lambdas
  alphaOpt <- as.numeric(trained$bestTune[1])
  lambdaOpt <- as.numeric(trained$bestTune[2])

  # actual elastic net optimization
  regOpt <- glmnet::glmnet(x = xx, y = yy, penalty.factor = penalty,
                           lambda = lambdaOpt, alpha = alphaOpt, standardize = TRUE, family = family)

  out <- list(reg = regOpt,
              sentomeasures = sentomeasures,
              alpha = alphaOpt,
              lambda = lambdaOpt,
              trained = trained,
              date = refDate)

  class(out) <- c("sentomodel")

  return(out)
}
model_CV <- compiler::cmpfun(.model_CV)

.sento_model_iter <- function(sentomeasures, y, x, h, family, alphas, lambdas,
                              type, nSample, start, trainWindow, testWindow, oos, do.progress) {

  nIter <- ifelse(is.null(nrow(y)), length(y), nrow(y)) - nSample - abs(h) - oos
  if (nIter <= 0 | start > nIter)
    stop("Data not sufficient to do at least one iteration for given sample size, horizon, out-of-sample skip and start.")

  regsOpt <- as.list(numeric(nIter - start + 1))
  lambdasOpt <- numeric(nIter - start + 1)
  alphasOpt <- numeric(nIter - start + 1)

  if (type == "cv") fun <- model_CV
  else fun <- model_IC

  for (i in start:nIter) {
    if (do.progress) cat("iteration: ", i, " from ", nIter, "\n", sep = "")

    reg <- fun(sentomeasures, y, x, h, alphas, lambdas,
               trainWindow = trainWindow, testWindow = testWindow,
               oos = oos, ic = type, do.progress = do.progress,
               i = i, nSample = nSample, family = family)

    regsOpt[[i - start + 1]] <- reg
  }

  # get optimal alphas and lambdas
  alphasOpt <- sapply(regsOpt, function(x) return(x$alpha))
  lambdasOpt <- sapply(regsOpt, function(x) return(x$lambda))

  # prepare for and get all predictions (estimations)
  alignedVarsAll <- align_variables(y, sentomeasures, x, h)
  xPred <- alignedVarsAll$x[(start + nSample):(nIter + nSample), , drop = FALSE]
  yReal <- alignedVarsAll$y[(start + nSample + oos):(nIter + nSample + oos), , drop = FALSE]
  datesX <- alignedVarsAll$datesX[(start + nSample):(nIter + nSample)] # dates from perspective of x
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

  out <- list(regs = regsOpt,
              alphas = alphasOpt,
              lambdas = lambdasOpt,
              performance = performance)

  return(out)
}
sento_model_iter <- compiler::cmpfun(.sento_model_iter)

compute_IC <- function(reg, y, x, alpha, ic, family) {

    beta <- reg$beta
  lambda <- reg$lambda

  if (family == "gaussian") type <- "link"
  else stop("To implement for 'binomial' and 'multinomial'.")

  yEst <- stats::predict(reg, newx = x, type = type)
  df_A <- compute_df(alpha, beta, lambda, x)
  RSS <- apply(yEst, 2, FUN = function(est) sum((y - est)^2))
  sigma2 <- RSS[length(RSS)] / (nrow(y) - df_A[length(RSS)])

  if (ic == "BIC") {
    fun <- compute_BIC
  } else if (ic == "AIC") {
    fun <- compute_AIC
  } else if (ic == "Cp") {
    fun <- compute_Cp
  }
  IC <- fun(y, yEst, df_A, RSS, sigma2)

  return(IC)
}

model_performance <- function(yEst, yReal, family, dates, ...) {

  dots <- list(...)

  if (family == "gaussian") {
    tp <- as.numeric(yReal > 0 & yEst > 0) # true positives
    fp <- as.numeric(yReal < 0 & yEst > 0) # false positives
    tn <- as.numeric(yReal < 0 & yEst < 0) # true negatives
    fn <- as.numeric(yReal > 0 & yEst < 0) # false negatives
    dAcc <- data.frame(cbind(tp, fp, tn, fn))
    colnames(dAcc) <- c("tp", "fp", "tn", "fn")
    DA <- (sum(tp) + sum(tn)) / (sum(tp) + sum(fp) + sum(tn) + sum(fn)) # directional accuracy

    error <- yEst - yReal
    error2 <- error^2
    AD <- abs(error) # absolute deviation
    APE <- 100 * AD / abs(yReal) # absolute percentage error
    errors <- data.frame(cbind(error, error2, AD, APE))
    colnames(errors) <- c("error", "errorSq", "AD", "APE")
    meanErrors <- colMeans(errors[, -1], na.rm = TRUE)

    raw <- data.frame(yReal, predicted = yEst, dAcc, errors)
    row.names(raw) <- dates

    errorsAll <- list(raw = raw, DA = DA, RMSFE = as.numeric(sqrt(meanErrors["errorSq"])),
                      MAD = as.numeric(meanErrors["AD"]), MAPE = as.numeric(meanErrors["APE"]))

  } else if (family %in% c("binomial", "multinomial")) {
    yRealClass <- as.factor(colnames(yReal)[yReal %*% 1:ncol(yReal)])
    yEstClass <- dots$yEstClass
    accuracy <- as.numeric(yRealClass == yEstClass)
    accuracyProb <- sum(accuracy)/length(accuracy)

    raw <- data.frame(response = yRealClass, predicted = yEstClass, accuracy = accuracy)
    row.names(raw) <- dates

    errorsAll <- list(raw = raw, accuracy = accuracyProb)
  }

  return(errorsAll)
}

#' Retrieves top-down sentiment attribution given forecasting model
#'
#' ### TODO
#'
#' @param sentomodel a \code{sentomodel} object.
#'
#' @return A list with all possible dimensions for which aggregation can be computed, as \code{data.table}s with an
#' \code{"id"} (at document-level) column, a \code{"date"} column and a \code{"attribution"} column. ### TODO
#'
#' @seealso \code{\link{sento_model}}
#'
#' @export
retrieve_attributions <- function(sentomodel) {
  check_class(sentomodel, "sentomodel")

  ### TODO: proper datesIn (fill sentiment values) [vs. time aggregation with unfilled values]
  ### TODO: date selection/timing of attribution
  ### TODO: application to logistic models (exponentiation?)

  sentomeasures <- sentomodel$sentomeasures
  sent <- sentomeasures$sentiment
  lex <- sentomeasures$lexicons
  features <- sentomeasures$features
  W <- sentomeasures$attribWeights[["W"]]
  B <- sentomeasures$attribWeights[["B"]]
  refDate <- sentomodel$date
  measures <- sentomeasures$measures[, -1]
  cols <- colnames(measures)
  coeffs <- stats::coef(sentomodel$reg)[2:(length(cols) + 1), ] # exclude intercept and other x variables

  attribsList <- function() {
    dates <- unique(sent$date)
    datesIn <- dates[(which(dates == refDate) - nrow(B) + 1):which(dates == refDate)]
    B$date <- datesIn
    outL <- vector(mode = "list", length = length(lex))
    names(outL) <- lex
    for (lexicon in lex) { # document-level attribution for all lexicons
      selS <- colnames(sent)[stringi::stri_detect(colnames(sent), regex = paste0(lexicon, "\\b"))]
      outF <- lapply(features, function(feat) { # for later summation over all features
        selF <- cols[stringi::stri_detect(cols, regex = paste0(paste0(c(lexicon, feat), collapse = "--"), "\\b"))]
        betas <- coeffs[selF]
        outB <- rbindlist(
          lapply(datesIn, function(b) { # for all dates between reference date and lag number
            timeWeights <- B[B$date == b, -ncol(B)]
            docWeights <- W[date == b, "w"]
            sents <- sent[date == b, selS, with = FALSE]
            colnames(sents) <- features
            out <- data.table(sent[date == b, c("id")], date = rep(b, nrow(docWeights)),
                              sum(timeWeights * betas) * docWeights * sents[, feat, with = FALSE])
            return(out)
          })
        )
        names(outB)[3] <- c("attrib")
        return(outB)
      })
      names(outF) <- features
      outL[[lexicon]] <- outF
    }
    return(outL)
  }
  attribsListed <- attribsList()

  attribsDocs <- lapply(attribsListed, function(l) {
    all <- rbindlist(l)
    out <- all[, list(attrib = sum(attrib)), by = list(id, date)]
    return(out)
  })

  attribsFeatures <- rbindlist(lapply(attribsListed, function(l) {
    ll <- rbindlist(l, idcol = "feature")
    return(ll[, list(attrib = sum(attrib)), by = feature])
  }))[, list(attrib = sum(attrib)), by = feature]

  attribsLexicons <- rbindlist(lapply(attribsDocs, function(l) {
    return(l[, list(attrib = sum(attrib))])
  }), idcol = "lexicon")

  attribsTime <- lapply(colnames(B), function(x) {
    sel <- cols[stringi::stri_detect(cols, regex = paste0("\\b", x, "\\b"))] # exact match
    attr <- sum(coeffs[sel] * measures[nrow(measures), sel, with = FALSE, drop = FALSE])
    attr <- data.table(attrib = attr)
    return(attr)
  })
  names(attribsTime) <- colnames(B)
  attribsTime <- rbindlist(attribsTime, idcol = "time")

  attribsAll <- vector(mode = "list", length = 4)
  attribsAll[[1]] <- attribsDocs
  attribsAll[[2]] <- attribsFeatures
  attribsAll[[3]] <- attribsLexicons
  attribsAll[[4]] <- attribsTime
  names(attribsAll) <- c("documents", "features", "lexicons", "time")

  return(attribsAll)
}

#' Summary of a sentomodel object
#'
#' Prints out a short summary consisting of the main elements of the constructed model (model type, calibrated values,
#' non-zero coefficients and performance).
#'
#' @param object a \code{sentomodel} object.
#' @param ... not used.
#'
#' @return A sequence of informative prints on the model's results.
#'
#' @seealso \code{\link{sento_model}}
#'
#' @export
summary.sentomodel <- function(object, ...) {
  sentomodel <- object
  reg <- sentomodel$reg
  if ("ic" %in% names(sentomodel)) {
    printCalib <- paste0("via ", sentomodel$ic[[1]], " information criterion")
  } else {
    printCalib <- paste0("via cross-validation; ",
                         "Ran trough ", nrow(sentomodel$trained$resample), " samples of size ",
                         length(sentomodel$trained$control$index[[1]]),
                         ", selection based on ", sentomodel$trained$metric, " metric")
  }
  cat("\n")
  cat("Model specifications \n")
  cat(rep("-", 20), "\n \n")
  cat("Calibration:", printCalib, "\n")
  cat("Number of observations:", reg$nobs, "\n")
  cat("Optimal elastic net alpha parameter:", sentomodel$alpha, "\n")
  cat("Optimal elastic net lambda parameter:", reg$lambda, "\n \n")
  cat("Non-zero coefficients \n")
  cat(rep("-", 20), "\n")
  print(nonzero_coeffs(reg))
  cat("\n \n")
  cat("Performance \n")
  cat(rep("-", 20), "\n \n")
  cat("Fraction of deviance explained: ", reg$dev.ratio * 100, "% \n \n")
  invisible()
}

#' Make predictions from a sentomodel object
#'
#' Prediction (forecasting) method for \code{sentomodel} class, with usage along the lines of \code{predict.glmnet}, but
#' simplified in terms of allowed parameters.
#'
#' @param object a \code{sentomodel} object.
#' @param newx a \code{matrix} of \code{numeric} values for all explanatory variables at which predictions are to be made,
#' see documentation for \code{\link{predict.glmnet}}.
#' @param type type of prediction required, a value from \code{c("link", "response", "class")}, see documentation for
#' \code{\link{predict.glmnet}}.
#' @param offset values to use as offset, only if an offset was also used in the model fitting, see documentation for
#' \code{\link{predict.glmnet}}.
#' @param ... not used.
#'
#' @return A prediction output depending on the \code{type} argument provided.
#'
#' @seealso \code{\link{predict.glmnet}}, \code{\link{sento_model}}
#'
#' @export
predict.sentomodel <- function(object, newx, type, offset = NULL, ...) {
  sentomodel <- object
  reg <- sentomodel$reg
  pred <- stats::predict(reg, newx = newx, type = type, offset = offset)
  return(pred)
}

