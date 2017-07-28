
### parallelization to add (internally or pointers in documentation on how to use parallelization)
### check warnings and sanity regarding caret::train() setup
### do is.na(x) <- 0 before or after cleaning?
### mice package in fill_measures?
### add binomial (model == "binomial") and multinomial (model == "multinomial") logistic regression

#' Add and fill missing dates
#'
#' @description Adds missing dates between earliest and latest date, such that time series is continuous on a period-by-period
#' basis. Fills in these dates with either \code{NA} or the respective latest non-\code{NA} values.
#'
#' @param sentomeasures a \code{sentomeasures} object.
#' @param do.fillLatest a \code{logical}, if \code{TRUE} fills added dates with most recent value.
#'
#' @return A modified \code{sentomeasures} object.
#'
#' @export
fill_measures <- function(sentomeasures, do.fillLatest = FALSE) {

  check_class(sentomeasures, "sentomeasures")

  by <- sentomeasures$by
  measures <- sentomeasures$measures
  dates <- measures$date
  ts <- seq(dates[1], dates[length(dates)], by = by)
  dt <- data.table(date = ts)

  # join to new measures
  measuresFill <- merge(dt, measures, by = "date", all = TRUE) # fills with NA
  if (do.fillLatest) measuresFill <- zoo::na.locf(measuresFill)
  sentomeasures$measures <- measuresFill

  return(sentomeasures)
}

compute_IC <- function(reg, y, x, alpha, ic) {

  beta <- reg$beta
  lambda <- reg$lambda

  yEst <- stats::predict(reg, newx = as.matrix(x))
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

model_performance <- function(yEst, yReal) {

  # model errors
  error <- yEst - yReal
  error2 <- error^2
  AD <- abs(error)
  APE <- 100 * AD / abs(yReal)
  errors <- data.frame(cbind(error, error2, AD, APE))
  colnames(errors) <- c("error", "error^2", "AD", "APE")

  meanErrors <- colMeans(errors, na.rm = TRUE)[2:ncol(errors)]

  # directional accuracy
  tp <- as.numeric(yReal > 0 & yEst > 0) # true positives
  fp <- as.numeric(yReal < 0 & yEst > 0) # false positives
  tn <- as.numeric(yReal < 0 & yEst < 0) # true negatives
  fn <- as.numeric(yReal > 0 & yEst < 0) # false negatives
  acc <- data.frame(cbind(tp, fp, tn, fn))
  colnames(acc) <- c("tp", "fp", "tn", "fn")

  DA <- (sum(tp) + sum(tn)) / (sum(tp) + sum(fp) + sum(tn) + sum(fn))

  errorsAll <- list(raw = cbind(realized = yReal, predicted = yEst, errors, acc),
                    RMSFE = as.numeric(sqrt(meanErrors["error^2"])),
                    MAD = as.numeric(meanErrors["AD"]),
                    MAPE = as.numeric(meanErrors["APE"]),
                    DA = DA)

  return(errorsAll)
}

#' Setup control for sentiment measures-based regression modelling
#'
#' @description Sets up control for linear or nonlinear modelling of a response variable onto a sparse panel of
#' textual sentiment measures (and potentially other variables). Models are computed using the elastic-net regularization
#' as implemented in the \pkg{glmnet} package, to account for the sparsity of the textual sentiment measures. The optimal
#' elastic-net parameters \code{lambda} and \code{alpha} are calibrated either through a to specify information criterion or
#' through cross-validation (based on the "rolling forecasting origin" principle).
#'
#' @param model a \code{character} vector with one of the following: "\code{lm}" (linear regression), "\code{binomial}"
#' (binomial logistic regression), or "\code{multinomial}" (multinomial logistic regression).
#' @param type a \code{character} vector indicating which model selection criteria to use. Currently supports "\code{BIC}",
#' "\code{AIC}" and "\code{Cp}" (Mallows's Cp) as sparse-regression adapted information criteria (cf. Zou, Hastie, Tibshirani
#' et al. (2007). "On the 'degrees of freedom' of the LASSO."), and "\code{cv}" (cross-validation based on the \code{train}
#' function from the \pkg{caret} package).
#' @param h an \code{integer} value to shift the time series to have the desired (forecasting) setup, \code{h == 0} means
#' no change to input data (nowcasting assuming data is aligned properly), \code{h > 0} shifts the dependent variable by
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

  if (oos < 0 | start <= 0)
    stop("Make sure all integer inputs are non-negative or positive for those required.")

  if (min(alphas) < 0 | max(alphas) > 1)
    stop("Each alpha value in alphas must be between 0 and 1, inclusive.")

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

#' Optimized sparse linear regression
#'
#' @description Linear model of a dependent variable on the wide number of sentiment measures and potentially
#' other explanatory variables. Either performs a linear regression on the provided variables at once, or computes
#' linear models sequentially for a given sample size over a longer time horizon, with associated forecasting performance
#' metrics.
#'
#' @param sentomeasures a \code{sentomeasures} object.
#' @param y a one-column \code{data.frame} or \code{numeric} vector capturing the dependent (response) variable.
#' @param x a named \code{data.frame} with other explanatory variables, by default set to \code{NULL}.
#' @param ctr output from a \code{ctr_model()} call.
#'
#' @return If \code{ctr$do.iter == FALSE}, a list containing:
#' \item{reg}{optimized regression object.}
#' \item{alpha}{optimized calibrated alpha.}
#' \item{lambda}{optimized calibrated lambda.}
#' \item{trained}{output from \code{caret::train} call within function (if \code{ctr$type ==} "\code{cv}").}
#' \item{ic}{a \code{numeric} vector of minimum information criterion values per element of \code{alphas} (if
#' \code{ctr$type !=} "\code{cv}").} \item{type}{information criterion used to calibrate (if \code{ctr$type !=} "\code{cv}").}
#'
#' @return If \code{ctr$do.iter == TRUE}, a list containing:
#' \item{regs}{optimized regression objects, i.e. lists as above.}
#' \item{alphas}{optimized calibrated alphass.}
#' \item{lambdas}{optimized calibrated lambdas.}
#' \item{performance}{a \code{data.frame} with performance-related measures, being "\code{RMSFE}" (root mean squared
#' forecasting error), "\code{MAD}" (mean absolute deviation), "\code{MAPE}" (mean absolute percentage error),
#' "\code{DA}" (directional accuracy), and each's respective individual values in the sample.}
#'
#' @seealso \code{\link{ctr_model}}, \code{\link[glmnet]{glmnet}}, \code{\link[caret]{train}}
#'
#' @export
sento_model <- function(sentomeasures, y, x = NULL, ctr) {

  check_class(sentomeasures, "sentomeasures")

  if (!is.data.frame(y)) y <- data.frame(y = y)

  family <- ctr$model
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

  if (family == "lm") family <- "gaussian"

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

  penalty <- rep(1, ncol(xx))
  if (!is.null(x)) penalty[ncol(sentomeasures$measures):ncol(xx)] <- 0 # no shrinkage for original x variables

  ICs <- list()
  lambdaVals <- c()
  for (alpha in alphas) {
    if (do.progress) {
      if (alpha == alphas[1]) cat("alphas run: ", alpha, ", ", sep = "")
      else if (alpha == alphas[length(alphas)]) cat(alpha, "\n")
      else cat(alpha, ", ", sep = "")
    }

    reg <- glmnet::glmnet(x = as.matrix(xx), y = as.matrix(yy), penalty.factor = penalty,
                          lambda = lambdas, alpha = alpha, standardize = TRUE, family = family)

    IC <- compute_IC(reg, yy, xx, alpha, ic)
    ICs[[as.character(alpha)]] <- suppressWarnings(min(IC, na.rm = TRUE))
    lambdaTmp <- ifelse(length(lambdas[which.min(IC)]) > 0, lambdas[which.min(IC)], Inf)
    lambdaVals <- c(lambdaVals, lambdaTmp)
  }

  # retrieve optimal alphas and lambdas
  minLoc <- which.min(ICs)
  lambdaOpt <- lambdaVals[minLoc]
  alphaOpt <- as.numeric(names(minLoc))

  # actual elastic net optimization
  regOpt <- glmnet::glmnet(x = as.matrix(xx), y = as.matrix(yy), penalty.factor = penalty,
                           lambda = lambdaOpt, alpha = alphaOpt, standardize = TRUE, family = family)

  out <- list(reg = regOpt,
              alpha = alphaOpt,
              lambda = lambdaOpt,
              ic = ICs,
              type = ic)

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

  penalty <- rep(1, ncol(xx))
  if (!is.null(x)) penalty[ncol(sentomeasures$measures):ncol(xx)] <- 0 # no shrinkage for original x variables

  # train model based on slices in sliced
  sliced <- create_cv_slices(1:nrow(yy), trainWindow, testWindow = testWindow, skip = oos, do.reverse = FALSE)
  ctrTrain <- caret::trainControl(index = sliced$train, indexOut = sliced$test, allowParallel = TRUE)
  gbmGrid <- expand.grid(alpha = alphas, lambda = lambdas)

  if (do.progress) cat("Training model... ")
  trained <- caret::train(x = as.matrix(xx), y = as.matrix(yy)[, 1], method = "glmnet",
                          family = family, standardize = TRUE, penalty.factor = penalty,
                          trControl = ctrTrain, tuneGrid = gbmGrid, metric = "RMSE")
  if (do.progress) cat("Done.", "\n")

  # retrieve optimal alphas and lambdas
  alphaOpt <- as.numeric(trained$bestTune[1])
  lambdaOpt <- as.numeric(trained$bestTune[2])

  # actual elastic net optimization
  regOpt <- glmnet::glmnet(x = as.matrix(xx), y = as.matrix(yy), penalty.factor = penalty,
                           lambda = lambdaOpt, alpha = alphaOpt, standardize = TRUE, family = family)

  out <- list(reg = regOpt,
              alpha = alphaOpt,
              lambda = lambdaOpt,
              trained = trained)

  return(out)
}

model_CV <- compiler::cmpfun(.model_CV)

.sento_model_iter <- function(sentomeasures, y, x, h, family, alphas, lambdas,
                              type, nSample, start, trainWindow, testWindow, oos, do.progress) {

  nIter <- nrow(y) - nSample - h - oos
  if (nIter <= 0 | start > nIter)
    stop("Data not sufficient to do at least one iteration for given sample size, horizon, out-of-sample skip and start.")

  regsOpt <- list()
  lambdasOpt <- c()
  alphasOpt <- c()

  if (type == "cv") fun <- model_CV
  else fun <- model_IC

  for (i in start:nIter) {
    if (do.progress) cat("iteration: ", i, " from ", as.character(nIter), "\n", sep = "")

    reg <- fun(sentomeasures, y, x, h, alphas, lambdas,
               trainWindow = trainWindow, testWindow = testWindow,
               oos = oos, ic = type, do.progress = do.progress,
               i = i, nSample = nSample, family = family)

    regsOpt[[i - start + 1]] <- reg
  }

  # get optimal alphas and lambdas
  alphasOpt <- sapply(regsOpt, function(x) return(x$alpha))
  lambdasOpt <- sapply(regsOpt, function(x) return(x$lambda))

  # get all predictions (estimations)
  alignedVarsAll <- align_variables(y, sentomeasures, x, h)
  xPred <- alignedVarsAll$x[(start + nSample):(nIter + nSample), ]
  yReal <- alignedVarsAll$y[(start + nSample + oos):(nIter + nSample + oos), ]
  yEst <- rep(NA, nIter - start + 1)
  for (j in 1:(nIter - start + 1)) {
    yEst[j] <- stats::predict(regsOpt[[j]]$reg, newx = as.matrix(xPred[j, ]))
  }
  names(yReal) <- names(yEst) <- sentomeasures$measures$date[start:nIter] # dates

  # compute model performance
  performance <- model_performance(yEst, yReal)

  out <- list(regs = regsOpt,
              alphas = alphasOpt,
              lambdas = lambdasOpt,
              performance = performance)

  return(out)
}

sento_model_iter <- compiler::cmpfun(.sento_model_iter)

retrieve_attribution <- function() {

  ### attribution analysis

  # coeffs[[i - start + 1]] <- as.matrix(stats::coef(regOpt))
  # attribs[[j]] <- coeffs[[j]] * c(1, as.matrix(xPred[j, ]))

}

sento_arima <- function() {

  ### arima for all measures

}

