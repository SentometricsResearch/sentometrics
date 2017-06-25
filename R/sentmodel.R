
### TO DO: implement other IC via msgps

#' Add and fill missing dates
#'
#' @description Adds missing dates between earliest and latest date, such that time series is continuous on a day-by-day basis. Fills in
#' these dates with either \code{NA} or the respective latest non-\code{NA} values.
#'
#' @param sentmeasures a \code{sentmeasures} object.
#' @param fillLatest a logical, if \code{TRUE} fills added dates with most recent value.
#'
#' @return A modified \code{sentmeasures} object.
#'
#' @export
fill_measures <- function(sentmeasures, fillLatest = FALSE) {

  if (!("sentmeasures" %in% class(sentmeasures))) stop("Please provide a sentmeasures object as first argument.")

  measures <- sentmeasures$measures
  dates <- as.Date(rownames(measures), format = '%Y-%m-%d')
  ts <- seq.POSIXt(as.POSIXct(dates[1], format = '%Y-%m-%d'), as.POSIXct(dates[length(dates)], format = '%Y-%m-%d'), by = "day")

  df <- data.frame(dates = as.Date(ts, format = '%Y-%m-%d'))

  # join to new measures and avoid that dates have been shifted because of dates conversions
  measuresFill = dplyr::full_join(df, cbind(dates = dates, measures), by = "dates")
  measuresFill <- measuresFill[which(measuresFill$dates == dates[1]):which(measuresFill$dates == dates[length(dates)]), ]
  row.names(measuresFill) <- measuresFill$dates
  measuresFill$dates <- NULL

  sentmeasures$measures <- measuresFill

  if (fillLatest) measuresFill <- zoo::na.locf(measuresFill)

  return(sentmeasures)
}

BIC_like <- function(reg, y, x, alpha) {

  beta <- reg$beta
  lambda <- reg$lambda

  df_A = vector(mode = "numeric", length = length(lambda))

  for(df in 1:length(lambda)) {
    A <- which(beta[, df] != 0)
    if (length(A) == 0) {df_A[df] <- NA; next}
    I <- diag(1, ncol = length(A), nrow = length(A))
    X_A <- as.matrix(x[, A])
    df_A[df] <- sum(diag(X_A %*% solve((t(X_A) %*% X_A + (1 - alpha) * lambda[df] * I)) %*% t(X_A)))
  }

  y_est = predict(reg, newx = as.matrix(x))

  RSS = apply(y_est, 2, FUN = function(est) sum((y - est)^2))
  sigma2 = RSS[length(RSS)] / (nrow(y) - df_A[length(RSS)])
  BIC = RSS/(nrow(y) * sigma2) + log(nrow(y))/nrow(y) * df_A

  return(BIC)
}

model_errors <- function(yEst, yReal) {

  error <- yEst - yReal
  error2 <- error^2
  MAD <- abs(error)
  MAPE <- 100 * MAD / yReal

  errors <- data.frame(cbind(error, error2, MAD, MAPE))
  colnames(errors) <- c("error", "error^2", "AD", "APE")

  meanErrors <- colMeans(errors, na.rm = TRUE)[2:ncol(errors)]

  errorsAll <- list(errors = errors,
                    RMSFE = as.numeric(sqrt(meanErrors["error^2"])),
                    MAD = as.numeric(meanErrors["AD"]),
                    MAPE = as.numeric(meanErrors["APE"]))

  return(errorsAll)

}

#' Setup control for sentiment measures-based regression modelling
#'
#' @description Sets up control for linear or nonlinear modelling of a response variable onto a sparse panel of
#' textual sentiment measures (and potentially other variables). Models are computed using the elastic-net regularization
#' as implemented in the \pkg{glmnet} package, to account for the sparsity of the textual sentiment measures.
#'
#' @param model a character vector indicator "\code{lm}" (linear model) or "\code{nlm}" (nonlinear model).
#' @param type a character vector indicating which model selection criteria to use. Currently supports "\code{BIC}" (BIC-like criterion,
#' cf. Zou, Hastie, Tibshirani, et al. (2007). "On the 'degrees of freedom' of the LASSO.") and "\code{cv}" (cross-validation based on
#' the \code{train} function from the \pkg{caret} package).
#' @param h an integer value to shift the time series to have the desired (forecasting) setup, \code{h == 0} means no change to input,
#' data, \code{h > 0} shifts the dependent variable by \code{h} periods, \code{h < 0} shifts the independent variables by \code{h} rows.
#' @param alphas a numeric vector of the different alphas to test for during optimization.
#' @param lambdas a numeric vector of the different lambdas to test for during optimization.
#' @param trainWindow an integer of the size of the training sample in the cross-validation (ignored if \code{type !=} Z"\code{cv}")
#' @param oos an integer to indicate the number of periods to skip from the end of the cross-validation training sample
#' (out-of-sample) up to the test sample (ignored if \code{type !=} "\code{cv}")
#' @param iter a logical, \code{TRUE} will induce an iterative optimization of models through time.
#' @param nSample an integer, size of the sample for model calibration at every iteration (ignored if \code{iter} == \code{FALSE})
#' @param start an integer to indicate at which point the iteration has to start (ignored if \code{iter} == \code{FALSE})
#'
#' @return A list encapsulating the control parameters.
#'
#' @export
ctrl_model <- function(model = c("lm", "nlm"), type = c("BIC", "cv"), h = 1, alphas = seq(0.05, 0.95, by = 0.10),
                       lambdas = 10^seq(2, -2, length.out = 50), trainWindow = NULL, oos = 0, iter = FALSE, nSample = 100, start = 1) {

  ### additional checks (e.g. trainWindow < nSample)

  if (length(type) > 1) type <- type[1]

  if (!is.null(trainWindow))
    if(trainWindow + oos >= nSample) stop("(trainWindow + oos) >= nSample. Adjust windows selection accordingly.")

  if (!(type %in% c("BIC", "cv"))) stop("Provide a proper modelling type.")

  ctrl_model <- list(model = model,
                     type = type,
                     iter = iter,
                     h = h,
                     nSample = nSample,
                     start = start,
                     oos = oos,
                     alphas = alphas,
                     lambdas = lambdas,
                     trainWindow = trainWindow)

  return(ctrl_model)

}

#' Optimized sparse linear regression
#'
#' @description Linear model of a dependent variable on the wide number of sentiment measures and potentially
#' other explanatory variables. Either performs a linear regression on the provided variables at once, or computes
#' linear models sequentially for a given sample size over a longer time horizon, with associated forecasting performance
#' metrics.
#'
#' @param sentmeasures a \code{sentmeasures} object.
#' @param y one-column \code{data.frame} of the dependent (response) variable.
#' @param x a named \code{data.frame} with other explanatory variables.
#' @param ctrlModel output from a \code{ctrl_model()} call.
#'
#' @return A list containing:
#' \item{reg}{optimized regression object.}
#' \item{alpha}{optimized calibrated alphas(s).}
#' \item{lambda}{optimized calibrated lambda(s).}
#'
#' @seealso \code{\link{ctrl_model}}, \code{\link[glmnet]{glmnet}}, \code{\link[caret]{train}}
#'
#' @export
lm_sent <- function(sentmeasures, y, x, ctrlModel) {

  if (!("sentmeasures" %in% class(sentmeasures))) stop("Please provide a sentmeasures object as first argument.")

  if (ctrlModel$model != "lm") stop("Expected model is 'lm'.")

  iter <- ctrlModel$iter
  type <- ctrlModel$type
  h <- ctrlModel$h
  alphas <- ctrlModel$alphas
  lambdas <- ctrlModel$lambdas

  if (!iter) {
    if (type == "BIC")
      out <- lm_BIC(sentmeasures = sentmeasures, y = y, x = x, h = h, alphas = alphas, lambdas = lambdas)
    else if (type == "cv")
      out <- lm_cv(sentmeasures = sentmeasures, y = y, x = x, h = h, alphas = alphas, lambdas = lambdas,
                   trainWindow = ctrlModel$trainWindow, oos = ctrlModel$oos)
  } else {
    nSample <- ctrlModel$nSample
    start <- ctrlModel$start
    oos <- ctrlModel$oos

    out <- sent_lm_iter(type = type, sentmeasures = sentmeasures, y = y, x = x, h = h, nSample = nSample, start = start,
                        oos = oos, alphas = alphas, lambdas = lambdas, trainWindow = ctrlModel$trainWindow)
  }

  return(out)
}

lm_BIC <- function(sentmeasures, y, x, h, alphas, lambdas, ...) {

  lower_bounds = c(rep(-Inf, ncol(sentmeasures$measures)))
  upper_bounds = c(rep(Inf, ncol(sentmeasures$measures)))

  alignedVars <- align_variables(y, sentmeasures, x, h, ...)
  y <- alignedVars$y
  x <- alignedVars$x

  ### purpose/definition of threshold? + change also in _cv model
  threshold <- Inf
  exclude = unique(c(which(duplicated(as.matrix(x), MARGIN = 2)),
                     which((colSums(as.matrix(x) == 0, na.rm = TRUE) / nSample) > threshold)))

  penalty <- rep(1, ncol(x))
  penalty[ncol(x)] <- 0 ### change to zero for ALL non-sentiment columns

  BICs <- list()
  for (alpha in alphas) {
    cat(alpha, "\n")

    ### for alpha == 0, glmnet is slower
    reg <- glmnet::glmnet(x = as.matrix(x),
                          y = as.matrix(y),
                          exclude = exclude,
                          penalty.factor = penalty,
                          alpha = alpha,
                          lambda = lambdas,
                          standardize = TRUE,
                          lower.limits = c(lower_bounds, -Inf),
                          upper.limits = c(upper_bounds, Inf))

    lambdaVals <- reg$lambda
    bic <- BIC_like(reg, y, x, alpha)
    BICs[[as.character(alpha)]] <- suppressWarnings(min(bic, na.rm = TRUE))
    lambdas <- c(lambdas, lambdaVals[which.min(bic)])
  }

  # retrieve optimal alphas and lambdas
  minLoc <- which.min(BICs)
  lambdaOpt <- lambdas[minLoc]
  alphaOpt <- as.numeric(names(minLoc))

  # actual LASSO optimization
  regOpt <- glmnet::glmnet(x = as.matrix(x),
                           y = as.matrix(y),
                           exclude = exclude,
                           penalty.factor = penalty,
                           lambda = lambdaOpt,
                           alpha = alphaOpt,
                           standardize = TRUE,
                           lower.limits = c(lower_bounds, -Inf),
                           upper.limits = c(upper_bounds, Inf))

  out <- list(reg = regOpt,
              alpha = alphaOpt,
              lambda = lambdaOpt)

  return(out)

}

lm_cv <- function(sentmeasures, y, x, h, alphas, lambdas, trainWindow, oos, ...) {

  lower_bounds = c(rep(-Inf, ncol(sentmeasures$measures)))
  upper_bounds = c(rep(Inf, ncol(sentmeasures$measures)))

  alignedVars <- align_variables(y, sentmeasures, x, h, ...)
  data = cbind(alignedVars$y, alignedVars$x)

  penalty <- rep(1, ncol(alignedVars$x))
  penalty[ncol(alignedVars$x)] <- 0 ### change to zero for ALL non-sentiment columns

  sliced <- create_cv_slices(1:nrow(data), trainWindow = trainWindow, skip = oos, reverse = FALSE)
  ctrl <- caret::trainControl(index = sliced$train, indexOut = sliced$test, allowParallel = TRUE)

  gbmGrid <-  expand.grid(alpha = alphas, lambda = lambdas)

  ### parallelization to add
  ### check warnings and sanity regarding caret::train setup

  # train model based on slices in sliced
  trained <- caret::train(y ~ .,
                          data = data,
                          method = "glmnet",
                          family = "gaussian",
                          standardize = TRUE,
                          penalty.factor = penalty,
                          trControl = ctrl,
                          tuneGrid = gbmGrid,
                          lower.limits = c(lower_bounds, -Inf), ### in function of number of columns (all lower/upper.limits!)
                          upper.limits = c(upper_bounds, Inf),
                          metric = "RMSE")

  # retrieve optimal alphas and lambdas
  alphaOpt <- as.numeric(trained$bestTune[1])
  lambdaOpt <- as.numeric(trained$bestTune[2])

  ### purpose? (cf. above)
  threshold <- Inf
  exclude = unique(c(which(duplicated(as.matrix(x), MARGIN = 2)),
                     which((colSums(as.matrix(x) == 0, na.rm = TRUE) / nSample) > threshold)))

  # actual LASSO optimization
  regOpt <- glmnet::glmnet(x = as.matrix(alignedVars$x),
                           y = as.matrix(alignedVars$y),
                           exclude = exclude,
                           penalty.factor = penalty,
                           lambda = lambdaOpt,
                           alpha = alphaOpt,
                           standardize = TRUE,
                           lower.limits = c(lower_bounds, -Inf),
                           upper.limits = c(upper_bounds, Inf))

  out <- list(reg = regOpt,
              alpha = alphaOpt,
              lambda = lambdaOpt)

}

.sent_lm_iter <- function(type, sentmeasures, y, x, h, nSample, start, oos, alphas, lambdas, trainWindow) {

  lower_bounds = c(rep(-Inf, ncol(sentmeasures$measures)))
  upper_bounds = c(rep(Inf, ncol(sentmeasures$measures)))

  ### role of nSample vs. trainWindow (nSample == trainWindow?)
  nIter = nrow(y) - nSample - h

  attribs = list()
  coeffs <- list()
  regsOpt <- list()
  lambdasOpt <- c()
  alphasOpt <- c()

  dates <- row.names(sentmeasures)
  yReal <- data.frame(rep(NA, nIter - start + 1), row.names = dates[start:nIter])
  yEst <- data.frame(rep(NA, nIter - start + 1), row.names = dates[start:nIter])

  if (type == "BIC") fun <- lm_BIC
  else if (type == "cv") fun <- lm_cv

  alignedVarsAll <- align_variables(y, sentmeasures, x, h) ### inefficient (also called in fun())
  for (i in start:nIter) {
    cat(i, "\n")

    reg <- fun(sentmeasures, y, x, h, alphas, lambdas, i, nSample)
    regOpt <- reg$reg

    coeffs[[i - start + 1]] <- as.matrix(coef(regOpt))
    regsOpt[[i - start + 1]] <- regOpt
    alphasOpt[i - start + 1] <- reg$alpha
    lambdasOpt[i - start + 1] <- reg$lambda

    xPred <- as.matrix(alignedVarsAll$x[nSample + i, ])
    attribs[[i]] <- coeffs[[i - start + 1]] * c(1, xPred)

    yEst[i - start + 1, ] <- predict(regOpt, newx = xPred)
    yReal[i - start + 1, ] <- alignedVarsAll$y[(nSample + i + oos + h), ]

  }

  errors <- model_errors(yEst, yReal)

  out <- list(coeff = coeffs,
              attribs = attribs,
              alphas = alphasOpt,
              lambdas = lambdasOpt,
              reg = regOpt,
              performance = errors)

  return(out)

}

sent_lm_iter <- compiler::cmpfun(.sent_lm_iter) ### still too slow

sent_univariate <- function() {

  # arima for all measures

}

