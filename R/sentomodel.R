
### TO DO: implement other IC via msgps

#' Add and fill missing dates
#'
#' @description Adds missing dates between earliest and latest date, such that time series is continuous on a period-by-period
#' basis. Fills in these dates with either \code{NA} or the respective latest non-\code{NA} values.
#'
#' @param sentomeasures a \code{sentomeasures} object.
#' @param fillLatest a logical, if \code{TRUE} fills added dates with most recent value.
#'
#' @return A modified \code{sentomeasures} object.
#'
#' @export
fill_measures <- function(sentomeasures, do.fillLatest = FALSE) {

  ### mice package?

  if (!("sentomeasures" %in% class(sentomeasures))) stop("Please provide a sentomeasures object as first argument.")

  by <- sentomeasures$by

  measures <- sentomeasures$measures
  dates <- measures$date
  ts <- seq(dates[1], dates[length(dates)], by = by)
  dt <- data.table(date = ts)

  # join to new measures
  measuresFill <- merge(dt, measures, by = "date", all = TRUE)
  if (do.fillLatest) measuresFill <- zoo::na.locf(measuresFill)
  sentomeasures$measures <- measuresFill

  return(sentomeasures)
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

model_performance <- function(yEst, yReal) {

  # model errors
  error <- yEst - yReal
  error2 <- error^2
  MAD <- abs(error)
  MAPE <- 100 * MAD / yReal
  errors <- data.frame(cbind(error, error2, MAD, MAPE))
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
#' as implemented in the \pkg{glmnet} package, to account for the sparsity of the textual sentiment measures.
#'
#' @param model a character vector indicator "\code{lm}" (linear model) or "\code{nlm}" (nonlinear model).
#' @param type a character vector indicating which model selection criteria to use. Currently supports "\code{BIC}" (BIC-like
#' criterion, cf. Zou, Hastie, Tibshirani, et al. (2007). "On the 'degrees of freedom' of the LASSO.") and "\code{cv}"
#' (cross-validation based on the \code{train} function from the \pkg{caret} package).
#' @param h a non-negative integer value to shift the time series to have the desired (forecasting) setup, \code{h == 0} means
#' no change to input data, \code{h > 0} shifts the dependent variable by \code{h} periods, \code{h < 0} shifts the independent
#' variables by \code{h} rows.
#' @param alphas a numeric vector of the different alphas to test for during optimization, between 0 and 1.
#' @param lambdas a numeric vector of the different lambdas to test for during optimization.
#' @param trainWindow a positive integer of the size of the training sample in the cross-validation (ignored if \code{type !=}
#' "\code{cv}")
#' @param oos a non-negative integer to indicate the number of periods to skip from the end of the cross-validation training
#' sample (out-of-sample) up to the test sample (ignored if \code{type !=} "\code{cv}")
#' @param do.iter a logical, \code{TRUE} will induce an iterative optimization of models through time.
#' @param nSample a positive integer, size of the sample for model calibration at every iteration (ignored if
#' \code{iter == FALSE})
#' @param start a positive integer to indicate at which point the iteration has to start (ignored if \code{iter == FALSE})
#'
#' @return A list encapsulating the control parameters.
#'
#' @export
ctr_model <- function(model = c("lm", "nlm"), type = c("BIC", "cv"), h = 1,
                      lambdas = 10^seq(2, -2, length.out = 50), alphas = seq(0.05, 0.95, by = 0.10),
                      nSample = NULL, trainWindow = NULL, oos = 0, do.iter = FALSE, start = 1) {

  if (length(type) > 1) type <- type[1]

  if (!(type %in% c("BIC", "cv")))
    stop("Provide a proper modelling type.")

  if (h < 0 | oos < 0 | start <= 0)
    stop("Make sure all integers are non-negative or positive as required.")

  if (min(alphas) < 0 | max(alphas) > 1)
    stop("Each alpha value in alphas must be between 0 and 1, inclusive.")

  if (do.iter & is.null(nSample))
    stop("Iterative modelling requires a non-NULL sample size given by nSample.")

  if (!is.null(nSample))
    if (nSample <= 0)
      stop("Make sure all integers are non-negative or positive as required.")

  if (type == "cv" & is.null(trainWindow))
    stop("Cross-validation requires a non-NULL training window size give by trainWindow.")

  if (!is.null(trainWindow))
    if (trainWindow <= 0)
      stop("Make sure all integers are non-negative or positive as required.")

  if (!is.null(nSample) & !is.null(trainWindow))
    if (trainWindow + oos >= nSample)
      stop("(trainWindow + oos) >= nSample. Adjust windows selection accordingly.")

  ctr_model <- list(model = model,
                    type = type,
                    do.iter = do.iter,
                    h = h,
                    nSample = nSample,
                    start = start,
                    oos = oos,
                    alphas = alphas,
                    lambdas = lambdas,
                    trainWindow = trainWindow)

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
#' @param y one-column \code{data.frame} of the dependent (response) variable.
#' @param x a named \code{data.frame} with other explanatory variables.
#' @param ctr output from a \code{ctr_model()} call.
#'
#' @return A list containing:
#' \item{reg}{optimized regression object.}
#' \item{alpha}{optimized calibrated alphas(s).}
#' \item{lambda}{optimized calibrated lambda(s).}
#'
#' @seealso \code{\link{ctr_model}}, \code{\link[glmnet]{glmnet}}, \code{\link[caret]{train}}
#'
#' @export
sento_lm <- function(sentomeasures, y, x, ctr) {

  if (!("sentomeasures" %in% class(sentomeasures))) stop("Please provide a sentomeasures object as first argument.")

  if (ctr$model != "lm") stop("Expected model is 'lm'.")

  do.iter <- ctr$do.iter
  type <- ctr$type
  h <- ctr$h
  alphas <- ctr$alphas
  lambdas <- ctr$lambdas

  if (!iter) {
    if (type == "BIC")
      out <- lm_BIC(sentomeasures = sentomeasures, y = y, x = x, h = h, alphas = alphas, lambdas = lambdas)
    else if (type == "cv")
      out <- lm_cv(sentomeasures = sentomeasures, y = y, x = x, h = h, alphas = alphas, lambdas = lambdas,
                   trainWindow = ctr$trainWindow, oos = ctr$oos)
  } else {
    nSample <- ctr$nSample
    start <- ctr$start
    oos <- ctr$oos

    out <- sento_lm_iter(type = type, sentomeasures = sentomeasures, y = y, x = x, h = h, nSample = nSample, start = start,
                         oos = oos, alphas = alphas, lambdas = lambdas, trainWindow = ctr$trainWindow)
  }

  return(out)
}

lm_BIC <- function(sentomeasures, y, x, h, alphas, lambdas, ...) {

  dots <- list(...)

  # clean measures (e.g. get rid of duplicated or too sparse measures)
  sentomeasures <- clean_panel(sentomeasures)

  alignedVars <- align_variables(y, sentomeasures, x, h, i = dots$i, nSample = dots$nSample)
  y <- alignedVars$y
  x <- alignedVars$x

  penalty <- rep(1, ncol(x))
  penalty[(ncol(sentomeasures$measures) + 1):ncol(x)] <- 0 # no shrinkage for original x variables

  low <- c(rep(-Inf, ncol(x)))
  up <- c(rep(Inf, ncol(x)))

  BICs <- list()
  for (alpha in alphas) {
    cat(alpha, "\n")

    ### for alpha == 0, glmnet is slower
    reg <- glmnet::glmnet(x = as.matrix(x), y = as.matrix(y), penalty.factor = penalty, alpha = alpha,
                          lambda = lambdas, standardize = TRUE, lower.limits = low, upper.limits = up)

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
  regOpt <- glmnet::glmnet(x = as.matrix(x), y = as.matrix(y), penalty.factor = penalty, lambda = lambdaOpt,
                           alpha = alphaOpt, standardize = TRUE, lower.limits = low, upper.limits = up)

  out <- list(reg = regOpt,
              alpha = alphaOpt,
              lambda = lambdaOpt)

  return(out)
}

lm_cv <- function(sentomeasures, y, x, h, alphas, lambdas, ...) {

  dots <- list(...)

  # clean measures (e.g. get rid of duplicated or too sparse measures)
  sentomeasures <- clean_panel(sentomeasures)

  alignedVars <- align_variables(y, sentomeasures, x, h, i = dots$i, nSample = dots$nSample)
  y <- alignedVars$y
  x <- alignedVars$x

  penalty <- rep(1, ncol(alignedVars$x))
  penalty[(ncol(sentomeasures$measures) + 1):ncol(x)] <- 0 # no shrinkage for original x variables

  sliced <- create_cv_slices(1:nrow(y), dots$trainWindow, skip = dots$oos, do.reverse = FALSE)
  ctr <- caret::trainControl(index = sliced$train, indexOut = sliced$test, allowParallel = TRUE)

  gbmGrid <- expand.grid(alpha = alphas, lambda = lambdas)

  ### parallelization to add
  ### check warnings and sanity regarding caret::train() setup

  low <- c(rep(-Inf, ncol(x)))
  up <- c(rep(Inf, ncol(x)))

  # train model based on slices in sliced
  ### Rsquared values are NA...
  trained <- caret::train(y ~ ., data = cbind(y, x), method = "glmnet", family = "gaussian", standardize = TRUE,
                          penalty.factor = penalty, trControl = ctr, tuneGrid = gbmGrid, lower.limits = low,
                          upper.limits = up, metric = "RMSE")

  # retrieve optimal alphas and lambdas
  alphaOpt <- as.numeric(trained$bestTune[1])
  lambdaOpt <- as.numeric(trained$bestTune[2])

  # actual LASSO optimization
  regOpt <- glmnet::glmnet(x = as.matrix(alignedVars$x), y = as.matrix(alignedVars$y), penalty.factor = penalty,
                           lambda = lambdaOpt, alpha = alphaOpt, standardize = TRUE, lower.limits = low, upper.limits = up)

  out <- list(reg = regOpt,
              alpha = alphaOpt,
              lambda = lambdaOpt)

  return(out)
}

.sento_lm_iter <- function(type, sentomeasures, y, x, h, nSample, start, oos, alphas, lambdas, trainWindow) {

  nIter <- nrow(y) - nSample - h - oos
  if (nIter <= 0 | start > nIter)
    stop("Data not sufficient to do at least one iteration for given sample size, horizon, out-of-sample skip and start.")

  attribs <- list()
  coeffs <- list()
  regsOpt <- list()
  lambdasOpt <- c()
  alphasOpt <- c()

  if (type == "BIC") fun <- lm_BIC
  else if (type == "cv") fun <- lm_cv

  for (i in start:nIter) {
    cat("iter:", i, "\n")

    reg <- fun(sentomeasures, y, x, h, alphas, lambdas, i, nSample, oos, trainWindow)
    regOpt <- reg$reg

    coeffs[[i - start + 1]] <- as.matrix(coef(regOpt))
    regsOpt[[i - start + 1]] <- regOpt
    alphasOpt[i - start + 1] <- reg$alpha
    lambdasOpt[i - start + 1] <- reg$lambda
  }

  alignedVarsAll <- align_variables(y, sentomeasures, x, h)
  xPred <- alignedVarsAll$x[(start + nSample):(nIter + nSample), ]
  yReal <- alignedVarsAll$y[(start + nSample + oos):(nIter + nSample + oos), ]

  yEst <- rep(NA, nIter - start + 1)
  for (j in 1:(nIter - start + 1)) {
    attribs[[j]] <- coeffs[[j]] * c(1, as.matrix(xPred[j, ]))
    yEst[j] <- predict(regsOpt[[j]], newx = as.matrix(xPred[j, ]))
  }

  names(yReal) <- names(yEst) <- sentomeasures$measures$date[start:nIter] # dates

  performance <- model_performance(yEst, yReal)

  out <- list(coeff = coeffs,
              attribs = attribs,
              alphas = alphasOpt,
              lambdas = lambdasOpt,
              reg = regsOpt,
              performance = performance)

  return(out)
}

sento_lm_iter <- compiler::cmpfun(.sento_lm_iter) ### still too slow

sento_univariate <- function() {

  # arima for all measures

}

