
############################################################
################# Utility/Helper functions #################
############################################################

almons <- function(n, orders = 1:3, do.inverse = TRUE, do.normalize = TRUE) {

  vals <- 1:n
  inv <- ifelse(do.inverse, 2, 1)
  almons <- data.frame(matrix(nrow = n, ncol = length(orders) * inv))
  colnames(almons) <- paste0("almon", rep(orders, rep(inv, length(orders))), c("", "_inv")[1:inv])

  if (n == 1) {
    almons[, ] <- 1
    return(almons)
  }

  for (i in 1:length(orders)) {
    b <- orders[i]

    stdindex <- vals/max(vals)
    if (do.inverse) {
      stdindex <- cbind(stdindex, - stdindex + 1)
      ind <- (i*2-1):(i*2)
    } else {
      ind <- i
    }
    almon <- (1 - (stdindex)^b) * stdindex^(max(orders) - b)
    almons[, ind] <- almon
  }

  # normalize weights so that it sums to 1 (if do.normalize is TRUE)
  if (do.normalize) almons <- t(t(almons)/colSums(almons))

  return(as.data.frame(almons))
}

exponentials <- function(n, alphas = seq(0.1, 0.5, by = 0.1)) {

  vals <- 1:n
  exponentials <- data.frame(matrix(nrow = n, ncol = length(alphas)))
  colnames(exponentials) <- paste0("exp_smoothing_", alphas)

  for (i in 1:length(alphas)) {
    alpha <- alphas[i]

    exponential <- ((alpha * (1 - alpha)^(1 - vals))) / sum((alpha * (1 - alpha)^(1 - vals)))
    exponentials[, i] <- exponential
  }

  return(as.data.frame(exponentials))
}

roll_weights <- function(x, w) {

  if (all(is.na(x))) {
    out <- NA
  } else {
    out <- sum(x[!is.na(x)] * w[!is.na(x)]) # x is sentiment index column, w is weights matrix
  }

  return(out)
}

setup_time_weights <- function(lag, how, ...) { # ... argument should be a list to match with functions in sentomeasures.R

  dots <- tryCatch(list(...)[[1]], # extract list from list of list
                   error = function(x) list(...))

  if (length(how) > 1) how <- how[1]

  if (how == "equal-weight") {
    weights <- data.frame(matrix(1/lag, nrow = lag, ncol = 1))
    colnames(weights) <- "equal_weight"
  } else if (how == "almon") {
    weights <- almons(lag, dots$orders, dots$do.inverse, dots$do.normalize)
  } else if (how == "linear") {
    weights <- data.frame(matrix((1:lag)/sum(1:lag), nrow = lag, ncol = 1))
    colnames(weights) <- "linear_moving"
  } else if (how == "exponential") {
    weights <- exponentials(lag, dots$alphas)
  } else stop("Please select an appropriate aggregation 'how'.")

  return(weights)
}

#' Supported options to perform aggregation into sentiment measures.
#'
#' @description Call for information purposes only. Used within \code{ctrl_agg()} to check if supplied
#' aggregation hows are supported.
#'
#' @return A list with the supported aggregation hows for words (within documents), docs (across documents, per date)
#' and time (across dates).
#'
#' @export
get_hows <- function() {

  words <- c("equal-weight", "proportional", "tf-idf", "counts")
  docs <- c("equal-weight", "proportional")
  time <- c("equal-weight", "almon", "linear", "exponential", "own")

  hows <- list(words = words,
               docs = docs,
               time = time)

  return(hows)
}

create_cv_slices <- function (y, trainWindow, testWindow = 1, skip = 0, do.reverse = FALSE) {

  ### to do: continue end-to-beginning
  if ((trainWindow + skip + testWindow) >= length(y)) stop("(trainWindow + skip + testWindow) >= length(y).")

  if (do.reverse) {
    stops <- seq(along = y)[(length(y)):(trainWindow + skip + testWindow)]
    test <- mapply(seq, stops - skip - trainWindow, stops - skip - trainWindow - testWindow + 1, SIMPLIFY = FALSE)
  } else {
    stops <- seq(along = y)[trainWindow:(length(y) - skip - testWindow)]
    test <- mapply(seq, stops + skip + 1, stops + skip + testWindow, SIMPLIFY = FALSE)
  }

  starts <- stops - trainWindow + 1
  train <- mapply(seq, starts, stops, SIMPLIFY = FALSE)

  names(train) <- paste("Training", gsub(" ", "0", format(seq(along = train))), sep = "")
  names(test) <- paste("Testing", gsub(" ", "0", format(seq(along = test))), sep = "")

  return(list(train = train, test = test))
}

align_variables <- function(y, sentomeasures, x, h, i = 1, nSample = NULL) {

  y[is.na(y)] <- 0

  sent <- sentomeasures$measures[, 2:ncol(sentomeasures$measures)]
  if (is.null(x)) x <- sent
  else x <- cbind(sent, x)
  x <- as.data.frame(x)
  x[is.na(x)] <- 0

  if (h > 0) {
    y <- y[(h + 1):nrow(x), , drop = FALSE]
    x <- x[1:nrow(y), ]
  } else if (h < 0) {
    x <- x[(abs(h) + 1):nrow(y), , drop = FALSE]
    y <- y[1:nrow(x), ]
  }
  if (!is.null(nSample)) {
    x <- x[i:(nSample + i - 1), ]
    y <- y[i:(nSample + i - 1), , drop = FALSE]
  }
  colnames(y) <- "y"

  return(list(y = y, x = x))
}

clean_panel <- function(sentomeasures, threshold = 0.50) {

  # discards columns from sentiment measures panel based on some simple rules
  # useful to simplify penalized variables regression (cf. 'exclude')

  measures <- sentomeasures$measures
  x <- measures[, 2:ncol(measures)]

  duplics <- duplicated(as.matrix(x), MARGIN = 2) # duplicated columns
  manyZeros <- (colSums(as.matrix(x) == 0, na.rm = TRUE) / nrow(x)) > threshold # columns with a too high proportion of zeros

  discard <- duplics & manyZeros
  measuresNew <- cbind(date = measures$date, x[, which(!discard), with = FALSE])

  if (ncol(measuresNew) == 1) stop("No sentiment measures retained after cleaning.")
  else sentomeasures$measures <- measuresNew

  return(sentomeasures)
}

update_info <- function(sentomeasures, newMeasures) {

  n <- ncol(newMeasures)
  newNames <- stringr::str_split(colnames(newMeasures), "--")[2:n] # drop first element (date column)

  sentomeasures$measures <- newMeasures
  sentomeasures$lexicons <- unique(sapply(newNames, "[", 1))
  sentomeasures$features <- unique(sapply(newNames, "[", 2))
  sentomeasures$time <- unique(sapply(newNames, "[", 3))

  return(sentomeasures)
}

check_class <- function(x, class) {
  if (!(class %in% class(x)))
    stop("Please provide a ", class, " object as first argument.")
}

compute_df <- function(alpha, beta, lambda, x) { # degrees-of-freedom estimator of elastic net (cf. Tibshirani and Taylor, 2012)

  df_A <- vector(mode = "numeric", length = length(lambda))
  for(df in 1:length(lambda)) {
    A <- which(beta[, df] != 0)
    if (length(A) == 0) {df_A[df] <- NA; next}
    I <- diag(1, ncol = length(A), nrow = length(A))
    X_A <- as.matrix(x[, A])
    df_A[df] <- sum(diag(X_A %*% solve((t(X_A) %*% X_A + (1 - alpha) * lambda[df] * I)) %*% t(X_A)))
  }

  return(df_A)
}

compute_BIC <- function(y, yEst, df_A, RSS, sigma2) { # BIC-like criterion
  BIC <- RSS/(nrow(y) * sigma2) + log(nrow(y))/nrow(y) * df_A
  return(BIC)
}

compute_AIC <- function(y, yEst, df_A, RSS, sigma2) { # AIC-like criterion
  AIC <- RSS/(nrow(y) * sigma2) + 2/nrow(y) * df_A
  return(AIC)
}

compute_Cp <- function(y, yEst, df_A, RSS, sigma2) { # Mallows's Cp-like criterion
  Cp <- RSS/nrow(y) + 2/nrow(y) * df_A * sigma2
  return(Cp)
}

