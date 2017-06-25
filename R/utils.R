
############################################################
################# Utility/Helper functions #################
############################################################

almons <- function(n, orders = 1:3, inverse = FALSE, normalize = TRUE) {

  vals <- 1:n
  inv <- ifelse(inverse, 2, 1)
  almons <- data.frame(matrix(nrow = n, ncol = length(orders) * inv))
  colnames(almons) <- paste0("almon", rep(orders, rep(inv, length(orders))), c("", "_inv")[1:inv])

  for (i in 1:length(orders)) {
    b <- orders[i]

    stdindex <- vals/max(vals)
    if (inverse) {
      stdindex <- cbind(stdindex, - stdindex + 1)
      ind <- (i*2-1):(i*2)
    } else {
      ind <- (ifelse(i == 1, 2, i)-1):i
    }
    almon <- (1 - (stdindex)^b) * stdindex^(max(orders) - b)
    almons[, ind] <- almon
  }

  # normalize weights so that it sums to 1 (if normalize is TRUE)
  if (normalize) almons <- t(t(almons)/colSums(almons))

  return(almons)
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

  return(exponentials)
}

roll_weights <- function(x, w) {

  if (all(is.na(x))) return(NA)
  else {
    return(sum(x[!is.na(x)] * w[!is.na(x)])) # x is sentiment index column, w is weights matrix
  }
}

setup_time_weights <- function(lag, how, ...) {

  if (length(how) > 1) how <- how[1]

  if (how == "equal-weight") {
    weights <- data.frame(matrix(1/lag, nrow = lag, ncol = 1))
    colnames(weights) <- "equal_weight"
  } else if (how == "almon") {
    weights <- almons(lag, ...$orders, ...$inverse, ...$normalize)
  } else if (how == "linear") {
    weights <- data.frame(matrix((1:lag)/sum(1:lag), nrow = lag, ncol = 1))
    colnames(weights) <- "linear_moving"
  } else if (how == "exponential") {
    weights <- exponentials(lag, ...$alphas)
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

create_cv_slices <- function (y, trainWindow, skip = 0, reverse = FALSE) {

  if (trainWindow + skip >= length(y)) stop("(trainWindow + skip) >= length(y).")

  if(reverse) {
    stops <- (seq(along = y))[(length(y)):(trainWindow + skip + 1)]
    test <- as.list(as.integer(stops - initialWindow - skip, SIMPLIFY = FALSE))
  } else {
    stops <- (seq(along = y))[trainWindow:(length(y) - skip - 1)]
    test <- as.list(as.integer(stops + skip + 1))
  }

  starts <- stops - trainWindow + 1
  train <- mapply(seq, starts, stops, SIMPLIFY = FALSE)

  names(train) <- paste("Training", gsub(" ", "0", format(seq(along = train))), sep = "")
  names(test) <- paste("Testing", gsub(" ", "0", format(seq(along = test))), sep = "")

  return(list(train = train, test = test))
}

align_variables <- function(y, sentmeasures, x, h, i = NULL, nSample = NULL) {

  y[is.na(y)] <- 0

  sent <- sentmeasures$measures
  x <- cbind(sent, x)
  x[is.na(x)] <- 0
  dates <- row.names(x)

  if (!all(is.null(i) | is.null(nSample))) {
    X <- x[i:(nSample + i + ifelse(h < 0, abs(h), 0) - 1), ]
    Y <- y[i:(nSample + i + ifelse(h > 0, h , 0) - 1), , drop = FALSE]
  }

  if (h > 0) {
    y <- y[(h + 1):nrow(x), , drop = FALSE]
    x <- x[1:nrow(y), ]
  } else if (h < 0) {
    x <- x[(abs(h) + 1):nrow(y), ]
    y <- y[1:nrow(x), drop = FALSE]
  }

  colnames(y) <- "y"

  return(list(y = y, x = x))
}

