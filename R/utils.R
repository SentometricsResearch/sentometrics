
############################################################
################# UTILITY/HELPER FUNCTIONS #################
############################################################

negate <- function(lexicon, s = -1) {
  lexicon$x <- paste0("NOT_", lexicon$x); lexicon$y <- s * (lexicon$y)
  return(lexicon)
}
amplify <- function(lexicon, s = 2) {
  lexicon$x <- paste0("VERY_", lexicon$x); lexicon$y <- s * (lexicon$y)
  return(lexicon)
}
deamplify <- function(lexicon, s = 0.5) {
  lexicon$x <- paste0("HARDLY_", lexicon$x); lexicon$y <- s * (lexicon$y)
  return(lexicon)
}

expand_lexicons <- function(lexicons, types = c(1, 2, 3), scores = c(-1, 2, 0.5)) {
  funcs <- list(negate, amplify, deamplify) # types: 1, 2, 3
  lexiconsExp <- lapply(lexicons, function(l) {
    out <- lapply(c(0, types), function(x) {
      if (x == 0) return(l)
      else {f = funcs[[x]]; return(f(l, scores[[x]]))}
    })
    return(rbindlist(out))
  })
  return(lexiconsExp) # expanded lexicons (original + copied and negated/amplified/deamplified words and scores)
}

# replaces valence words in texts and combines into bigrams
include_valence <- function(texts, val, valIdentifier = c("NOT_", "VERY_", "HARDLY_")) {
  negators <- paste0("\\b", paste0(val[val$t == 1, ]$x, collapse = " \\b|\\b"), " \\b")
  amplif <- paste0("\\b", paste0(val[val$t == 2, ]$x, collapse = " \\b|\\b"), " \\b")
  deamplif <- paste0("\\b", paste0(val[val$t == 3, ]$x, collapse = " \\b|\\b"), " \\b")
  all <- c(negators, amplif, deamplif)
  for (i in seq_along(all)) {
    if (all[i] != "\\b \\b") texts <- stringi::stri_replace_all(texts, valIdentifier[i], regex = all[i])
  }
  return(texts)
}

#' Compute Almon polynomials
#'
#' @description Computes Almon polynomial weighting curves. Handy to self-select specific time aggregation weighting schemes
#' for input in \code{\link{ctr_agg}}.
#'
#' @details The Almon polynomial formula implemented is:
#' \eqn{(1 - (i/n)^{b})(i/n)^{B - b}}{(1 - (i/n)^b) * (i/n)^(B - b)}, where \eqn{i} is the lag index from 1 to \eqn{n}.
#' The inverse is computed by changing \eqn{i/n} to \eqn{1 - i/n}.
#'
#' @param n a single \code{numeric} to indicate the length of the curve (the number of lags, cf., \emph{n}).
#' @param orders a \code{numeric} vector as the sequence of the Almon orders (cf., \emph{b}). The maximum value
#' corresponds to \emph{B}.
#' @param do.inverse \code{TRUE} if the inverse Almon polynomials should be calculated as well.
#' @param do.normalize \code{TRUE} if polynomials should be normalized to unity.
#'
#' @return A \code{data.frame} of all Almon polynomial weighting curves, of size \code{length(orders)} (times two if
#' \code{do.inverse = TRUE}).
#'
#' @seealso \code{\link{ctr_agg}}
#'
#' @export
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
      ind <- (i*2 - 1):(i*2)
    } else {
      ind <- i
    }
    almon <- (1 - (stdindex)^b) * stdindex^(max(orders) - b)
    almons[, ind] <- almon
  }
  if (do.normalize) almons <- t(t(almons)/colSums(almons)) # make weights sum to 1 (if do.normalize is TRUE)
  return(as.data.frame(almons))
}

#' Compute exponential weighting curves
#'
#' @description Computes exponential weighting curves. Handy to self-select specific time aggregation weighting schemes
#' for input in \code{\link{ctr_agg}}.
#'
#' @param n a single \code{numeric} to indicate the length of the curve (the number of lags).
#' @param alphas a \code{numeric} vector of decay factors.
#'
#' @return A \code{data.frame} of exponential weighting curves per value of \code{alphas}.
#'
#' @seealso \code{\link{ctr_agg}}
#'
#' @export
exponentials <- function(n, alphas = seq(0.1, 0.5, by = 0.1)) {
  if (max(alphas) >= 1 & min(alphas) <= 0)
    stop("Values in 'alphas' should be between 0 and 1 (both excluded).")
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

setup_time_weights <- function(lag, how, ...) {
  dots <- tryCatch(list(...)[[1]], # extract list from list of list (... a list to match with functions in sentomeasures.R)
                   error = function(x) list(...)) # if ... is empty
  if (!all(how %in% get_hows()$time)) stop("Please select an appropriate aggregation 'how'.")
  weights <- data.frame(row.names = 1:lag)
  if ("equal_weight" %in% how) {
    weights <- cbind(weights, data.frame(equal_weight = matrix(1/lag, nrow = lag, ncol = 1)))
  }
  if ("linear" %in% how) {
    weights <- cbind(weights, data.frame(linear = matrix((1:lag)/sum(1:lag), nrow = lag, ncol = 1)))
  }
  if ("exponential" %in% how) {
    weights <- cbind(weights, exponentials(lag, dots$alphasExp))
  }
  if ("almon" %in% how) {
    weights <- cbind(weights, almons(lag, dots$ordersAlm, dots$do.inverseAlm, dots$do.normalizeAlm))
  }
  if ("own" %in% how) {
    weights <- cbind(weights, dots$weights)
  }
  return(weights)
}

#' Options supported to perform aggregation into sentiment measures
#'
#' @description Call for information purposes only. Used within \code{\link{ctr_agg}} to check if supplied
#' aggregation hows are supported.
#'
#' @details See the package's \href{https://ssrn.com/abstract=3067734}{vignette} for a thoughtful explanation of
#' the different aggregation options. The \code{howWithin = "proportionalPol"} option divides each document's
#' initial sentiment score by the number of polarized words found in the document (counting each word only once
#' would it appear multiple times), instead of the total number of words which the \code{"proportional"} option gives.
#'
#' @return A list with the supported aggregation hows for arguments \code{howWithin} (\code{"words"}), \code{howDows}
#' (\code{"docs"}) and \code{howTime} (\code{"time"}), to be supplied to \code{\link{ctr_agg}}.
#'
#' @seealso \code{\link{ctr_agg}}
#'
#' @export
get_hows <- function() {
  words <- c("proportional", "proportionalPol", "tf-idf", "counts")
  docs <- c("equal_weight", "proportional")
  time <- c("equal_weight", "almon", "linear", "exponential", "own")
  return(list(words = words, docs = docs, time = time))
}

create_cv_slices <- function (y, trainWindow, testWindow = 1, skip = 0, do.reverse = FALSE) {
  if ((trainWindow + skip + testWindow) >= length(y)) stop("(trainWindow + skip + testWindow) >= length(y).")
  if (do.reverse) {
    stops <- seq(along = y)[(length(y)):(trainWindow + skip + testWindow)]
    test <- mapply(seq, stops - skip - trainWindow, stops - skip - trainWindow - testWindow + 1, SIMPLIFY = FALSE)
  } else {
    stops <- seq(along = y)[trainWindow:(length(y) - skip - testWindow)]
    test <- mapply(seq, stops + skip + 1, stops + skip + testWindow, SIMPLIFY = FALSE)
  }
  names(test) <- paste("Testing", gsub(" ", "0", format(seq(along = test))), sep = "")
  starts <- stops - trainWindow + 1
  train <- mapply(seq, starts, stops, SIMPLIFY = FALSE)
  names(train) <- paste("Training", gsub(" ", "0", format(seq(along = train))), sep = "")
  return(list(train = train, test = test))
}

align_variables <- function(y, sentomeasures, x, h, i = 1, nSample = NULL) {

  if (is.factor(y)) {
    levs <- levels(y)
    yNew <- matrix(nrow = length(y), ncol = length(levs))
    for (j in seq_along(levs)) {
      col <- as.numeric(y == levs[j])
      yNew[, j] <- col
    }
    y <- yNew
    colnames(y) <- levs
  } else {
    y <- as.matrix(y)
    if (NCOL(y) == 1) colnames(y) <- "response"
    row.names(y) <- NULL
  }

  datesX <- sentomeasures$measures$date
  sent <- sentomeasures$measures[, -1] # drop date
  if (is.null(x)) x <- sent
  else x <- cbind(sent, x)
  x <- as.matrix(x)
  x[is.na(x)] <- 0 # check

  if (h > 0) {
    y <- y[(h + 1):nrow(x), , drop = FALSE]
    x <- x[1:nrow(y), , drop = FALSE]
    datesX <- datesX[1:nrow(y)]
  } else if (h < 0) {
    x <- x[(abs(h) + 1):nrow(y), , drop = FALSE]
    datesX <- datesX[(abs(h) + 1):nrow(y)]
    y <- y[1:nrow(x), , drop = FALSE]
  }
  if (!is.null(nSample)) {
    x <- x[i:(nSample + i - 1), , drop = FALSE]
    datesX <- datesX[i:(nSample + i - 1)]
    y <- y[i:(nSample + i - 1), , drop = FALSE]
  }

  return(list(y = y, x = x, datesX = datesX))
}

clean_panel <- function(x, nx, threshold = 0.25) {

  # discards columns only from panel of sentiment measures based on a few simple rules
  # useful to simplify (to some extent) the penalized variables regression (cf. 'exclude')

  start <- ncol(x) - nx + 1
  end <- ncol(x)
  if (nx != 0) xSent <- x[, -start:-end] # drop non-sentiment variables
  else xSent <- x
  xSent[is.na(xSent)] <- 0 # check
  duplics <- duplicated(as.matrix(xSent), MARGIN = 2) # duplicated columns
  manyZeros <- (colSums(as.matrix(xSent) == 0, na.rm = TRUE) / nrow(xSent)) > threshold # columns with too many zeros
  toDiscard <- duplics | manyZeros
  if (ncol(xSent) == 1) stop("No sentiment measures retained after cleaning: too many duplicated columns and/or zeros.")
  else {
    if (nx != 0) {
      xNew <- cbind(xSent[, !toDiscard], x[, start:end])
      colnames(xNew) <- c(names(which(!toDiscard)), colnames(x[, start:end, drop = FALSE]))
    }
    else xNew <- xSent[, !toDiscard]
  }

  return(list(xNew = xNew, discarded = toDiscard))
}

update_info <- function(sentomeasures, newMeasures) {
  check_class(sentomeasures, "sentomeasures")
  n <- ncol(newMeasures)
  newNames <- stringi::stri_split(colnames(newMeasures), regex = "--")[-1] # drop first element (date column)
  sentomeasures$measures <- newMeasures
  sentomeasures$lexicons <- unique(sapply(newNames, "[", 1))
  sentomeasures$features <- unique(sapply(newNames, "[", 2))
  sentomeasures$time <- unique(sapply(newNames, "[", 3))
  sentomeasures$stats <- compute_stats(sentomeasures) # measures in sentomeasures are already updated by here
  return(sentomeasures)
}

check_class <- function(x, class) {
  if (!inherits(x, class))
    stop("Please provide a ", class, " object as first argument.")
}

compute_stats <- function(sentomeasures) {
  measures <- sentomeasures$measures[, -1]
  names <- c("mean", "sd", "max", "min", "meanCorr")
  stats <- data.frame(matrix(NA, nrow = length(names), ncol = length(measures), dimnames = list(names)))
  colnames(stats) <- colnames(measures)
  stats["mean", ] <- measures[, lapply(.SD, mean, na.rm = TRUE)]
  stats["sd", ] <- measures[, lapply(.SD, stats::sd, na.rm = TRUE)]
  stats["max", ] <- measures[, lapply(.SD, max, na.rm = TRUE)]
  stats["min", ] <- measures[, lapply(.SD, min, na.rm = TRUE)]
  if (ncol(measures) > 1) {
    corrs <- stats::cor(measures)
    corrs[corrs == 1] <- NA
    meanCorrs <- colMeans(corrs, na.rm = TRUE)
    stats["meanCorr", ] <- meanCorrs
  } else stats <- stats[row.names(stats) != "meanCorr", , drop = FALSE]
  return(stats)
}

compute_df_old <- function(alpha, beta, lambda, x) { # elastic net degrees-of-freedom estimator (Tibshirani and Taylor, 2012)
  x <- scale(x) # scale x first
  dfA <- lapply(1:length(lambda), function(df) {
    A <- which(beta[, df] != 0)
    if (alpha == 1) {return(length(A))} # df equal to non-zero parameters if LASSO (alpha = 1)
    if (length(A) == 0) {return(NA)}
    I <- diag(1, ncol = length(A), nrow = length(A))
    xA <- as.matrix(x[, A])
    estimate <- tryCatch(sum(diag(xA %*% solve((t(xA) %*% xA + (1 - alpha) * lambda[df] * I)) %*% t(xA))),
                         error = function(x) {NA}) # to handle rare matrix inversion problems
    return(estimate)
  })
  return(unlist(dfA))
}

compute_BIC <- function(y, yEst, dfA, RSS, sigma2) { # BIC-like criterion
  BIC <- RSS/(nrow(y) * sigma2) + (log(nrow(y))/nrow(y)) * dfA
  return(BIC)
}

compute_AIC <- function(y, yEst, dfA, RSS, sigma2) { # AIC-like criterion
  AIC <- RSS/(nrow(y) * sigma2) + (2/nrow(y)) * dfA
  return(AIC)
}

compute_Cp <- function(y, yEst, dfA, RSS, sigma2) { # Mallows's Cp-like criterion
  Cp <- RSS/nrow(y) + (2/nrow(y)) * dfA * sigma2
  return(Cp)
}

to_long <- function(measures) { # changes format of sentiment measures data.table from wide to long
  dates <- measures$date
  n <- length(dates)
  names <- colnames(measures)[-1]
  measuresTrans <- as.data.table(t(measures[, -1]))
  colnames(measuresTrans) <- as.character(1:n)
  triplets <- stringi::stri_split(names, regex = "--")
  measuresTrans[, "lexicons" := sapply(triplets, "[", 1)]
  measuresTrans[, "features" := sapply(triplets, "[", 2)]
  measuresTrans[, "time" := sapply(triplets, "[", 3)]
  long <- melt(measuresTrans, id.vars = c("lexicons", "features", "time"), variable.name = "toDrop")
  long[, "toDrop" := NULL]
  long[, "date" := rep(dates, rep(length(names), length(dates)))][]
  return(long)
}

nonzero_coeffs <- function(reg) {
  coeffs <- stats::coef(reg)
  df <- as.data.frame(as.matrix(coeffs))
  nz <- df[df != 0]
  names(nz) <- row.names(df)[df != 0]
  if (length(nz) == 0) return("All coefficients are equal to zero.")
  nz <- data.frame(nz)
  colnames(nz) <- NULL
  return(nz)
}

pdf_manual <- function(wd) {
  setwd(wd)
  shell('R CMD Rd2pdf --encoding=UTF-8 sentometrics')
  setwd(paste0(wd, "/sentometrics"))
}

# this function is directly taken from the sentimentr package
# (the as_key() function) but copied to bring R version
# depends down and decrease number of dependencies by one
# the function is slightly simplified (only 1 argument)
sento_as_key <- function (x, ...) {
  stopifnot(is.data.frame(x))
  culprits <- NULL
  if (length(x[[1]]) != length(unique(x[[1]]))) {
    tab <- table(x[[1]])
    culprits <- paste(paste0("   * ", sort(names(tab[tab > 1]))), collapse = "\n")
    warning("One or more terms in the first column are repeated. Terms must be unique.\n  ",
            "I found the following likely culprits:\n\n", culprits,
            "\n\nThese terms have been dropped.\n")
  }
  if (any(grepl("[A-Z]", x[[1]]))) {
    culprits2 <- grep("[A-Z]", x[[1]], value = TRUE)
    culprits2 <- paste(paste0("   * ", culprits2), collapse = "\n")
    warning("One or more terms in the first column contain capital letters. Capitals are ignored.\n  ",
            "I found the following suspects:\n\n", culprits2,
            "\n\nThese terms have been lower cased.\n")
    x[[1]] <- tolower(x[[1]])
  }
  if (is.factor(x[[1]])) {
    warning("Column 1 was a factor...\nConverting to character.")
    x[[1]] <- as.character(x[[1]])
  }
  if (!is.character(x[[1]]))
    stop("Column 1 must be character.")
  if (!is.numeric(x[[2]]))
    stop("Column 2 must be numeric.")
  colnames(x) <- c("x", "y")
  data.table::setDT(x)
  x <- x[order(x), ]
  if (!is.null(culprits))
    x <- x[!x %in% sort(names(tab[tab > 1])), ]
  data.table::setkey(x, "x")
  x
}

