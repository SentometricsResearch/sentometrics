
###########################################################
#################### UTILITY FUNCTIONS ####################
###########################################################

#' Compute exponential weighting curves
#'
#' @description Computes exponential weighting curves. Handy to self-select specific time aggregation weighting schemes
#' for input in \code{\link{ctr_agg}} using the \code{weights} argument.
#'
#' @param n a single \code{numeric} to indicate the lag length.
#' @param alphas a \code{numeric} vector of decay factors.
#'
#' @return A \code{data.frame} of exponential weighting curves per value of \code{alphas}.
#'
#' @seealso \code{\link{ctr_agg}}
#'
#' @export
weights_exponential <- function(n, alphas = seq(0.1, 0.5, by = 0.1)) {
  if (max(alphas) >= 1 || min(alphas) <= 0)
    stop("Values in 'alphas' should be between 0 and 1 (both excluded).")
  vals <- 1:n
  exponentials <- data.frame(matrix(nrow = n, ncol = length(alphas)))
  colnames(exponentials) <- paste0("exponential_", alphas)
  for (i in 1:length(alphas)) {
    alpha <- alphas[i]
    exponential <- ((alpha * (1 - alpha)^(1 - vals))) / sum((alpha * (1 - alpha)^(1 - vals)))
    exponentials[, i] <- exponential
  }
  return(as.data.frame(exponentials))
}

#' Compute Almon polynomials
#'
#' @description Computes Almon polynomial weighting curves. Handy to self-select specific time aggregation weighting schemes
#' for input in \code{\link{ctr_agg}} using the \code{weights} argument.
#'
#' @details The Almon polynomial formula implemented is:
#' \eqn{(1 - (1 - i/n)^{b})(1 - i/n)^{B - b}}{(1 - (1 - i/n)^b) * (1 - i/n)^(B - b)}, where \eqn{i} is the lag index ordered from
#' 1 to \eqn{n}. The inverse is computed by changing \eqn{i/n} to \eqn{1 - i/n}.
#'
#' @param n a single \code{numeric} to indicate the lag length (cf., \emph{n}).
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
weights_almon <- function(n, orders = 1:3, do.inverse = TRUE, do.normalize = TRUE) {
  vals <- (1:n) / n
  inv <- ifelse(do.inverse, 2, 1)
  almons <- data.frame(matrix(nrow = n, ncol = length(orders) * inv))
  colnames(almons) <- paste0("almon", rep(orders, rep(inv, length(orders))), c("", "_inv")[1:inv])
  if (n == 1) {
    almons[, ] <- 1
    return(almons)
  }
  for (i in 1:length(orders)) {
    b <- orders[i]
    stdindex <- 1 - vals
    if (do.inverse) {
      stdindex <- cbind(stdindex, - stdindex + 1)
      ind <- (i*2 - 1):(i*2)
    } else {
      ind <- i
    }
    almon <- (1 - stdindex^b) * (stdindex)^(max(orders) - b)
    almons[, ind] <- almon
  }
  if (do.normalize) almons <- t(t(almons)/colSums(almons)) # make weights sum to 1 (if do.normalize is TRUE)
  return(as.data.frame(almons)) # first row is most lagged value
}

#' Compute Beta weighting curves
#'
#' @description Computes Beta weighting curves as in Ghysels, Sinko and Valkanov (2007). Handy to self-select specific
#' time aggregation weighting schemes for input in \code{\link{ctr_agg}} using the \code{weights} argument.
#'
#' @details The Beta weighting abides by following formula:
#' \eqn{f(i/n; a, b) / \sum_{i}(i/n; a, b)}{f(i/n; a, b) / \sum(i/n; a, b)}, where \eqn{i} is the lag index ordered
#' from 1 to \eqn{n}, \eqn{a} and \eqn{b} are two decay parameters, and
#' \eqn{f(x; a, b) = (x^{a - 1}(1 - x)^{b - 1}\Gamma(a + b)) / (\Gamma(a)\Gamma(b))}{f(x; a, b)
#'  = (x^(a - 1) * (1 - x)^(b - 1) * T(a + b)) / (T(a) * T(b))}, where \eqn{\Gamma(.)}{T(.)} is
#' the \code{\link{gamma}} function.
#'
#' @param n a single \code{numeric} to indicate the lag length (cf., \emph{n}).
#' @param a a \code{numeric} as the first parameter (cf., \emph{a}).
#' @param b a \code{numeric} as the second parameter (cf., \emph{b}).
#'
#' @return A \code{data.frame} of beta weighting curves per combination of \code{a} and \code{b}. If \code{n = 1},
#' all weights are set to 1.
#'
#' @seealso \code{\link{ctr_agg}}
#'
#' @references Ghysels, Sinko and Valkanov (2007). ``MIDAS regressions: Further results and new directions''.
#' \emph{Econometric Reviews 26, 53-90}, \url{https://doi.org/10.1080/07474930600972467}.
#'
#' @export
weights_beta <- function(n, a = 1:4, b = 1:4) {
  if (any(c(a, b) <= 0))
    stop("Values in 'a' and 'b' should be positive.")
  vals <- (1:n) / n
  betas <- data.frame(matrix(nrow = n, ncol = length(a) * length(b)))
  colnames(betas) <- paste0("beta", paste0(rep(a, rep(length(b), length(a))), b))
  if (n == 1) betas[, ] <- 1
  else {
    k <- 1
    for (i in 1:length(a)) {
      for(j in 1:length(b)) {
        aa <- a[i]
        bb <- b[j]
        beta <- (vals^(aa - 1) * (1 - vals)^(bb - 1) * gamma(aa + bb)) / (gamma(aa) * gamma(bb))
        betas[, k] <- beta/sum(beta)
        k <- k + 1
      }
    }
  }
  return(as.data.frame(betas))
}

setup_time_weights <- function(lag, how, ...) {
  dots <- tryCatch(list(...)[[1]], # extract list from list of list (if ... is a list)
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
    weights <- cbind(weights, weights_exponential(lag, dots$alphasExp))
  }
  if ("almon" %in% how) {
    weights <- cbind(weights, weights_almon(lag, dots$ordersAlm, dots$do.inverseAlm, TRUE)) # always normalize
  }
  if ("beta" %in% how) {
    weights <- cbind(weights, weights_beta(lag, dots$aBeta, dots$bBeta))
  }
  if ("own" %in% how) {
    weights <- cbind(weights, dots$weights)
  }
  return(weights)
}

#' Options supported to perform aggregation into sentiment measures
#'
#' @description Outputs the supported aggregation arguments. Call for information purposes only. Used within
#' \code{\link{ctr_agg}} to check if supplied aggregation hows are supported.
#'
#' @details See the package's \href{https://ssrn.com/abstract=3067734}{vignette} for a detailed explanation of all
#' aggregation options.
#'
#' @return A list with the supported aggregation hows for arguments \code{howWithin} (\code{"words"}), \code{howDows}
#' (\code{"docs"}) and \code{howTime} (\code{"time"}), to be supplied to \code{\link{ctr_agg}}.
#'
#' @seealso \code{\link{ctr_agg}}
#'
#' @export
get_hows <- function() {
  words <- c("proportional", "proportionalPol", "counts")
  docs <- c("equal_weight", "proportional")
  time <- c("equal_weight", "almon", "beta", "linear", "exponential", "own")
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

align_variables <- function(y, sentomeasures, x, h, difference, i = 1, nSample = NULL) {

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

  datesX <- get_dates(sentomeasures)
  sent <- get_measures(sentomeasures)[, -1] # drop dates
  if (is.null(x)) x <- sent
  else x <- cbind(sent, x)
  x <- as.matrix(x)
  x[is.na(x)] <- 0 # check

  if (h > 0) {
    if (difference)
      y <- diff(y, lag = h)
    else
      y <- y[(h + 1):nrow(x), , drop = FALSE]
    x <- x[1:nrow(y), , drop = FALSE]
    datesX <- datesX[1:nrow(y)]
  } else if (h < 0) {
    x <- x[(abs(h) + 1):nrow(y), , drop = FALSE]
    datesX <- datesX[(abs(h) + 1):nrow(y)]
    if (difference)
      y <- diff(y, lag = abs(h))
    else
      y <- y[1:nrow(x), , drop = FALSE]
  }
  if (!is.null(nSample)) {
    x <- x[i:(nSample + i - 1), , drop = FALSE]
    datesX <- datesX[i:(nSample + i - 1)]
    y <- y[i:(nSample + i - 1), , drop = FALSE]
  }

  return(list(y = y, x = x, datesX = datesX))
}

clean_panel <- function(x, nx, threshold = 0.50) {

  # discards columns from panel of explanatory variables based on a few simple rules
  # useful to simplify (to some extent) the penalized variables regression (cf. 'exclude')

  if (nx != 0) { # do not perform cleaning on last nx variables
    start <- ncol(x) - nx + 1
    end <- ncol(x)
    xx <- x[, -(start:end)] # drop non-sentiment variables if no shrinkage imposed on these variables
  } else xx <- x
  xx[is.na(xx)] <- 0 # check
  duplics <- duplicated(as.matrix(xx), MARGIN = 2) # duplicated columns
  manyZeros <- (colSums(as.matrix(xx) == 0, na.rm = TRUE) / nrow(xx)) > threshold # columns with too many zeros
  toDiscard <- duplics | manyZeros
  if (ncol(xx) == 1) stop("No explanatory variables retained after cleaning: too many duplicated columns and/or zeros.")
  else {
    if (nx != 0) {
      xNew <- cbind(xx[, !toDiscard], x[, start:end])
      colnames(xNew) <- c(names(which(!toDiscard)), colnames(x[, start:end, drop = FALSE]))
    }
    else xNew <- xx[, !toDiscard]
  }

  return(list(xNew = xNew, discarded = toDiscard))
}

update_info <- function(sentomeasures, newMeasures, ...) {
  check_class(sentomeasures, "sentomeasures")
  n <- ncol(newMeasures)
  newNames <- stringi::stri_split(colnames(newMeasures), regex = "--")[-1] # drop first element (date column)
  sentomeasures$measures <- newMeasures
  sentomeasures$lexicons <- unique(sapply(newNames, "[", 1))
  sentomeasures$features <- unique(sapply(newNames, "[", 2))
  sentomeasures$time <- unique(sapply(newNames, "[", 3))
  sentomeasures$stats <- compute_stats(sentomeasures) # measures in sentomeasures are already updated by here
  sentomeasures$attribWeights <- update_attribweights(sentomeasures, ...)
  return(sentomeasures)
}

update_attribweights <- function(sentomeasures, ...) {
  attribWeights <- sentomeasures$attribWeights
  dims <- get_dimensions(sentomeasures)
  dots <- list(...)
  toMerge <- dots$merges

  B <- attribWeights[["B"]]
  W <- attribWeights[["W"]]

  lexFeats <- unique(
    sapply(stringi::stri_split(colnames(get_measures(sentomeasures))[-1], regex = "--"),
           function(x) paste0(x[1:2], collapse = "--"))
  )

  if (!is.null(toMerge)) {
    for (t in seq_along(toMerge[["time"]])) {
      tt <- toMerge[["time"]][t]
      B[, names(tt)] <- rowMeans(B[, unlist(tt)])
    }
    lexs <- unique(sapply(stringi::stri_split(colnames(W)[-c(1:2)], regex = "--"), "[", 1))
    for (f in seq_along(toMerge[["features"]])) {
      ff <- toMerge[["features"]][f]
      for (l in lexs) {
        cols <- paste0(l, "--", unlist(ff))
        WW <- W[, c("date", cols), with = FALSE]
        WW[is.na(WW)] <- 0
        W[, paste0(l, "--", names(ff))] <- WW[, list(rowMeans(.SD)/sum(rowMeans(.SD))), by = date][[2]]
      }
    }
    feats <- unique(sapply(stringi::stri_split(colnames(W)[-c(1:2)], regex = "--"), "[", 2)) # updated columns
    for (l in seq_along(toMerge[["lexicons"]])) {
      ll <- toMerge[["lexicons"]][l]
      for (f in feats) {
        cols <- paste0(unlist(ll), "--", f)
        WW <- W[, c("date", cols), with = FALSE]
        WW[is.na(WW)] <- 0
        W[, paste0(names(ll), "--", f)] <- WW[, list(rowMeans(.SD)/sum(rowMeans(.SD))), by = date][[2]]
      }
    }
  }

  newB <- B[, sentomeasures$time, drop = FALSE]
  newW <- W[, c("id", "date", lexFeats), with = FALSE]
  for (col in lexFeats) set(newW, which(is.nan(newW[[col]])), col, NA) # convert NaN to NA

  list(W = newW, B = newB)
}

check_class <- function(x, class) {
  if (!inherits(x, class))
    stop("Please provide a ", class, " object as one of the arguments.")
}

compute_stats <- function(sentomeasures) {
  measures <- get_measures(sentomeasures)[, -1] # drop dates
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

compute_BIC <- function(y, dfA, RSS, sigma2) { # BIC-like criterion
  BIC <- RSS/(nrow(y) * sigma2) + (log(nrow(y))/nrow(y)) * dfA
  return(BIC)
}

compute_AIC <- function(y, dfA, RSS, sigma2) { # AIC-like criterion
  AIC <- RSS/(nrow(y) * sigma2) + (2/nrow(y)) * dfA
  return(AIC)
}

compute_Cp <- function(y, dfA, RSS, sigma2) { # Mallows's Cp-like criterion
  Cp <- RSS/nrow(y) + (2/nrow(y)) * dfA * sigma2
  return(Cp)
}

measures_to_long <- function(measures) { # changes format of sentiment measures data.table from wide to long
  dates <- measures[["date"]]
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
  long[, "date" := rep(dates, rep(length(names), length(dates)))]
  setcolorder(long, c("date", "value", "lexicons", "features", "time"))[]
  return(long)
}

is_names_correct <- function(x) !any(stringi::stri_detect(x, regex = "-"))

check_nCore <- function(nCore) {
  # if (any(nCore < 1)) {
  #   nCore[which(nCore < 1)] <- 1
  #   warning("Nonpositive elements in the 'nCore' argument are set to 1.", call. = FALSE)
  # }
  if (length(nCore) == 1 && is.numeric(nCore) && nCore < 1) {
    nCore <- 1
    warning("The 'nCore' argument is set to 1.")
  }
  nCore
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
  x
}

convert_date <- function(date, by = "day") {
  if (by == "year") {
    year <- stringi::stri_split(date, regex = "-")[[1]][1]
    date <- as.Date(paste0(year, "-01-01"), format = "%Y-%m-%d")
  } else if (by == "month") {
    month <- paste0(stringi::stri_split(date, regex = "-")[[1]][1:2], collapse = "-")
    date <- as.Date(paste0(month, "-01"), format = "%Y-%m-%d")
  } else if (by == "week") {
    week <- ISOweek::ISOweek(date)
    date <- ISOweek::ISOweek2date(paste(week, 1, sep = "-")) # get first day of week based on ISO standard
  } else { # day
    date <- as.Date(date, format = "%Y-%m-%d")
  }
  return(date)
}

get_names_lags <- function(nLags) {
  k <- nchar(nLags)
  paste0("lag_",
         sapply((nLags - 1):0, function(n) ifelse(nchar(n) == k,
                                                  as.character(n),
                                                  paste0(c(rep("0", k - nchar(n)), n), collapse = ""))))
}

check_sentiment_format <- function(sentiment) {
  errMessage <- "The 'sentiment' input is not appropriate. It should be computed from a 'sentocorpus' object."
  if (!is.list(sentiment)) {
    stop(errMessage)
  } else if (!all(names(sentiment) %in% c("sentiment", "features", "lexicons", "howWithin"))) {
      stop(errMessage)
  } else {
    s <- sentiment[["sentiment"]]
    if (!all(colnames(s)[1:3] %in% c("id", "date", "word_count")))
      stop(errMessage)
    else if (ncol(s) != 3 + length(sentiment$features) * length(sentiment$lexicons))
      stop(errMessage)
    else if (!inherits(s[["id"]], "character"))
      stop(errMessage)
    else if (!inherits(s[["date"]], "Date"))
      stop(errMessage)
    else if (!all(sapply(3:ncol(s), function(i) inherits(s[[i]], "numeric"))))
      stop(errMessage)
  }
}

plot_theme <- function(legendPos) { # plotting specifications (border and grid)
  theme(
    legend.title = element_blank(),
    legend.position = legendPos,
    panel.background = element_rect(colour = "black", size = 0.35),
    panel.grid.major = element_line(colour = "grey95", size = 0.10),
    panel.grid.minor = element_line(colour = "grey95", size = 0.10)
  )
}

