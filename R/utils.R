
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

# to export
get_hows <- function() {

  words <- c("equal-weight", "proportional", "tf-idf")
  sentences <- c("equal-weight", "proportional")
  docs <- c("equal-weight", "proportional")
  time <- c("equal-weight", "almon", "linear", "exponential", "own")

  hows <- list(words = words,
               sentences = sentences,
               docs = docs,
               time = time)

  return(hows)
}

