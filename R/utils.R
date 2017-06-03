
############################################################
################# Utility/Helper functions #################
############################################################

almons <- function(n, orders = 1:3, inverse = FALSE, normalize = TRUE, ...) {
  
  vals <- 1:n
  inv <- ifelse(inverse, 2, 1)
  almons <- data.frame(matrix(nrow = n, ncol = length(orders) * inv))
  colnames(almons) <- paste0("almon", rep(orders, rep(inv, length(orders))), c("", "_inv")[1:inv])
  
  for (i in 1:length(orders)) {
    b <- orders[i]
    
    stdindex = vals/max(vals)
    if (inverse) {
      stdindex <- cbind(stdindex, - stdindex + 1)
      ind <- (i*2-1):(i*2)
    } else {
      ind <- (ifelse(i == 1, 2, i)-1):i
    }
    
    almon <- (1 - (stdindex)^b) * stdindex^(max(orders) - b)
    almons[, ind] <- almon
  }
  
  if (normalize) almons <- t(t(almons)/colSums(almons)) # normalize weights so that it sums to 1
  
  return(almons)
  
}

roll_weights <- function(x, w) {
  
  if (all(is.na(x))) return(NA)
  else {
    return(sum(x[!is.na(x)] * w[!is.na(x)])) # x is sentiment index column, Z is weights function (which sums to 1)
  }
  
}

