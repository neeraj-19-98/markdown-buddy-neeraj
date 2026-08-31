ema <- function(data, period) {
  result <- rep(NA, length(data))
  
  if (period <= 0 || period > length(data)) {
    return(result)
  }
  
  result[period] <- mean(data[1:period])
  alpha <- 2 / (period + 1)
  
  for (i in (period + 1):length(data)) {
    result[i] <- alpha * data[i] + (1 - alpha) * result[i - 1]
  }
  
  return(result)
}
