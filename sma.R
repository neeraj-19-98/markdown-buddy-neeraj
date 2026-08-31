sma <- function(data, period) {
  result <- rep(NA, length(data))
  
  if (period <= 0 || period > length(data)) {
    return(result)
  }
  
  for (i in period:length(data)) {
    result[i] <- mean(data[(i - period + 1):i])
  }
  
  return(result)
}
