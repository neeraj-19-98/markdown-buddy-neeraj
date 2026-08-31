stoch_rsi <- function(data, period) {
  rsi_values <- rsi(data, period)
  result <- rep(NA, length(data))
  
  if (period <= 0 || period > length(data)) {
    return(result)
  }
  
  for (i in period:length(data)) {
    window <- rsi_values[(i - period + 1):i]
    
    if (all(is.na(window))) {
      next
    }
    
    lowest <- min(window, na.rm = TRUE)
    highest <- max(window, na.rm = TRUE)
    
    if (highest == lowest) {
      result[i] <- 0
    } else {
      result[i] <- (rsi_values[i] - lowest) / (highest - lowest)
    }
  }
  
  return(result)
}
