rsi <- function(data, period) {
  result <- rep(NA, length(data))
  
  if (period <= 0 || period >= length(data)) {
    return(result)
  }
  
  changes <- diff(data)
  
  gains <- ifelse(changes > 0, changes, 0)
  losses <- ifelse(changes < 0, abs(changes), 0)
  
  for (i in period:length(changes)) {
    avg_gain <- mean(gains[(i - period + 1):i])
    avg_loss <- mean(losses[(i - period + 1):i])
    
    if (avg_loss == 0) {
      result[i + 1] <- 100
    } else {
      rs <- avg_gain / avg_loss
      result[i + 1] <- 100 - (100 / (1 + rs))
    }
  }
  
  return(result)
}
