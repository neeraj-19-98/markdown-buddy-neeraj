linreg <- function(data, period) {
  result <- rep(NA, length(data))
  
  if (period <= 0 || period > length(data)) {
    return(result)
  }
  
  for (i in period:length(data)) {
    y <- data[(i - period + 1):i]
    x <- 1:period
    
    x_mean <- mean(x)
    y_mean <- mean(y)
    
    numerator <- sum((x - x_mean) * (y - y_mean))
    denominator <- sum((x - x_mean)^2)
    
    slope <- numerator / denominator
    intercept <- y_mean - slope * x_mean
    
    result[i] <- intercept + slope * period
  }
  
  return(result)
}
