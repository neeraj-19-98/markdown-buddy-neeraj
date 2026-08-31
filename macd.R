macd <- function(data, fast_period, slow_period, signal_period) {
  fast_ema <- ema(data, fast_period)
  slow_ema <- ema(data, slow_period)
  
  macd_line <- fast_ema - slow_ema
  signal_line <- ema(macd_line, signal_period)
  histogram <- macd_line - signal_line
  
  return(list(
    macd = macd_line,
    signal = signal_line,
    histogram = histogram
  ))
}
