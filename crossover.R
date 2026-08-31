crossover <- function(x, y) {
  result <- rep(FALSE, length(x))
  
  if (length(x) != length(y)) {
    return(result)
  }
  
  for (i in 2:length(x)) {
    if (!is.na(x[i]) && !is.na(y[i]) &&
        !is.na(x[i - 1]) && !is.na(y[i - 1])) {
      
      if (x[i] > y[i] && x[i - 1] <= y[i - 1]) {
        result[i] <- TRUE
      }
    }
  }
  
  return(result)
}
