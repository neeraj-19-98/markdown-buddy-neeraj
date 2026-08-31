# Load the functions
source("sma.R")
source("ema.R")
source("macd.R")
source("stdev.R")
source("linreg.R")
source("rsi.R")
source("stoch_rsi.R")
source("crossover.R")
source("crossunder.R")

# Sample data
data <- c(10, 12, 11, 14, 16, 15, 18, 20, 19, 22)

# Test SMA
print(sma(data, 3))

# Test EMA
print(ema(data, 3))

# Test MACD
print(macd(data, 3, 5, 2))

# Test Standard Deviation
print(stdev(data, 3))

# Test Linear Regression
print(linreg(data, 3))

# Test RSI
print(rsi(data, 3))

# Test StochRSI
print(stoch_rsi(data, 3))

# Test Crossover
x <- c(1, 2, 3, 5, 6)
y <- c(2, 2, 2, 4, 7)
print(crossover(x, y))

# Test Crossunder
x <- c(5, 4, 3, 2, 1)
y <- c(2, 3, 3, 3, 2)
print(crossunder(x, y))
