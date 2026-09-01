library(shiny)
library(ggplot2)
library(quantmod)
# Stock data settings
stock_symbol <- "AAPL"
start_date <- "2023-01-01"
end_date <- "2023-07-01"

# Fetch stock data from Yahoo Finance
stock_data <- getSymbols(
  stock_symbol,
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

# View the first few rows
head(stock_data)
# Convert stock data to a data frame
stock_df <- data.frame(
  Date = index(stock_data),
  Close = as.numeric(Cl(stock_data))
)

# Check the data
head(stock_df)
# Create stock price chart
ggplot(stock_df, aes(x = Date, y = Close)) +
  geom_line() +
  labs(
    title = "AAPL Stock Price",
    x = "Date",
    y = "Closing Price"
  ) +
  theme_minimal()
# Calculate 20-day Moving Average
stock_df$MA20 <- as.numeric(
  stats::filter(stock_df$Close, rep(1/20, 20), sides = 1)
)


# Check the Moving Average
head(stock_df, 25)
# Plot closing price with 20-day Moving Average
ggplot(stock_df, aes(x = Date)) +
  geom_line(aes(y = Close), linewidth = 0.7) +
  geom_line(aes(y = MA20), linewidth = 1) +
  labs(
    title = "AAPL Stock Price with 20-Day Moving Average",
    x = "Date",
    y = "Price"
  ) +
  theme_minimal()
# Calculate RSI
delta <- c(NA, diff(stock_df$Close))

gain <- ifelse(delta > 0, delta, 0)
loss <- ifelse(delta < 0, -delta, 0)

avg_gain <- stats::filter(gain, rep(1/14, 14), sides = 1)
avg_loss <- stats::filter(loss, rep(1/14, 14), sides = 1)

RS <- avg_gain / avg_loss

stock_df$RSI <- 100 - (100 / (1 + RS))

# Check RSI
tail(stock_df, 10)
# Calculate MACD
ema12 <- stats::filter(stock_df$Close, 2/13, sides = 1)
ema26 <- stats::filter(stock_df$Close, 2/27, sides = 1)

stock_df$MACD <- as.numeric(ema12 - ema26)

# Check MACD
tail(stock_df, 10)
# Plot MACD
ggplot(stock_df, aes(x = Date, y = MACD)) +
  geom_line() +
  labs(
    title = "AAPL MACD",
    x = "Date",
    y = "MACD"
  ) +
  theme_minimal()
# Generate Trading Signals
stock_df$Signal <- "HOLD"

stock_df$Signal[stock_df$RSI < 30] <- "BUY"
stock_df$Signal[stock_df$RSI > 70] <- "SELL"

# Check signals
tail(stock_df, 15)
# Plot Trading Signals
ggplot(stock_df, aes(x = Date, y = Close)) +
  geom_line() +
  geom_point(
    aes(shape = Signal),
    size = 2
  ) +
  labs(
    title = "AAPL Trading Signals",
    x = "Date",
    y = "Closing Price",
    shape = "Signal"
  ) +
  theme_minimal()
# Generate Trading Signals
stock_df$Signal <- "HOLD"

stock_df$Signal[stock_df$RSI < 30] <- "BUY"
stock_df$Signal[stock_df$RSI > 70] <- "SELL"

# Check signals
tail(stock_df, 15)
ggplot(stock_df, aes(x = Date, y = Close)) +
  geom_line() +
  geom_point(aes(shape = Signal), size = 2) +
  labs(
    title = "AAPL Trading Signals",
    x = "Date",
    y = "Closing Price",
    shape = "Signal"
  ) +
  theme_minimal()
# Shiny Dashboard

ui <- fluidPage(
  titlePanel("AAPL Portfolio Dashboard"),
  
  dateRangeInput(
    "date_range",
    "Select Date Range:",
    start = "2023-01-01",
    end = "2023-07-01"
  ),
  
  selectInput(
    "time_frame",
    "Select Time Frame:",
    choices = c("Daily", "Weekly", "Monthly")
  ),
  
  selectInput(
    "indicator",
    "Select Indicator:",
    choices = c("None", "Moving Average", "RSI", "MACD")
  ),
  
  plotOutput("stock_chart")
)

server <- function(input, output) {
  
  output$stock_chart <- renderPlot({
    
    filtered_data <- stock_df[
      stock_df$Date >= input$date_range[1] &
        stock_df$Date <= input$date_range[2],
    ]
    
    p <- ggplot(filtered_data, aes(x = Date, y = Close)) +
      geom_line() +
      labs(
        title = "AAPL Stock Price",
        x = "Date",
        y = "Closing Price"
      ) +
      theme_minimal()
    
    if (input$indicator == "Moving Average") {
      p <- p + geom_line(aes(y = MA20))
    }
    if (input$indicator == "RSI") {
      p <- ggplot(filtered_data, aes(x = Date, y = RSI)) +
        geom_line() +
        labs(
          title = "AAPL RSI",
          x = "Date",
          y = "RSI"
        ) +
        theme_minimal()
    }
    
    if (input$indicator == "MACD") {
      p <- ggplot(filtered_data, aes(x = Date, y = MACD)) +
        geom_line() +
        labs(
          title = "AAPL MACD",
          x = "Date",
          y = "MACD"
        ) +
        theme_minimal()
    }
    print(p)
  })
}

shinyApp(ui, server)