# R Data Analysis Script

# Create a simple synthetic dataset
data <- data.frame(
  Category = c("A", "B", "C"),
  Value = c(10, 20, 15)
)

# Display the dataset
print(data)

# Calculate the average value
average_value <- mean(data$Value)

# Display the average value
print(average_value)
