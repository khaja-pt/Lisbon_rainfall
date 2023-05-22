# Load the necessary packages
library(forecast)
library(readr)

# Load the data
data <- read_csv("C:\\Users\\Zaffer Khaja\\Downloads\\Rainfall_data.csv")

# Convert to time series object
ts_data <- ts(data$Precipitation, start = c(2000, 1), end = c(2021, 12), frequency = 12)

# Divide the data into training and testing sets
# I'm assuming the last year (2021) is reserved for testing
train_data <- window(ts_data, end=c(2020, 12))
test_data <- window(ts_data, start=c(2021, 1))

# Fit the SARIMA model to the training data
model <- auto.arima(train_data)

# Print the model's details
print(model)

# Use the model to make forecasts
forecasts <- forecast(model, h=length(test_data))

# Plot the forecasts
autoplot(forecasts) + 
  autolayer(test_data, series="Actual", PI=FALSE) +
  xlab("Year") + 
  ylab("Rainfall (mm)") +
  ggtitle("Rainfall forecast using auto.arima")

