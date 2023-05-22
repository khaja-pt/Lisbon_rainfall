# Load the required libraries
library(fpp2)

# Load the time series data
data <- read.csv("C:\\Users\\Zaffer Khaja\\Downloads\\Rainfall_data.csv")

ts_data <- ts(data$Precipitation, start = c(2000, 1), end = c(2021, 12), frequency = 12)

autoplot(ts_data) +
  ggtitle("Rainfall Data from 2000 to 2021 for Lisbon, Portugal") +
  xlab("Year") +
  ylab("Precipitation")

# Determine the order of differencing d required to make the time series stationary
d <- ndiffs(ts_data)

# Perform the KPSS test
kpss.test(ts_data)

# Seasonal plot
ggseasonplot(ts_data, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Millimetres") +
  ggtitle("Seasonal plot: Rainfall in Lisbon")

#Seasonal subseries plot
ggsubseriesplot(ts_data) +
  ylab("Millimetres") +
  ggtitle("Seasonal subseries plot: Lisbon Rainfall")

#ACF & PACF plot
tsdisplay(ts_data,lag.max = 48) 

#Lag plot
df<- window(ts_data, start=2000)
gglagplot(df)

#ACF plot
ggAcf(df, lag=48)

#Classical Decomposition plot
ts_data %>% decompose(type="additive") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical additive decomposition of Lisbon Rainfall data")

#Simple exponential smoothing
fc <- ses(ts_data, h=12)
autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("Rainfall (mm)") + xlab("Year")
checkresiduals(fc)

#Holt's method
fc1 <- holt(ts_data, h=12)
fc2 <- holt(ts_data, damped=TRUE, phi = 0.9, h=12)
autoplot(ts_data) +
  autolayer(fc1, series="Holt's method", PI=FALSE) +
  autolayer(fc2, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Rainfall (mm)")
checkresiduals(fc1)

# Holt-Winters’ seasonal method
fit_hw <- hw(ts_data,seasonal="additive")
autoplot(ts_data) +
  autolayer(fit_hw, series="HW additive forecasts", PI=FALSE) +
  xlab("Year") +
  ylab("Rainfall(mm)") +
  ggtitle("Rainfall data for Lisbon, Portugal")
checkresiduals(fit_hw)

#State space models ETS()
train_data <- window(ts_data, end = c(2020, 12))
test_data <- window(ts_data, start = c(2021, 1))

fit_ets <- ets(train_data, model = "ANA", alpha = NULL, beta =NULL, gamma = NULL)
forecasts <- forecast(fit_ets, h = length(test_data))

plot(forecasts, main = "Exponential Smoothing Forecast for Rainfall", ylab = "Rainfall (mm)")
lines(test_data, col = "red")
checkresiduals(fit_ets)

#SARIMA
fit_s <- auto.arima(train_data, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
forecasts <- forecast(fit_s, h = length(test_data))

plot(forecasts, main = "SARIMA Forecast for Rainfall", ylab = "Rainfall (mm)")
lines(test_data, col = "red")
checkresiduals(fit_s)

# Fit a linear regression model with a seasonal component
lm_model <- tslm(ts_data ~ trend + season)

# Forecast the rainfall data for the next 12 months
forecast <- forecast(lm_model, h = 12)

autoplot(forecast) +
  ggtitle("Forecasts of Rainfall using regression") +
  xlab("Year") + ylab("rainfall(mm)")
checkresiduals(lm_model)


forecast

########################################












############## miscellaneous ############### 


# Fit ETS model to the training data
fit_ets <- ets(train_data)

# Use the model to forecast the future values
forecasts <- forecast(fit_ets, h = length(test_data))

# Compute accuracy measures for the ETS forecast
accuracy_metrics <- accuracy(forecasts, test_data)

# Print accuracy metrics
print(accuracy_metrics)


# Making predictions on the test dataset
forecasts <- forecast(fit, h = length(test_data))

# Extracting point forecasts
point_forecasts <- forecasts$mean

# Computing the RMSE
rmse <- sqrt(mean((test_data - point_forecasts)^2))









# Initialize vectors to store order parameters as strings
non_seasonality_order_values <- c()
seasonal_order_values <- c()

# Iterate over parameters
for(i in 1:length(parameters)) {
  # Fit model
  fit <- Arima(train_data, order=parameters[[i]]$order, seasonal=parameters[[i]]$seasonal)
  
  # Forecast
  forecasts <- forecast(fit, h=length(test_data))
  
  # Compute RMSE
  rmse <- sqrt(mean((test_data - forecasts$mean)^2))
  
  # Compute AIC
  aic <- AIC(fit)
  
  # Store RMSE and AIC
  rmse_values <- c(rmse_values, rmse)
  aic_values <- c(aic_values, aic)
  
  # Store order parameters as strings
  non_seasonality_order_values <- c(non_seasonality_order_values, paste(parameters[[i]]$order, collapse=","))
  seasonal_order_values <- c(seasonal_order_values, paste(parameters[[i]]$seasonal, collapse=","))
}

# Fit auto.arima model
fit_auto <- auto.arima(train_data)

# Forecast
forecasts_auto <- forecast(fit_auto, h=length(test_data))

# Compute RMSE for auto.arima
rmse_auto <- sqrt(mean((test_data - forecasts_auto$mean)^2))

# Compute AIC for auto.arima
aic_auto <- AIC(fit_auto)

# Append auto.arima results to vectors
rmse_values <- c(rmse_values, rmse_auto)
aic_values <- c(aic_values, aic_auto)
non_seasonality_order_values <- c(non_seasonality_order_values, paste(fit_auto$arma[1:3], collapse=","))
seasonal_order_values <- c(seasonal_order_values, paste(fit_auto$arma[4:7], collapse=","))

# Create a data frame to store the results
results <- data.frame(
  Model = c("sarima1", "sarima2", "sarima3", "sarima4", "sarima5", "sarima6", "auto.arima"),
  Non_seasonality_order = non_seasonality_order_values,
  Seasonal_order = seasonal_order_values,
  RMSE = rmse_values,
  AIC = aic_values
)

# Print the results
print(results)


# Fit a linear regression model
fit_linear <- lm()

# Fit a Holt model
fit_holt <- Holt()

# Fit a Holt-Winter's model
fit_holt_winters <- HoltWinters()

# Fit a Simple Exponential Smoothing model
fit_simple_exp <- HoltWinters(beta=FALSE, gamma=FALSE)

# Fit a SARIMA model
fit_sarima <- Arima(order=(), seasonal=())

# Fit an ETS model
fit_ets <- ets()

# List of models
models <- list(fit_linear, fit_holt, fit_holt_winters, fit_simple_exp, fit_sarima, fit_ets)

# For each model, forecast, calculate residuals, and RMSE
for (model in models) {
  forecast <- forecast(model)
  residuals <- test_data - forecast
  rmse <- sqrt(mean(residuals^2))
  print(rmse)
}







# Fit a linear regression model
fit_linear <- lm()

# Fit a Holt model
fit_holt <- Holt()

# Fit a Holt-Winter's model
fit_holt_winters <- HoltWinters()

# Fit a Simple Exponential Smoothing model
fit_simple_exp <- HoltWinters(beta=FALSE, gamma=FALSE)

# Fit a SARIMA model
fit_sarima <- Arima(order=(), seasonal=())

# Fit an ETS model
fit_ets <- ets()

# List of models
models <- list(fit_linear, fit_holt, fit_holt_winters, fit_simple_exp, fit_sarima, fit_ets)

# For each model, forecast, calculate residuals, and RMSE
for (model in models) {
  forecast <- forecast(model)
  residuals <- test_data - forecast
  rmse <- sqrt(mean(residuals^2))
  print(rmse)
}




