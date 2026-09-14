library(wavethresh)
library(wavelets)
library(forecast)
library(readxl)
library(zoo)
library(tseries)

# Load the data from the Excel file
data_input <- read_excel("data 120.xlsx")
x <- data_input$`Data Inflasi`

# Step 1: Test for stationarity using ADF test
result <- adf.test(x)
cat("ADF test statistic: ", result$statistic, "\n")
cat("p-value: ", result$p.value, "\n")
if (result$p.value < 0.05) {
  cat("The time series is stationary.\n")
} else {
  cat("The time series is non-stationary.\n")
}
#Transform the data
log_x<-log(x)
result_trans <- adf.test(log_x)
cat("ADF test statistic: ", result_trans$statistic, "\n")
cat("p-value: ", result_trans$p.value, "\n")
if (result_trans$p.value < 0.05) {
  cat("The time series is stationary.\n")
} else {
  cat("The time series is non-stationary.\n")
}

# Step 2: Identify the model using ACF and PACF test
ts_data <- ts(log_x)
par(mfrow=c(1,2))
ggAcf(ts_data, main = "ACF and PACF Plots")
ggPacf(ts_data, main = "PACF Plot")

# Step 3: Estimate the model parameters using MODWT-ARIMA
filters<-"d4"
scale<-5
modwt.decomp <- modwt(log_x, filters, n.levels = scale)
wavelet.coeff <- modwt.decomp@W[4]
arima.model <- arima(data.frame(wavelet.coeff))


# Step 4: Perform diagnostic testing
residuals <- residuals(arima.model)
checkresiduals(na.omit(residuals))
Box.test(residuals, lag = 20, type = "Ljung-Box")

# Step 5: Determine the best ARIMA model
best.model <- arima(log_x, order = arima.model$arma[1:3], method = "ML")

# Step 6: Perform forecasting
forecast <- forecast(best.model, h = 10)

# Step 7: Calculate the RMSE and MAPE
actual <- data_input$`Data Inflasi`[(length(log_x)-9):length(log_x)]
rmse <- sqrt(mean((forecast$mean - actual)^2))
mape <- mean(abs(forecast$mean - actual)/actual)*100

# Print the results
cat(sprintf("Best ARIMA Model: %s\n", paste(arima.model$arma, collapse = ",")))
cat(sprintf("Forecasted Values: %s\n", paste(round(forecast$mean, 2), collapse = ",")))
cat(sprintf("RMSE: %s\n", round(rmse, 2)))
cat(sprintf("MAPE: %s%%\n", round(mape, 2)))

