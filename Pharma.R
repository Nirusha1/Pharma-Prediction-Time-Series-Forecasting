#Final Project Pharma Sales

# Reading Data
Pharma.data <- read.csv("salesdaily.csv")
head(Pharma.data)
dim(Pharma.data)

# Load necessary libraries
library(dplyr)

# Convert the 'datum' column to Date format
Pharma.data$datum <- as.Date(Pharma.data$datum, format = "%m/%d/%Y")

# Group by Year and Month, then summarize each drug category with sum (or average if desired)
monthly_data <- Pharma.data %>%
  mutate(YearMonth = format(datum, "%b-%y")) %>%  # Create 'YearMonth' column in "MMM-YY" format
  group_by(YearMonth) %>%
  summarise(
    M01AB = sum(M01AB, na.rm = TRUE),
    M01AE = sum(M01AE, na.rm = TRUE),
    N02BA = sum(N02BA, na.rm = TRUE),
    N02BE = sum(N02BE, na.rm = TRUE),
    N05B = sum(N05B, na.rm = TRUE),
    N05C = sum(N05C, na.rm = TRUE),
    R03 = sum(R03, na.rm = TRUE),
    R06 = sum(R06, na.rm = TRUE)
  ) %>%
  # Convert 'YearMonth' to a date format for chronological ordering
  mutate(Date = as.Date(paste("01", YearMonth), format = "%d %b-%y")) %>%  # Add "01" to create a valid date
  arrange(Date) %>%                                                        # Order by Date
  select(-Date)                                                            # Remove Date column if no longer needed

# View the ordered data
head(monthly_data)

sum(is.na(monthly_data))


mo1ab <- monthly_data
mo1ab$M01AE<-NULL
mo1ab$N02BA<-NULL
mo1ab$N02BE<-NULL
mo1ab$N0B<-NULL
mo1ab$N05B<-NULL
mo1ab$N05C<-NULL
mo1ab$N0C<-NULL
mo1ab$R03<-NULL
mo1ab$R06<-NULL
tail(mo1ab)
#mo1ab = Anti-inflammatory and antirheumatic products, non-steriods, Acetic Acide Derivatices and related substances

#convert to ts object
pharma.ts <- ts(mo1ab$M01AB, start = c(2014,1), end = c(2019, 9), freq = 12) 
plot(pharma.ts, xlab = "Time", ylab = "Sales")


#no seasonality
#noise
#level
#Quadratic/polynomial

library(forecast)
seasonplot(pharma.ts)

#Training and Validation Data Set
pharma.train= window(pharma.ts, start=c(2014,1), end=c(2018,9))
pharma.valid=window(pharma.ts,start=c(2018,10))

#Naive Forecasting--------------------------------------------------------------------------------
pharma.naive = naive(pharma.train, h=12)


# Add forecasts for the validation period with a dotted line
plot(pharma.ts, xlab = "Time", ylab = "Sales", main="Naive Forecasting Model")
lines(pharma.naive$mean, lwd=2, lty=2, col="blue")
lines(c(2018.75, 2018.75), c(0,220))
lines(c(2019.70,2019.70), c(0,220))
text(2018.3, 200, "Training")
text(2019.3, 200, "Validation")


# Calculating Accuracy for Naive Forecasting
accuracy(pharma.naive$mean, pharma.valid)

#               ME     RMSE      MAE       MPE     MAPE        ACF1    Theil's U
#  Test set    7.9475 16.92275 14.0125 4.081447 8.43586 -0.2844996 0.7149448

# The RMSE for the validation period with naïve forecasts is 16.92.

#LinearRegression Forecasting--------------------------------------------------------------------------------
pharma.reg.linear = tslm(pharma.train ~  trend)
pharma.reg.linear.pred=forecast(pharma.reg.linear,h=24)
pharma.reg.linear.pred.2020=forecast(pharma.reg.linear.pred,h=12)


dev.off()
# Add forecasts for the validation period with a dotted line
plot(pharma.ts, xlab = "Time", ylab = "Sales")
lines(pharma.reg.linear.pred$mean, lwd=2, lty=2, col="blue")

lines(c(2018.75, 2018.75), c(0,220))
lines(c(2019.70,2019.70), c(0,220))
text(2018.3, 200, "Training")
text(2019.3, 200, "Validation")


# Calculating Accuracy for Naive Forecasting
accuracy(pharma.reg.linear.pred$mean, pharma.valid)

#               ME     RMSE      MAE       MPE     MAPE        ACF1    Theil's U
#  Test set    48.4062 53.53201 48.4062 29.07145 29.07145 0.320023   2.26218

# The RMSE for the validation period with quadratic regression forecasts is 53.53.

#Regression Forecasting--------------------------------------------------------------------------------
pharma.reg = tslm(pharma.train ~  trend + I(trend^2))
pharma.reg.pred=forecast(pharma.reg.linear,h=12)

dev.off()
# Add forecasts for the validation period with a dotted line
plot(pharma.ts, xlab = "Time", ylab = "Sales")
lines(pharma.reg.pred$mean, lwd=2, lty=2, col="blue")

lines(c(2018.75, 2018.75), c(0,220))
lines(c(2019.70,2019.70), c(0,220))
text(2018.3, 200, "Training")
text(2019.3, 200, "Validation")


# Calculating Accuracy for Naive Forecasting
accuracy(pharma.reg.pred$mean, pharma.valid)

#               ME     RMSE      MAE       MPE     MAPE        ACF1    Theil's U
#  Test set    -7.271503 16.10367 13.84415 -5.387333 9.048938 -0.3707133 0.6058128

# The RMSE for the validation period with quadratic regression forecasts is 16.10.

#ARIMA
#checking if the data is stationary

library(tseries)
adf.test(pharma.train)

# p-value > 0.05, thus the data is non-stationary

#First Differencing
pharma.diff = diff(pharma.train)
plot(pharma.diff, main="Differenced Data", xlab="Time", ylab="Sales")
adf.test(pharma.diff)

#ACF Plot
Pacf(pharma.reg$residuals)
acf(pharma.diff, main="ACF of differenced Data")
#q=1
Pacf(pharma.diff, main="Pacf of Differenced data")
#p=0

#Practical Use- ARIMA(p,d,q)
#Use ARIMA(1,0,0) when:
 # The series is stationary (confirmed via stationarity tests like ADF).
#The data shows a significant auto-correlation with one lag (check the ACF plot).


#Auto-Arima
pharma.arima = auto.arima(pharma.diff)
summary(pharma.arima)

#Forecasting the sales for the next 12 months
pharma.arima.forecast = forecast(pharma.arima, h=12)
plot(pharma.arima.forecast, xlab="Year")

#Undifferencing
last_value_train = tail(pharma.train,1)

#Re-integrating the forecasted values
pharma.forecast.undiff = cumsum(pharma.arima.forecast$mean)+as.numeric(last_value_train)

accuracy(pharma.forecast.undiff, pharma.valid)


#Arima(0,1,1)
pharma.arima.new=arima(pharma.diff, c(0,1,1))
summary(pharma.arima.new)
pharma.arima.new.forecast = forecast(pharma.arima.new, h=12)
plot(pharma.arima.new.forecast)
#Undifferencing
last_value_train = tail(pharma.train,1)

#Re-integrating the forecasted values
pharma.forecast.new.undiff = cumsum(pharma.arima.forecast$mean)+as.numeric(last_value_train)


accuracy(pharma.forecast.new.undiff, pharma.valid)
