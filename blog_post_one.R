# Bitcoin Project 1 Zcash Script #

# Blog post located at http://btc.bashingbitcoin.com/2018/07/zcash.html #

## Load libraries ##
library(ggplot2)
library(forecast)
library(tseries)
  # summary.arma
library(ARIMA)
require(ggplot2)
install.packages("Rcpp")

### Create basic plots comparing zcash price and its interest
# Closing Price over Time
plot1 <- ggplot(data=Zcash_data, aes(x=time, y=close)) + geom_line(color='magenta')
figure1 <- plot1 + xlab('Date') + ylab('Closing Price') + ggtitle('Zcash Price Over Time')

# Google Trend Data over Time
plot2 <- ggplot(data=Zcash_interest, aes(x=week, y=interest)) + geom_line(color='blue')
figure2 <- plot2 + xlab('Week') + ylab('Interest') + ggtitle('Zcash Google Trend Interst over Time')

### Create linear model
# Merge Zcash price data with the Zcash google interest data
total <- merge(Zcash_data,Zcash_interest,by="week")
total

# create a time variable 
t <- 1:(nrow(total))
t

# Fit the linear model
linmod <- lm((log(close)) ~ interest + t , data = total)
linmod
# Coefficients:
# (Intercept)     interest            t  
# 3.349909     0.031142     0.008173 
  # t is barely statistically significant though
summary(linmod)
# Larger adjusted r-squared and more statistically significant
# Good Rsquared (0.86)
# Very small standard errors (good)

# Plot fitMod
fVals <- fitted.values(linmod)
fVals

# Use zcash for arima
points(t, fVals, col = "red", type = "l") # ?

# Plot residuals to linear model
resids <- residuals(linmod)
plot3 <- plot(residuals(linmod), type = "b")
figure3 <- plot3 + title('Linear Model Residuals')


# Look at acf and pacf plots of the residuals of the linear model
  # don't know if this is necessary
figure4 <- acf(residuals) 
figure5 <- pacf(residuals)

## Trying to predict shitty linear model (doesn't work)
  ## DON'T THINK I NEED THIS BECAUSE THE MODEL FAILED EARLIER
par(mfrow=c(1,1))
pred = predict(ARIMAfit2, n.ahead = 8) # don't have ARIMAfit2
prediction = predict(fitMod, data = Zcash_data$close)
plot(prediction)


### ARIMA model of the difference in log(close)
zcashsmall <- Zcash_data[8:309,]
zcashsmall
  # to account for the dramatic drop in zcash price after the first week
figure6 <- acf(diff(log(zcashsmall$close)))
figure7 <- pacf(diff(log(zcashsmall$close)))

# First prove that the data in stationary (it is)
adf.test(diff(log(zcashsmall$close)), alternative = "stationary", k = 0)
# data:  diff(log(zcashsmall$close))
# Dickey-Fuller = -15.103, Lag order = 0, p-value = 0.01
# alternative hypothesis: stationary

# Looking at the ARIMA model
auto.arima(diff(log(zcashsmall$close)))
  # Suggests we should use arima(1,1,1), but go on below as arima(1,0,1)
smallarima <- arima(diff(log(zcashsmall$close)))
fitsmallarima <- Arima(diff(log(zcashsmall$close)), order=c(1,0,1))
summary(fitsmallarima)
  # gives ar = 0.3967 and ma = -0.02572
x <- ARMAacf(ar = c(0.3967), ma = c(-0.2572))
plot(x)
  # don't know why we need this plot
plot(forecast(fitsmallarima))
  # plots the forecasted predictions
predict <- predict(fitsmallarima, n.ahead=10)
predict
  # gives ten day predictions
autoplot(fitsmallarima)
  # roots outside the circle (because they are inverse)

# Trying to use ARIMA(1,1,1)
newfitsmallarima <- Arima(diff(log(zcashsmall$close)), order=c(1,1,1))
newsummary <- summary(newfitsmallarima)
  # new results
  # ar = 0.1098 and ma = -0.9715
newx <- ARMAacf(ar = c(0.1098), ma = c(-0.9715))
plot(newx)
newforecastplot <- plot(forecast(newfitsmallarima))
newautoplot <- autoplot(newfitsmallarima)
  # both points outside the circle, but just barely
newpredict <- predict(newfitsmallarima, n.ahead=10)
newpredict
# this matches with what happened in reality
  # very similar predictions

