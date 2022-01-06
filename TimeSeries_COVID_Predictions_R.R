install.packages("forecast") 
install.packages("fpp") 

library(forecast)
library(fpp)
library(dplyr)

Covid19cases.data <- read.csv("C:\\Users\\Siddharth\\Desktop\\Queens_Coursework\\867\\Assignment3\\owid-covid-data.csv") 


#Filtering data by Canada
Canada_cases = Covid19cases.data %>% filter(Covid19cases.data$iso_code == "CAN")

plot.ts(Canada_cases$new_cases)
plot.ts(Canada_cases$stringency_index)
plot.ts(Canada_cases$total_vaccinations)

# 1.	Use auto.arima on the original time series (i.e., without preprocessing) to make forecasting
CanadaNewcase <- Canada_cases$new_cases
model.raw <- auto.arima(CanadaNewcase, stepwise = FALSE, seasonal = TRUE)
model.raw
fit.raw <- Arima(CanadaNewcase, order=c(0,1,5))
autoplot(forecast(fit.raw,20))

# Preprocessing 

CanadaNewcase <- Canada_cases$new_cases
CanadaNewcase<-tail(CanadaNewcase,-100) # Remove the first 100 days when the case numbers are small

CanadaNewcase <- ts(CanadaNewcase, frequency=365, start=c(2020, 122))


plot.ts(CanadaNewcase, xlab="Date", ylab="US New cases")


#-----------(1) Stabilizing the Variance---------------

logCanNewcase<-log(CanadaNewcase) # We use log transform for simplicity 
plot.ts(logCanNewcase, xlab="Date", ylab="log Canada New cases")


#-----------(2) Remove Seasonality through Seasonal Differencing ---------------
# To check the period of cyclic pattern, use the autocorrelation function 
Acf(diff(logCanNewcase,1),lag.max =25) # We see spikes at p=7,14,21.. What does it suggest?  # Here we first perform regular differencing "diff(logUSNewcase,1)" to make the series more stationary, so its seasonality becomes easier to detect in the Acf plot

# We now remove the seasonality using seasonal differencing
logCanNewcase.deSeasonality <- diff(logCanNewcase,7) # period is 7 because of the weekly pattern 

plot.ts(logCanNewcase.deSeasonality, xlab="Date", ylab="log Canada New Case after removing trend and seasonality")
Acf(logCanNewcase.deSeasonality,lag.max =25) 


#-------------Check Stationarity -------------------
# Perform the augmented Dickey-Fuller (ADF) test to check stationarity. The null hypothesis assumes that the series is non-stationary.
adf.test(CanadaNewcase,alternative = "stationary") # What? the test suggests stationarity? Now try the following
adf.test(logCanNewcase.deSeasonality,alternative = "stationary")

#-------------Automatic ARIMA Modeling -------------------
# To begin, we use an automated algorithm to find a good model. However, there is no guarantee that it is the best model. So we treat it as a starting point. 
model.auto <- auto.arima( logCanNewcase.deSeasonality, stepwise=FALSE, seasonal= FALSE) #Fit using the Hyndman-Khandakar algorithm (Hyndman & Khandakar, 2008)
model.auto
# It suggests a ARIMA(4,0,1) model with zero mean
checkresiduals(model.auto)  # Check the quality of fit. Residuals should: 

# We can use the auto selected model to make forecasting 
fit.yourself <- Arima(logCanNewcase, order=c(1,0,9), seasonal=list(order=c(0,1,1),period=7)) # The seasonal differencing with period=7 is equivalent to "seasonal=list(order=c(0,1,0),period=7)"
fit.yourself
autoplot(forecast(fit.yourself,20) )

# Plot the forecasting in the original scale
fc<-forecast(fit.yourself,20)

fc$x <- exp(fc$x)
fc$mean <- exp(fc$mean)
fc$lower <- exp(fc$lower)
fc$upper <- exp(fc$upper)
autoplot(fc) 

write.csv(fc, file = "C:\\Users\\Siddharth\\Desktop\\Queens_Coursework\\867\\Assignment3\\prediction.csv", row.names = TRUE)

#-------------Improving the Automatically selected Model -------------------
fit.alternative1 <- Arima(logUSNewcase, order=c(4,0,2), seasonal=list(order=c(0,1,0),period=7)) 
fit.alternative1
checkresiduals(fit.alternative1)
fc1<-forecast(fit.yourself,20)

fc1$x <- exp(fc1$x)
fc1$mean <- exp(fc1$mean)
fc1$lower <- exp(fc1$lower)
fc1$upper <- exp(fc1$upper)
autoplot(fc1)

################################ Qurstion 4 ##############################

Ques4 <- Canada_cases
Ques4<-tail(Ques4,-100)
Q4_stringency_index <- Ques4$stringency_index
Q4_newCases <- Ques4$new_cases


# Lagged predictors. Test 0, 1, 2 or 3 lags.
Stringer_lag <- cbind(Q4_stringency_index[],
                c(NA,Q4_stringency_index[1:494]),
                c(NA,NA,Q4_stringency_index[1:493]),
                c(NA,NA,NA,Q4_stringency_index[1:492]))
colnames(Stringer_lag) <- paste("StringencyLag",0:3,sep="")
Stringer_lag


# Choose optimal lag length for advertising based on AIC
# Restrict data so models use same fitting period
fit1 <- auto.arima(Q4_newCases[4:495], xreg=Stringer_lag[4:495,1], d=0)
fit2 <- auto.arima(Q4_newCases[4:495], xreg=Stringer_lag[4:495,1:2], d=0)
fit3 <- auto.arima(Q4_newCases[4:495], xreg=Stringer_lag[4:495,1:3], d=0)
fit4 <- auto.arima(Q4_newCases[4:495], xreg=Stringer_lag[4:495,1:4], d=0)

# Compute Akaike Information Criteria
AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)


# Compute Bayesian Information Criteria
BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)

#Best fit (as per AIC and BIC) is with all data (1), so the final model becomes
fit <- auto.arima(logCanNewcase[], xreg=Stringer_lag[,1], d=0) # d is the order of first-differencing
fit

par(mfrow=c(2,2))
# forecast insurance quotes with Stringency index=10
fc10 <- forecast(fit, xreg=cbind(rep(10,20)), h=20)
plot(fc10, main=" Stringency index = 10", ylab="New Cases")

fc40 <- forecast(fit, xreg=cbind(rep(40,20)), h=20)
plot(fc40, main=" Stringency index = 40", ylab="New Cases")

fc80 <- forecast(fit, xreg=cbind(rep(80,20)), h=20)
plot(fc80, main=" Stringency index = 80", ylab="New Cases")

fc100 <- forecast(fit, xreg=cbind(rep(100,20)), h=20)
plot(fc100, main="Stringency index = 100", ylab="New Cases")
