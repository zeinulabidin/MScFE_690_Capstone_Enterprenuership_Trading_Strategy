#Loading necessary libraries
library(zoo)
library(xts)
library(quantmod)
library(PerformanceAnalytics)
library(dplyr)
library(fpp2)
library(nnet)
library(mice)
library(neuralnet)
library(caret)
library(tseries)
#Downloading the data of the indices that we are going to use for backtesting
indices <- c('^GDAXI' # German Dax Index 
             ,'^GSPC',# S&P 500 index
             '^DJI', # DOw30 Index
             '^N225' ) #Nikie index

start_date <- '2010-01-01'

end_date <- '2019-12-31'

getSymbols(Symbols = indices, src = "yahoo",  index.class = "POSIXct", from =start_date, to = end_date)

#extracting the data for each index and remove missing values

dax<- na.omit(GDAXI[,"GDAXI.Adjusted"])

sp500<-na.omit(GSPC[,"GSPC.Adjusted"])

dow_jones<- na.omit(DJI[,"DJI.Adjusted"])

nikie<- na.omit(N225[,"N225.Adjusted"])

#Draw the Autocorelated partial autocorrealetd function to oberse the stationarity of the series

dax1<-dax$GDAXI.Adjusted[3:2511]
log_dax<-na.omit(log(dax1))
par(mfrow=c(1,2))
Acf(log_dax, lag.max =40, main="ACF diffenced series")
Pacf(log_dax, lag.max =40, main="PACF diffenced series")

diff_log_dax<- na.omit(diff(log_dax))
head(diff_log_dax)

#ADF test
print(adf.test(diff_log_dax))

#Fit the model to autoarima
fit_dax<- auto.arima(log_dax, seasonal = FALSE)
tsdisplay(residuals(fit_dax), lag.max = 40, main = "Residua Auto.Arima")
fit_dax

#Forecasted values from Auto.Arima
log_dax_forecasted<- forecast(fit_dax, h=20)
log_dax_forecasted
dax_forecasted_extracted<- as.numeric(log_dax_forecasted$mean)
dax_forecasted<-exp(dax_forecasted_extracted)


#Percentage error
df_dax<- data.frame(dax$GDAXI.Adjusted[2512:2531],  dax_forecasted)
col_headings<-c("Actual price", "Forecasted price")
names(df_dax)<-col_headings

df_dax$perfentage_error_dax<- (df_dax$`Actual price`-df_dax$`Forecasted price`)/(df_dax$`Actual price`)
df_dax
mean(df_dax$perfentage_error_dax)


