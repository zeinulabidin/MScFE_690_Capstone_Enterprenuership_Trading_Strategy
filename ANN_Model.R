#Loading necessary libraries
library(zoo)
library(xts)
library(quantmod)
library(PerformanceAnalytics)
library(dplyr)
library(fpp2)
library(nnet)


#Downloading the data of the indices that we are going to use for backtesting
indices <- c('^GDAXI' # SPDR S&P 500 ETF TRUST 
          ,'^GSPC',# iShares Core S&P 500 ETF 
          '^DJI', # PowerShares QQQ Trust, Series 1
          '^N225' ) #iShares Russell 1000 Growth ETF

start_date <- '2010-01-01'

end_date <- '2019-12-31'

getSymbols(Symbols = indices, src = "yahoo",  index.class = "POSIXct", from =start_date, to = end_date)


#extracting the data for each index and remove missing values

dax<- na.omit(GDAXI[,"GDAXI.Adjusted"])

sp500<-na.omit(GSPC[,"GSPC.Adjusted"])

dow_jones<- na.omit(DJI[,"DJI.Adjusted"])

nikie<- na.omit(N225[,"N225.Adjusted"])


#calculating daily returns for each index
dax_return<- na.omit(Delt(dax))

sp500_return<- na.omit(Delt(sp500))

dow_return <- na.omit(Delt(dow_jones))

nikie_return<- na.omit(Delt(nikie))

# calulating technical indicators 

#Simple moving average 50 and 200

sma50_dax<- rollapply(dax,50,mean) 

sma200_dax<- rollapply(dax,200,mean)

sma50_sp500<- rollapply(sp500,50,mean) 

sma200_sp500<- rollapply(sp500,200,mean)

sma50_dowjones<- rollapply(dow_jones,50,mean) 

sma200_dowjones<- rollapply(dow_jones,200,mean)

sma50_nikie<- rollapply(nikie,50,mean) 

sma200_nikie<- rollapply(nikie,200,mean) 

#MACD 12,26,9

macd_dax<- MACD(dax,12,26,9,"SMA")

macd_sp500<- MACD(sp500,12,26,9,"SMA")

macd_dowjones<- MACD(dow_jones,12,26,9,"SMA")

macd_nikei<- MACD(nikie,12,26,9,"SMA")


#RSI

rsi_dax<- RSI(dax,5,"SMA") 

rsi_sp500<- RSI(sp500,5,"SMA") 

rsi_dowjones<- RSI(dow_jones,5,"SMA") 

rsi_nikei<- RSI(nikie,5,"SMA") 

#stochastics
                 
stoch_dax<-stoch(dax, nFastK= 14, nFastD=3, nSlowD=3)

stoch_sp500<-stoch(sp500, nFastK= 14, nFastD=3, nSlowD=3)

stoch_dowjones<-stoch(dow_jones, nFastK= 14, nFastD=3, nSlowD=3)

stoch_nikei<-stoch(nikie, nFastK= 14, nFastD=3, nSlowD=3)


#Bollinger Bands

bollinger_bands_dax<- BBands(dax,20,"SMA",2)

bollinger_bands_sp500<- BBands(sp500,20,"SMA",2)

bollinger_bands_dowjones<- BBands(dow_jones,20,"SMA",2)

bollinger_bands_nikie<- BBands(nikie,20,"SMA",2)

#Generating daily Direction
#returns >0 "Up"
#returns <0 "Down"

direction_dax<- data.frame(matrix(NA, dim(dax)[1],1))

direction_sp500<- data.frame(matrix(NA,dim(sp500)[1],1)) 

direction_dowjones<- data.frame(matrix(NA,dim(dow_jones)[1],1)) 

direction_nikie<- data.frame(matrix(NA,dim(nikie)[1],1)) 

direction_dax[dax_return> 0, ] <- "Up" 
direction_dax[dax_return< 0, ] <- "Down" 

direction_sp500[sp500_return> 0, ] <- "Up" 
direction_sp500[sp500_return< 0, ] <- "Down" 

direction_dowjones[dow_return> 0, ] <- "Up" 
direction_dowjones[dow_return< 0, ] <- "Down" 

direction_nikie[nikie_return> 0, ] <- "Up" 
direction_nikie[nikie_return< 0, ] <- "Down" 

# Combining the closing prices with the technical indicators
dax_comb<- cbind(dax, sma50_dax, sma200_dax, macd_dax, rsi_dax, bollinger_bands_dax, stoch_dax)

sp500_comb<- cbind(sp500, sma50_sp500, sma200_sp500, macd_sp500, rsi_sp500, bollinger_bands_sp500, stoch_sp500)

dowjones_comb<- cbind(dow_jones, sma50_dowjones, sma200_dowjones, macd_dowjones, rsi_dowjones, bollinger_bands_dowjones,
                      stoch_dowjones)

nikie_comb<- cbind(nikie, sma50_nikie, sma200_nikie, macd_nikei, rsi_nikei, bollinger_bands_nikie, stoch_nikei)


#We divide our data into three parts
#Training dataset -> training the neural network 
# Validating dataset -> validating the estimated parameters  
# Testing dataset -> measure the accuracy of the prediction 

train_sdate<- "2010-01-01" 
train_edate<- "2015-12-31" 

vali_sdate<- "2016-01-01" 
vali_edate<- "2018-06-30" 

test_sdate<- "2018-07-01" 
test_edate<- "2019-12-31"

#Data ranges
#Dax
trainrow_dax<- which(index(dax_comb) >= train_sdate & index(dax_comb) <= train_edate) 

valirow_dax<- which(index(dax_comb) >= vali_sdate & index(dax_comb) <= vali_edate) 

testrow_dax<- which(index(dax_comb) >= test_sdate & index(dax_comb) <= test_edate)

#sp500
trainrow_sp500<- which(index(sp500_comb) >= train_sdate & index(sp500_comb) <= train_edate) 

valirow_sp500<- which(index(sp500_comb) >= vali_sdate & index(sp500_comb) <= vali_edate) 

testrowsp500<- which(index(sp500_comb) >= test_sdate & index(sp500_comb) <= test_edate)

#Dowjones
trainrow_dowjones<- which(index(dowjones_comb) >= train_sdate & index(dowjones_comb) <= train_edate) 

valirow_dowjones<- which(index(dowjones_comb) >= vali_sdate & index(dowjones_comb) <= vali_edate) 

testrow_dowjones<- which(index(dowjones_comb) >= test_sdate & index(dowjones_comb) <= test_edate)

#Nikie
trainrow_nikie<- which(index(nikie_comb) >= train_sdate & index(nikie_comb) <= train_edate) 

valirow_nikie<- which(index(nikie_comb) >= vali_sdate & index(nikie_comb) <= vali_edate) 

testrow_nikie<- which(index(nikie_comb) >= test_sdate & index(nikie_comb) <= test_edate)


#Extracting data for training, validating and testing periods 

#Dax data

train_dax<- dax_comb[trainrow_dax,] 
vali_dax<- dax_comb[valirow_dax,] 
test_dax<- dax_comb[testrow_dax,]

#Sp500 data

train_sp500<- sp500_comb[trainrow_sp500,] 
vali_sp500<- sp500_comb[valirow_sp500,] 
test_sp500<- sp500_comb[testrowsp500,]

#dowjones data
train_dowjones<- dax_comb[trainrow_dowjones,] 
vali_dowjones<- dax_comb[valirow_dowjones,] 
test_dowjones<- dax_comb[testrow_dowjones,]

#nikie data
train_nikie<- dax_comb[trainrow_nikie,] 
vali_nikie<- dax_comb[valirow_nikie,] 
test_nikie<- dax_comb[testrow_nikie,]

# Normalization of our data using matrix transpose

#Calculate mean and standard deviation of the training data 
trainme_dax<- apply(train_dax,2,mean) 
trainstd_dax<- apply(train_dax,2,sd)


trainme_sp500<- apply(train_sp500,2,mean) 
trainstd_sp500<- apply(train_sp500,2,sd)


trainme_dowjones<- apply(train_dowjones,2,mean) 
trainstd_dowjones<- apply(train_dowjones,2,sd)


trainme_nikie<- apply(train_nikie,2,mean) 
trainstdnikie<- apply(train_nikie,2,sd)

#Create  matrices of dimensions equal to the Training, validating and testing data dimensions

#Dax
trainidn_dax<- (matrix(1,dim(train_dax)[1],dim(train_dax)[2])) 

valiidn_dax<- (matrix(1,dim(vali_dax)[1],dim(vali_dax)[2]))

testidn_dax<- (matrix(1,dim(test_dax)[1],dim(test_dax)[2])) 

#sp500
trainidn_sp500<- (matrix(1,dim(train_sp500)[1],dim(train_sp500)[2])) 

valiidn_sp500<- (matrix(1,dim(vali_sp500)[1],dim(vali_sp500)[2]))

testidn_sp500<- (matrix(1,dim(test_sp500)[1],dim(test_sp500)[2]))

#Dowjones
trainidn_dowjones<- (matrix(1,dim(train_dowjones)[1],dim(train_dowjones)[2])) 

valiidn_dowjones<- (matrix(1,dim(vali_dowjones)[1],dim(vali_dowjones)[2]))

testidn_dowjones<- (matrix(1,dim(test_dowjones)[1],dim(test_dowjones)[2])) 

#nikie
trainidn_nikie<- (matrix(1,dim(train_nikie)[1],dim(train_nikie)[2])) 

valiidn_nikie<- (matrix(1,dim(vali_nikie)[1],dim(vali_nikie)[2]))

testidn_nikie<- (matrix(1,dim(test_nikie)[1],dim(test_nikie)[2])) 

#Normalization
#Dax
norm_train_dax<- (train_dax - t(trainme_dax*t(trainidn_dax))) /t(trainstd_dax*t(trainidn_dax))
norm_vali_dax<- (vali_dax - t(trainme_dax*t(valiidn_dax))) / t(trainstd_dax*t(valiidn_dax)) 
norm_test_dax<- (test_dax - t(trainme_dax*t(testidn_dax))) / t(trainstd_dax*t(testidn_dax)) 

#sp500
norm_train_sp500<- (train_sp500 - t(trainme_sp500*t(trainidn_sp500))) /t(trainstd_sp500*t(trainidn_sp500)) 
norm_vali_sp500<- (vali_sp500 - t(trainme_sp500*t(valiidn_sp500))) / t(trainstd_sp500*t(valiidn_sp500)) 
norm_test_sp500<- (test_sp500 - t(trainme_sp500*t(testidn_sp500))) / t(trainstd_sp500*t(testidn_sp500))

#Dowjones
norm_train_dowjones<- (train_dowjones - t(trainme_dowjones*t(trainidn_dowjones))) /t(trainstd_dowjones*t(trainidn_dowjones)) 
norm_vali_dowjones<- (vali_dowjones - t(trainme_dowjones*t(valiidn_dowjones))) / t(trainstd_dowjones*t(valiidn_dowjones)) 
norm_test_dowjones<- (test_dowjones - t(trainme_dowjones*t(testidn_dowjones))) / t(trainstd_dowjones*t(testidn_dowjones))

#Nikie
norm_train_nikie<- (train_nikie - t(trainme_nikie*t(trainidn_nikie))) /t(trainstdnikie*t(trainidn_nikie)) 
norm_vali_nikie<- (vali_nikie - t(trainme_nikie*t(valiidn_nikie))) / t(trainstdnikie*t(valiidn_nikie)) 
norm_test_nikie<- (test_nikie - t(trainme_nikie*t(testidn_nikie))) / t(trainstdnikie*t(testidn_nikie))


#Define training for training, validating and testing of the direction
#Dax
traindir_dax<- direction_dax[trainrow_dax,1] 
validir_dax<- direction_dax[valirow_dax,1] 
testdir_dax<- direction_dax[testrow_dax,1]

#sp500
traindir_sp500<- direction_sp500[trainrow_sp500,1] 
validir_sp500<- direction_sp500[valirow_sp500,1] 
testdir_sp500<- direction_sp500[testrowsp500,1]

#Dowjone
traindir_dowjones<- direction_dowjones[trainrow_dowjones,1] 
validir_dowjones<- direction_dowjones[valirow_dowjones,1] 
testdir_dowjones<- direction_dowjones[testrow_dowjones,1]

#Nikie
traindir_nikie<- direction_nikie[trainrow_nikie,1] 
validir_nikie<- direction_nikie[valirow_nikie,1] 
testdir_nikie<- direction_nikie[testrow_nikie,1]

#ANN model
set.seed(1)

#Implement ANN for DAx
## This part has an error
train_dax1<- na.omit(train_dax)
train_dax1<- train_dax1[2:1522,]

class(train_dax1)
length(train_dax1)
length(traindir_dax)
neural_network_dax<- nnet(train_dax1,class.ind(traindir_dax),size=4 ,trace=T) 
neural_network_dax 







