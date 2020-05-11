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
<<<<<<< HEAD
library(caret)
=======

>>>>>>> 569cc1cda621f70b9c0a8d50062ca8217fa075c3
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


#calculating daily returns for each index
dax$return<- na.omit((Delt(dax)))

sp500$return<- na.omit((Delt(sp500)))

dow_jones$return <- na.omit((Delt(dow_jones)))

nikie$return<- na.omit((Delt(nikie)))

<<<<<<< HEAD



#convert into dataframes

dax<- na.omit(as.data.frame(dax))

sp500<- na.omit(as.data.frame(sp500))

dow_jones <- na.omit(as.data.frame(dow_jones))

nikie<- na.omit(as.data.frame(nikie))

=======
#convert into dataframes

dax<- as.data.frame(dax)

sp500<- as.data.frame(sp500)

dow_jones <- as.data.frame(dow_jones)

nikie<- as.data.frame(nikie)
>>>>>>> 569cc1cda621f70b9c0a8d50062ca8217fa075c3

#renaming my columns
names(dax)<-c("price","returns")

names(sp500)<-c("price","returns")

names(dow_jones)<-c("price","returns")

names(nikie)<-c("price","returns")

<<<<<<< HEAD
#Generating daily Direction
#returns >0 "Up"
#returns <0 "Down"
dax$direction<- ifelse(dax$returns>0, 1, 0)

sp500$direction<- ifelse(sp500$returns>0, 1, 0)

dow_jones$direction<- ifelse(dow_jones$returns>0, 1, 0)

nikie$direction<- ifelse(nikie$returns>0, 1, 0)


=======
>>>>>>> 569cc1cda621f70b9c0a8d50062ca8217fa075c3

# calulating technical indicators 

#Simple moving average 50 and 200

dax$sma50<- rollapply(dax$price,50,mean, fill = NA) 

dax$sma200<- rollapply(dax$price,200,mean, fill = NA)

sp500$sma50<- rollapply(sp500$price,50,mean, fill = NA) 

sp500$sma200<- rollapply(sp500$price,200,mean, fill = NA)

dow_jones$sma50<- rollapply(dow_jones$price,50,mean,fill = NA) 

dow_jones$sma200<- rollapply(dow_jones$price,200,mean,fill = NA)

nikie$sma50<- rollapply(nikie$price,50,mean, fill = NA) 

nikie$sma200<- rollapply(nikie$price,200,mean, fill = NA) 

#MACD 12,26,9

macd_dax<- MACD(dax$price,12,26,9,"SMA")
macd_dax<-as.data.frame(macd_dax)
dax<-cbind(dax,macd_dax)

macd_sp500<- MACD(sp500$price,12,26,9,"SMA")
macd_sp500<-as.data.frame(macd_sp500)
sp500<-cbind(sp500,macd_sp500)

macd_dowjones<- MACD(dow_jones$price,12,26,9,"SMA")
macd_dowjones<-as.data.frame(macd_dowjones)
dow_jones<-cbind(dow_jones, macd_dowjones)

macd_nikie<- MACD(nikie$price,12,26,9,"SMA")
macd_nikie<-as.data.frame(macd_nikie)
nikie<-cbind(nikie, macd_nikie)


#RSI

dax$rsi<- RSI(dax$price,5,"SMA") 

sp500$rsi<- RSI(sp500$price,5,"SMA") 

dow_jones$rsi<- RSI(dow_jones$price,5,"SMA") 

nikie$rsi_<- RSI(nikie$price,5,"SMA") 

#stochastics

stoch_dax<-stoch(dax$price, nFastK= 14, nFastD=3, nSlowD=3)
stoch_dax<-as.data.frame(stoch_dax)
dax<- cbind(dax, stoch_dax)


stoch_sp500<-stoch(sp500$price, nFastK= 14, nFastD=3, nSlowD=3)
stoch_sp500<-as.data.frame(stoch_sp500)
sp500<-cbind(sp500, stoch_sp500)

stoch_dow_jones<-stoch(dow_jones$price, nFastK= 14, nFastD=3, nSlowD=3)
stoch_dow_jones<-as.data.frame(stoch_dow_jones)
dow_jones<-cbind(dow_jones,stoch_dow_jones)


stoch_nikie<-stoch(nikie$price, nFastK= 14, nFastD=3, nSlowD=3)
stoch_nikie<-as.data.frame(stoch_nikie)
nikie<-cbind(nikie, stoch_nikie)


#Bollinger Bands

bollinger_bands_dax<- BBands(dax$price,20,"SMA",2)
bollinger_bands_dax<-as.data.frame(bollinger_bands_dax)
dax<-cbind(dax, bollinger_bands_dax)

bollinger_bands_sp500<- BBands(sp500$price,20,"SMA",2)
bollinger_bands_sp500<- as.data.frame(bollinger_bands_sp500)
sp500<-cbind(sp500, bollinger_bands_sp500)

bollinger_bandsdow_jones<- BBands(dow_jones$price,20,"SMA",2)
bollinger_bandsdow_jones<-as.data.frame(bollinger_bandsdow_jones)
dow_jones<-cbind(dow_jones, bollinger_bandsdow_jones)

bollinger_bands_nikie<- BBands(nikie$price,20,"SMA",2)
bollinger_bands_nikie<-as.data.frame(bollinger_bands_nikie)
nikie<-cbind(nikie, bollinger_bands_nikie)



<<<<<<< HEAD



#Normalization of our data using the Min-Max method.
source("normalize_func.R")
dax<-na.omit(dax)
norm_dax <- normalize.many(dax, c(1,2,3:15))
norm_dax<-norm_dax[,16:30]

sp500<-na.omit(sp500)
norm_sp500 <- normalize.many(sp500, c(1,2,3:15))
norm_sp500<-norm_sp500[,16:30]

dow_jones<-na.omit(dow_jones)
norm_dowjones <- normalize.many(dow_jones, c(1,2,3:15))
norm_dowjones<-norm_dowjones[,16:30]

nikie<-na.omit(nikie)
norm_nikie <- normalize.many(nikie, c(1,2,3:15))
norm_nikie<-norm_nikie[,16:30]




#Splitting dax
idx_dax <- sample(seq(1, 3), size = nrow(norm_dax), replace = TRUE, prob = c(.6, .2, .2))
train_dax <- norm_dax[idx_dax == 1,]
test_dax <- norm_dax[idx_dax == 2,]
vali_dax <- norm_dax[idx_dax == 3,]

#Splitting sp500
idx_sp500 <- sample(seq(1, 3), size = nrow(norm_sp500), replace = TRUE, prob = c(.6, .2, .2))
train_sp500 <- norm_sp500[idx_sp500 == 1,]
test_sp500 <- norm_sp500[idx_sp500 == 2,]
vali_sp500 <- norm_sp500[idx_sp500 == 3,]

#Splitting dowjones
idx_dow_jones <- sample(seq(1, 3), size = nrow(norm_dowjones), replace = TRUE, prob = c(.6, .2, .2))
train_dow_jones <- norm_dowjones[idx_dow_jones == 1,]
test_dowjones<- norm_dowjones[idx_dow_jones == 2,]
vali_dowjones <- norm_dowjones[idx_dow_jones == 3,]

#Splitting nikie
idx_nikie <- sample(seq(1, 3), size = nrow(norm_nikie), replace = TRUE, prob = c(.6, .2, .2))
train_nikie <- norm_nikie[idx_nikie == 1,]
test_nikie <- norm_nikie[idx_nikie == 2,]
vali_nikie <- norm_nikie[idx_nikie == 3,]



=======
#Generating daily Direction
#returns >0 "Up"
#returns <0 "Down"
dax$direction<- ifelse(dax$returns>0, "Up", "Down")

sp500$direction<- ifelse(sp500$returns>0, "Up", "Down")

dow_jones$direction<- ifelse(dow_jones$returns>0, "Up", "Down")

nikie$direction<- ifelse(nikie$returns>0, "Up", "Down")



#We divide our data into three parts
#Training dataset -> training the neural network 
# Validating dataset -> validating the estimated parameters  
# Testing dataset -> measure the accuracy of the prediction 

#Splitting dax
idx_dax <- sample(seq(1, 3), size = nrow(dax), replace = TRUE, prob = c(.6, .2, .2))
train_dax2 <- dax[idx_dax == 1,]
test_dax2 <- dax[idx_dax == 2,]
vali_dax2 <- dax[idx_dax == 3,]

#Splitting sp500
idx_sp500 <- sample(seq(1, 3), size = nrow(sp500), replace = TRUE, prob = c(.6, .2, .2))
train_sp5002 <- dax[idx_sp500 == 1,]
test_sp5002 <- dax[idx_sp500 == 2,]
vali_sp5002 <- dax[idx_sp500 == 3,]

#Splitting dowjones
idx_dow_jones <- sample(seq(1, 3), size = nrow(dow_jones), replace = TRUE, prob = c(.6, .2, .2))
train_dow_jones2 <- dax[idx_dow_jones == 1,]
test_dowjones2 <- dax[idx_dow_jones == 2,]
vali_dowjones2 <- dax[idx_dow_jones == 3,]

#Splitting nikie
idx_nikie <- sample(seq(1, 3), size = nrow(nikie), replace = TRUE, prob = c(.6, .2, .2))
train_nikie2 <- dax[idx_nikie == 1,]
test_nikie2 <- dax[idx_nikie == 2,]
vali_nikie2 <- dax[idx_nikie == 3,]



#Imputing data for the missing values

md.pattern(train_dax2)
md.pattern(vali_dax2)


imputed_train_dax2 <- mice(train_dax2, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_train_dax2)

imputed_vali_dax2 <- mice(vali_dax2, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_vali_dax2)

#get completed data
complete_train_dax <- complete(imputed_train_dax2,2)
complete_vali_dax <- complete(imputed_vali_dax2,2)
>>>>>>> 569cc1cda621f70b9c0a8d50062ca8217fa075c3

#ANN model
set.seed(1)

#Implement ANN for DAx
## This part has an error

<<<<<<< HEAD
neural_network_dax<- neuralnet(direction.z~price.z +  sma50.z+sma200.z+macd.z+signal.z+rsi.z+fastK.z
                              +fastD.z +slowD.z+dn.z+mavg.z+up.z +pctB.z,
                               data=train_dax, threshold = 0.1, hidden=5, err.fct = "sse", act.fct = "logistic",
                               rep = 3, algorithm = "rprop+",
                               linear.output = TRUE) 
 neural_network_dax 
=======
neural_network_dax<- neuralnet(direction~price + sma50+sma200+macd+signal+rsi+fastK+fastD +slowD+dn+mavg+up+pctB,
                               data=complete_train_dax, hidden=c(5,3), err.fct = "sse", act.fct = "tanh",
                               rep = 1, algorithm = "rprop+",
                               linear.output = TRUE) 
neural_network_dax 
>>>>>>> 569cc1cda621f70b9c0a8d50062ca8217fa075c3


# plot neural network
plot(neural_network_dax)


# Prediction
<<<<<<< HEAD
output <- compute(neural_network_dax,train_dax[,-3])
head(output$net.result)
p1 <- output$net.result
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(pred1, train_dax$direction)
tab1
1-sum(diag(tab1))/sum(tab1)
=======
output <- compute(neural_network_dax, complete_train_dax[,-15])
head(output$net.result)
p1 <- output$net.result
pred1 <- ifelse(p1>0.5, "Up", "Down")
tab1 <- table(pred1, complete_train_dax$direction)
tab1

#Making prediction using validation data
vali_pred<- predict(neural_network_dax,complete_vali_dax) 
head(vali_pred) 

vali_pred_class<-data.frame(matrix(NA,dim(vali_pred)[1],1))

vali_pred_class[vali_pred[,"Down"] > 0.5,1] <- "Down" 

vali_pred_class[vali_pred[,"Up"] > 0.5,1] <- "Up" 

>>>>>>> 569cc1cda621f70b9c0a8d50062ca8217fa075c3


