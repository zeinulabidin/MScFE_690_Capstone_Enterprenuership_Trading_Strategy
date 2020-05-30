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

SLoss<-50
source("General_functions_code.R")
#Downloading the data of the indices that we are going to use for backtesting
indices <- c('^GDAXI' # German Dax Index 
             ,'^GSPC',# S&P 500 index
             '^DJI', # DOw30 Index
             '^N225' ) #Nikie index

start_date <- '2010-01-01'

end_date <- '2019-12-31'

getSymbols(Symbols = indices, src = "yahoo",  index.class = "POSIXct", from =start_date, to = end_date)

#Market names
MktName<-c("DAX", "SP500",  "Dow", "Nikkei")

#Extracting data Dax

Mktdax<- as.data.frame(GDAXI)
names_col<-c("Open", "High", "Low", "Close", "Volume", "Adjusted")
names(Mktdax)<-names_col
Mktdax<-Mktdax[,c(1:4,6)]

#Extracting data sp500

Mktsp500<- as.data.frame(GSPC)
names_col<-c("Open", "High", "Low", "Close", "Volume", "Adjusted")
names(Mktsp500)<-names_col
Mktsp500<-Mktsp500[,c(1:4,6)]

#Extracting data dowjones

Mktsdowjones<- as.data.frame(DJI)
names_col<-c("Open", "High", "Low", "Close", "Volume", "Adjusted")
names(Mktsdowjones)<-names_col
Mktsdowjones<-Mktsdowjones[,c(1:4,6)]

#Extracting data nikie

Mktnikie<- as.data.frame(N225)
names_col<-c("Open", "High", "Low", "Close", "Volume", "Adjusted")
names(Mktnikie)<-names_col
Mktnikie<-Mktnikie[,c(1:4,6)]




Mktdax$prevHigh <- c( NA, Mktdax$High[ - length(Mktdax$High) ] )
Mktdax$prevLow <- c( NA, Mktdax$Low[ - length(Mktdax$Low) ] )
  
###########################################################################################################
#Dax performance
# Break out high 
Mktdax$LongPl <- round(ifelse(Mktdax$High >Mktdax$prevHigh ,Mktdax$Close -Mktdax$prevHigh ,NA), 2) 

 
if (SLoss < 0) { 
    Mktdax$LongPl <- ifelse(Mktdax$High >Mktdax$prevHigh,
                       ifelse((Mktdax$Low-Mktdax$Open) < SLoss , SLoss , Mktdax$LongPl), 
                       Mktdax$LongPl) 
    #results["LongPL"] <- round(sum(Mkt$Long , na.rm=TRUE)) 
  } 
  
# Break out low 
Mktdax$Short <- ifelse(Mktdax$Low<Mktdax$prevLow,Mktdax$prevLow -Mktdax$Close ,NA)
  #results["ShortPL"] <- round(sum(Mkt$Short , na.rm=TRUE))
  
if (SLoss < 0) { 
    Mktdax$Short <- ifelse(Mktdax$Low<Mktdax$prevLow , 
                        ifelse((Mktdax$Open -Mktdax$High) < SLoss , SLoss , Mktdax$Short), 
                        Mktdax$Short) 
    #results["ShortPL"] <- round(sum(Mkt$Short , na.rm=TRUE))
} 




Mktdax$Pl<-rowSums(Mktdax[, c("LongPl", "Short")], na.rm = TRUE)
Mktdax$CumPl<- ave(Mktdax$Pl, FUN=cumsum)


Mktdax<-within(Mktdax, daxReturns<-c(0, diff(Adjusted)))

Mktdax$daxReturns[which(is.na(Mktdax$daxReturns))]<-0
Mktdax$CumDaxR<-ave(Mktdax$daxReturns, FUN=cumsum)
  
  
autoplot(ts(Mktdax[,"CumPl"]), series = " Dax breakout system") +
  autolayer(ts(Mktdax[,"CumDaxR"]), series = "Dax Index")+
  ggtitle("Dax Cumulative Profit and Loss") +
  xlab("Time") +
  ylab("Returns")
  labs(colour = "Performance")
  scale_color_manual( values = c(1, 2))

 
##########################################################################################################
#SP500 performance


Mktsp500$prevHigh <- c( NA, Mktsp500$High[ - length(Mktsp500$High) ] )
Mktsp500$prevLow <- c( NA, Mktsp500$Low[ - length(Mktsp500$Low) ] )


# Break out high 
Mktsp500$LongPl <- round(ifelse(Mktsp500$High >Mktsp500$prevHigh,Mktsp500$Close -Mktsp500$prevHigh ,NA), 2) 


if (SLoss < 0) { 
  Mktsp500$LongPl <- ifelse(Mktsp500$High >Mktsp500$prevHigh,
                          ifelse((Mktsp500$Low-Mktdax$Open) < SLoss , SLoss , Mktsp500$LongPl), 
                          Mktsp500$LongPl) 
  #results["LongPL"] <- round(sum(Mkt$Long , na.rm=TRUE)) 
} 

# Break out low 
Mktsp500$Short <- ifelse(Mktsp500$Low<Mktsp500$prevLow,Mktsp500$prevLow -Mktsp500$Close ,NA)
#results["ShortPL"] <- round(sum(Mkt$Short , na.rm=TRUE))

if (SLoss < 0) { 
  Mktsp500$Short <- ifelse(Mktsp500$Low<Mktsp500$prevLow , 
                         ifelse((Mktsp500$Open -Mktsp500$High) < SLoss , SLoss , Mktsp500$Short), 
                         Mktsp500$Short) 
  #results["ShortPL"] <- round(sum(Mkt$Short , na.rm=TRUE))
} 




Mktsp500$Pl<-rowSums(Mktsp500[, c("LongPl", "Short")], na.rm = TRUE)
Mktsp500$CumPl<- ave(Mktsp500$Pl, FUN=cumsum)


Mktsp500<-within(Mktsp500, spReturns<-c(0, diff(Adjusted)))

Mktsp500$spReturns[which(is.na(Mktsp500$spReturns))]<-0
Mktsp500$CumSpR<-ave(Mktsp500$spReturns, FUN=cumsum)


autoplot(ts(Mktsp500[,"CumPl"]), series = "Breakout System") +
  autolayer(ts(Mktsp500[,"CumSpR"]), series = "SP500 Index")+
  ggtitle("S&P 500 Cumulative Profit and Loss") +
  xlab("Time") +
  ylab("Returns")+
  labs(colour = "Performance") +
  scale_color_manual( values = c(1, 2))
