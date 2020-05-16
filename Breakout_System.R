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
names(Mktdowjones)<-names_col
Mktsdowjones<-Mktsdowjones[,c(1:4,6)]

#Extracting data nikie

Mktnikie<- as.data.frame(N225)
names_col<-c("Open", "High", "Low", "Close", "Volume", "Adjusted")
names(Mktnikie)<-names_col
Mktnikie<-Mktnikie[,c(1:4,6)]



Break_out_System <- function(Mkt, SLoss , MktName){
   # Trading system based on the break out of the previous day???s high/low value.
   #
   # Mkt: market data 
   # SLoss: stop loss 
   # MktName: market???s name for print out 
   # Returns: 
   # results vector.
  
   
   
   
   results <- createResultsVector(MktName[1] , SLoss) 
   
   Mkt$prevHigh <- c( NA, Mkt$High[ - length(Mkt$High) ] )
   Mkt$prevLow <- c( NA, Mkt$Low[ - length(Mkt$Low) ] )
   
 
   # Break out high 
   Mkt$Long <- ifelse(Mkt$High >Mkt$prevHigh ,Mkt$Close -Mkt$prevHigh ,NA) 
   results["LongPL"] <- round(sum(Mkt$Long , na.rm=TRUE)) 
   #Adj for SLoss 
   if (SLoss < 0) { 
      Mkt$Long <- ifelse(Mkt$High >Mkt$prevHigh,
                         ifelse((Mkt$Low-Mkt$Open) < SLoss , SLoss , Mkt$Long), 
                         Mkt$Long) 
       results["LongPL"] <- round(sum(Mkt$Long , na.rm=TRUE)) 
    } 
   
   # Break out low 
  Mkt$Short <- ifelse(Mkt$Low<Mkt$prevLow ,Mkt$prevLow -Mkt$Close ,NA)
  results["ShortPL"] <- round(sum(Mkt$Short , na.rm=TRUE))
  
  if (SLoss < 0) { 
     Mkt$Short <- ifelse(Mkt$Low<Mkt$prevLow , 
                         ifelse((Mkt$Open -Mkt$High) < SLoss , SLoss , Mkt$Short), 
                          Mkt$Short) 
     results["ShortPL"] <- round(sum(Mkt$Short , na.rm=TRUE))
  } 
  
  Stats <- calcStats(Mkt$Long) 
  results[5:7] <- Stats 
  
  Stats <- calcStats(Mkt$Short)
  results[8:10] <- Stats 
  
  return(results) 
}

Break_out_System(Mktdax, 50, "Dax")
