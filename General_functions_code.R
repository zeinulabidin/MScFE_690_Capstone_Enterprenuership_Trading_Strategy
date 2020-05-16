
#market names
nm <- c("DAX", "SP500",  "Dow", "Nikkei")

#Creating function to store results
createResultsVector <- function(MktName , SLossValue){
  # Function to create results vector
  # Args:
  # SLoss: stop loss value
  # MktName: market???s name for print out
  # Returns:
  # results vector.
  
  results <- rep(0,11)
  nam <- c("Mkt", # 1. Name of Mkt
           "S Loss",
           "LongPL",
           "ShortPL",  
           "L Win %",  
           "L Trades", 
           "Av L PL", 
           "S Win %", 
           "S Trades", 
           "Av S PL", 
           "SMA") 
  names(results) <- nam 
  results["Mkt"] <- MktName
  results["S Loss"] <- SLossValue 
  return(results) 
} 

calcStats <- function(x){ 
   # Function to calculate trade stats 
   # 
   # Args: 
   # x - data set 
   # 
   # Returns: 
   # results vector. 
  
  results <- 1:3 
  v <- na.omit(x) 
  
  # Win % 
  wins <- length(v[v>0]) 
  losses <- length(v[v<0]) 
  results[1] <- round(wins/(wins+losses)*100) 
  
  # Num Trades 
  results[2] <- length(v) 
  
  # Av Long PL 
  results[3] <- round(sum(v) / length(v))
  
  return(results) 
} 

calcWinPer <- function(x){ 
  wins <- length(x[x>0]) 
  losses <- length(x[x<0])
  return(wins/(wins+losses)*100)
}

calcAverageWin <- function(x){ 
  wins <- length(x) 
  winpl <- sum(x, na.rm=T) 
  return((winpl/wins)) 
} 

calcNumTrades <- function(x){ 
  return(length(na.omit(x)))
}