#min-max normalization function

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}





normalize.many <- function(dat, column.nos) {
  nms <- names(dat)
  for(col in column.nos) {
    name <- paste(nms[col],".z", sep = "")
    dat[name] <- normalize(dat[,col])
  }
  cat(paste("Normalized ", length(column.nos), " variable(s)\n"))
  dat
}

