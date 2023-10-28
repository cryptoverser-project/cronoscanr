# rescale the value of a token with respective decimals 
rescale_decimals <- function(x, decimals = 18){
  
  y <- as.numeric(x)
  # scale each number depending on decimals 
  if (length(decimals) == length(x)) {
    for(i in 1:length(y)){
      y[i] <- y[i]/(10^as.numeric(decimals[i]))
    }
  } else {
    # scale all numbers with same decimals 
    scale <- 10^as.numeric(decimals)
    y <- y/scale
  }
  return(y)
}

#' rescale_decimals(123, 18)
