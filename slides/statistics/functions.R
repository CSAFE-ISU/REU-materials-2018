# z-score function

zscore <- function(x, mu, sigma){
  # put the stuff the function does here
  # computing the z score: (obs - mean)/sd
  z <- (x-mu)/sigma 
  return(z)
}

zscore(4948, 4313, 583)

# function skeleton
my_fun <- function(args){
  # stuff the function does 
  return(val) # must return 1 object only! 
}