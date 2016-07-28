ci.rate <- function(rate_unit, pop, num_cases, upper = T){
  # calculates upper and lower 95%CI for crude rates
  if(upper){
    result <- rate_unit / pop * (num_cases + 1.96 * sqrt(num_cases))
  } else {
   
    result <- rate_unit / pop * (num_cases - 1.96 * sqrt(num_cases))
  }
  return(result)
}



citywide <- function(x){
  # Sums the city-wide counts and returns df with added row for totals
  x <- x
  nvar <- ncol(x)
  nobs <- nrow(x)
  x[, 1] <- as.character(x[, 1])
  x[nobs+1, 2:nvar] <- colSums(x[, 2:nvar])
  x[nobs + 1, 1] <- "total"
  return(x)
}