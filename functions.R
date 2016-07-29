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
  x[nobs + 1, 1] <- "Total"
  return(x)
}

row.merge <- function (x){
  # Merges & sums the last two rows (excluding the first colum) and relabels the
  # final row "70+"
  x[, 1] <- as.character(x[, 1])
  nobs <- nrow(x)
  nvar <- ncol(x)
  x[nobs-1, 2:nvar] <- x[nobs, 2:nvar] + x[nobs-1, 2:nvar]
  x <- x[-nobs, ]
  x[nobs-1, 1] <- "70+"
  return(x)
}