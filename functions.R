ci.rate <- function(rate_unit, pop, num_cases, upper = T){
  # calculates upper and lower 95%CI for crude rates
  if(upper){
    result <- rate_unit / pop * (num_cases + 1.96 * sqrt(num_cases))
  } else {
    result <- rate_unit / pop * (num_cases - 1.96 * sqrt(num_cases))
  }
  return(result)
}