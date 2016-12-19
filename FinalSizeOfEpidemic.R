

# root equation factory for the final size of an epidemic
rootEquation <- function(R){
  function(s) log(s) - R * (s - 1) 
}

#
rootsOut <- function(minR0, maxR0, partitions = 1000){
  interval <- c(minR0, maxR0)
  sequence <- seq(from = interval[1], to = interval[2], length.out = partitions)
  equations <- lapply(sequence, rootEquation)
  roots <- lapply(equations, 
                  uniroot,
                  lower = 1e-08,
                  upper = 1 - 1e-08,
                  tol = 1e-12)
  roots <- lapply(roots, function(root) root$root)
  
  plot(x = sequence, y = roots, type = "l")
  out <- data.frame(min = NA,
                    max = NA)
  out$min <- min(unlist(roots))
  out$max <- max(unlist(roots))
  return(out)
}
rootsOut(1.5, 1.8)
