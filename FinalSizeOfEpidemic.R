# INTRO -------------------------------------------------------------------
rm(list = ls())
graphics.off()
load("data/r0.Rdata")

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
  out$min <- 1-max(unlist(roots))
  out$max <- 1-min(unlist(roots))
  return(out)
}

# CPH

finalSize <- function(x, city_f, method_f = "TD"){
  # browser()
  out <- x %>%
    filter(city==city_f, method!=method_f)
  out <- rootsOut(min(out$ci_l), max(out$ci_u))
  return(out)
}

cfrAdjusted <- function(final_size, city_pop, num_dead){
  true_sick <- c(final_size$max * city_pop, final_size$min * city_pop)
  num_dead / true_sick * 100
}

finalSize(r0, "Copenhagen")
r0 %>%
  finalSize("Copenhagen") %>%
  cfrAdjusted(city_pop = 138030, num_dead = 4737)

finalSize(r0, "Åalborg")
r0 %>%
  finalSize("Åalborg") %>%
  cfrAdjusted(city_pop = 8621, num_dead = 409)

finalSize(r0, "Korsør")
r0 %>%
  finalSize("Korsør") %>%
  cfrAdjusted(city_pop = 2258, num_dead = 201)




