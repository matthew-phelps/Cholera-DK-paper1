ci.rate <- function(rate_unit, pop, num_cases, upper = T){
  # calculates upper and lower 95%CI for crude rates
  # from: http://goo.gl/vLe54m
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
  x[nobs+1, 2:nvar] <- colSums(data.frame(x[, 2:nvar]))
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


rotatedAxisElementText = function(size,angle,position='x'){
  # Function allows auto justification when axis labels are rotated between 0 
  # and 90* in ggplot2. From: https://goo.gl/MX8bSj 
  # Function to Return Element Text Object to be delivered to a ggplot function.
  # First aurgument is the angel which text should be roated, the second is "x"
  # or "y" to specify which axis is to be rotated.
  size      = size[1];
  angle     = angle[1]; 
  position  = position[1]
  positions = list(x=0,y=90,top=180,right=270)
  if(!position %in% names(positions))
    stop(sprintf("'position' must be one of [%s]",paste(names(positions),collapse=", ")),call.=FALSE)
  if(!is.numeric(angle))
    stop("'angle' must be numeric",call.=FALSE)
  rads  = (-angle - positions[[ position ]])*pi/180
  hjust = 0.5*(1 - sin(rads))
  vjust = 0.5*(1 + cos(rads))
  element_text(size = size, angle=angle,vjust=vjust,hjust=hjust)
}



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


parishDataPrep <- function(x, thresholdAR){
  # Take parish data and make it usable to calculate probability of seeing an
  # outbreak given an index case
  x[, 3:5] <- lapply(x[, 3:5], as.numeric)
  
  out <- x %>%
    select(parish_sogn, Cases, deaths, pop) %>%
    filter(Cases >0 | deaths > 0) %>%
    mutate(AR = round(Cases / pop*100, digits = 1)) %>%
    mutate(outbreak = FALSE)
  
  
  out[["outbreak"]] <- out$AR > thresholdAR
  
  # Outbreak occured in Taarby 
  out[["outbreak"]][1] <- TRUE
  return(out)
}
