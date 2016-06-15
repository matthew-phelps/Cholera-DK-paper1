# Author: Matthew Phelps
# Desc: Comparing the small cities in DK
# Output datasets: Rdata in local directory


## Intro

ifelse(grepl("wrz741", getwd()),
       data.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\Cholera-DK-paper1",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1")
setwd(data.path)
rm(list = ls())
graphics.off()

devtools::install_github('matthew-phelps/CholeraDataDK', force = F)
library(ggplot2)
library(dplyr)
library(grid)
library(tidyr)
library(CholeraDataDK)
library(epitools)

cases <- case_records

nyk_1 <- cases[cases$location == "nykoebing", ]
fred_1 <- cases[cases$location == "frederikshavn", ]

nyk_2 <- data.frame(table(nyk_1$date_admission))
fred_2 <- data.frame(table(fred_1$date_admission))

# Convert back to date formate
nyk_2$Var1 <- as.Date(nyk_2$Var1)
fred_2$Var1 <- as.Date(fred_2$Var1)

# Order by date
nyk_3 <- nyk_2[order(nyk_2$Var1), ]
fred_3 <- fred_2[order(fred_2$Var1), ]

nyk_min <- min(nyk_3$Var1)
nyk_max <- max(nyk_3$Var1)
fred_min <- min(fred_3$Var1)
fred_max <- max(fred_3$Var1)

# Generate time seq of 1 day within interval
nyk_int <- seq(nyk_min, nyk_max, by = "day")
fred_int <- seq(fred_min, fred_max, by = "day")

nyk_int <- data.frame(Var1 = nyk_int)
fred_int <- data.frame(Var1 = fred_int)

nyk <- merge(nyk_int, nyk_3, all = T)
fred <- merge(fred_int, fred_3, all = T)

nyk$Freq[is.na(nyk$Freq)] <- 0
fred$Freq[is.na(fred$Freq)] <- 0

ggplot()
