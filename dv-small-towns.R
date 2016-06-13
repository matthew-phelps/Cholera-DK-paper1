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

devtools::install_github('matthew-phelps/CholeraDataDK', force = T)
library(ggplot2)
library(dplyr)
library(grid)
library(tidyr)
library(CholeraDataDK)
library(epitools)