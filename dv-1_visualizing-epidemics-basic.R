# Author: Matthew Phelps
# Desc: Comparing the 3 cities' outbreals
# Output datasets: Rdata in local directory


## Intro
rm(list = ls())
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\Cholera-DK-paper1\\data",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/data")

ifelse(grepl("wrz741", getwd()),
       save.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\Cholera-DK-paper1\\Rdata\\Data-1_transfer-from-CPH-project",
       save.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/Rdata/Data-1_transfer-from-CPH-project")

setwd(data.path)


# LOAD --------------------------------------------------------------------

load("date2_cities_daily.Rdata")
