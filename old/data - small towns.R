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

all_cases_temp <- cholera_daily_data
cases <- cholera_daily_data_towns



# RATES FOR SMALL TOWNS
pop <- dk_population
pop <- pop[!is.na(pop$year),]
pop <- pop[pop$year=="1853" | pop$year == "1857" | pop$city == "nykoebing", ]
colnames(cases) <- c("date", "cases", "day_index", "city")

cases$cases_norm[cases$city=="nykoebing"] <- cases$cases[cases$city=="nykoebing"] / pop$pop[pop$city == "nykoebing"]
cases$cases_norm[cases$city=="frederikshavn"] <- cases$cases[cases$city=="frederikshavn"] / pop$pop[pop$city == "frederikshavn"]


ggplot()
