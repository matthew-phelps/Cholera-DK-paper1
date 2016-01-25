# Author: Matthew Phelps
# Desc: Collect data from various files
# Output datasets: Rdata in local directory


## Intro
rm(list = ls())
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\Cholera-DK-paper1\\data",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/data")

setwd(data.path)

aalborg_day <- read.csv('Aalborg_daily_1853.csv') 
korsoer_day <- read.csv('Korsoer_daily_1857.csv')
cph_day <- read.csv('CPH_daily_1853.csv')


korsoer_day$month <- sprintf("%02d", korsoer_day$month) # pad with leading zeros
korsoer_day$date <- paste(korsoer_day$years, korsoer_day$month, korsoer_day$day, sep = "-")
korsoer_day$date <- as.Date(korsoer_day$date)
korsoer_day$years <- korsoer_day$month <- korsoer_day$day <- NULL

aalborg_day$month <- sprintf("%02d", aalborg_day$month) # pad with leading zeros
aalborg_day$date <- paste(aalborg_day$years, aalborg_day$month, aalborg_day$day, sep = "-")
aalborg_day$date <- as.Date(aalborg_day$date)
aalborg_day$years <- aalborg_day$month <- aalborg_day$day <- NULL

cph_day$month <- sprintf("%02d", cph_day$month) # pad with leading zeros
cph_day$date <- paste(cph_day$years, cph_day$month, cph_day$day, sep = "-")
cph_day$date <- as.Date(cph_day$date)
cph_day$years <- cph_day$month <- cph_day$day <- NULL

save(aalborg_day, cph_day, korsoer_day, file = "date2_cities_daily.Rdata")
