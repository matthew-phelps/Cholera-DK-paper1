# Author: Matthew Phelps
# Desc: Collect data from various files
# Output datasets: Rdata in local directory


## Intro
rm(list = ls())
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\Cholera-DK-paper1",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1a")

setwd(data.path)

aalborg_day <- read.csv('data\\Aalborg_daily_1853.csv') 
cph_day <- read.csv('data\\CPH_daily_1853.csv')
korsoer_day <- read.csv('data\\Korsoer_daily_1857.csv')


# Population numbers from Lone's excel: "Cholera daily cases in 3 cities.xlsx
aalborg_pop <- 7745
cph_pop <- 143591
korsoer_pop <- 2236 



# DATE PREPERATION --------------------------------------------------------

aalborg_day$month <- sprintf("%02d", aalborg_day$month) # pad with leading zeros
aalborg_day$date <- paste(aalborg_day$years, aalborg_day$month, aalborg_day$day, sep = "-")
aalborg_day$date <- as.Date(aalborg_day$date)
aalborg_day$city <- "aalborg"
aalborg_day$years <- aalborg_day$month <- aalborg_day$day <- NULL

cph_day$month <- sprintf("%02d", cph_day$month) # pad with leading zeros
cph_day$date <- paste(cph_day$years, cph_day$month, cph_day$day, sep = "-")
cph_day$date <- as.Date(cph_day$date)
cph_day$city <- "cph"
cph_day$years <- cph_day$month <- cph_day$day <- NULL

korsoer_day$month <- sprintf("%02d", korsoer_day$month) # pad with leading zeros
korsoer_day$date <- paste(korsoer_day$years, korsoer_day$month, korsoer_day$day, sep = "-")
korsoer_day$date <- as.Date(korsoer_day$date)
korsoer_day$city <- "korsoer"
korsoer_day$years <- korsoer_day$month <- korsoer_day$day <- NULL



# NORMALIZING DATA --------------------------------------------------------

aalborg_day$cases_norm <- aalborg_day$cases/aalborg_pop * 10000
aalborg_day$deaths_norm <- aalborg_day$deaths/aalborg_pop * 10000

cph_day$cases_norm <- cph_day$cases/cph_pop * 10000
cph_day$deaths_norm <- cph_day$deaths/cph_pop * 10000

korsoer_day$cases_norm <- korsoer_day$cases/korsoer_pop * 10000
korsoer_day$deaths_norm <- korsoer_day$deaths/korsoer_pop * 10000



# SAVE --------------------------------------------------------------------

save(aalborg_day, cph_day, korsoer_day, file = "Rdata\\Data-2_cities-daily.Rdata")

