# Author: Matthew Phelps
# Desc: Estimating the duration of infectiousness based upon hospital admission
# data in Nykoebing

rm(list = ls())
ifelse(grepl("wrz741", getwd()),
       script.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1",
       script.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1")
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/Data",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data")

ifelse(grepl("wrz741", getwd()),
       check.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/Data/data to check",
       check.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data/data to check")


setwd(data.path)

graphics.off()
# devtools::install_github('matthew-phelps/grouping', force = T)
# devtools::install_github('matthew-phelps/CholeraDataDK')
library(dplyr)
library(ggplot2)
library(data.table)
library(Publish)
library(CholeraDataDK)
library(grouping)

# LOAD --------------------------------------------------------------------
aal <- aalborg_1850_census
all_cases_temp <- cholera_daily_data[cholera_daily_data$city == "aalborg",]
# RESHAPE -----------------------------------------------------------------

table(aal$age)

men <- tapply(aal$gender, aal$age, function(x){ sum(x=="M")}) # count men
cen <- data.frame(men)
cen$female <-  tapply(aal$gender, aal$age, function(x){ sum(x=="F")}) # count women
cen$age <- as.numeric(row.names(men))
age2 <- data.frame(age = as.numeric(1:99))


cen2 <- merge(cen, age2, by = "age", all = T) # merge to 1 df
cen2[is.na(cen2)] <- 0 # Rows with no counts set to 0
cen <- cen2 
rm(cen2) # remove temp obj

# Grouping by age groups

aal_cen <- group2(cen, 5, age_labels = T)
aal_cen <- select(aal_cen, c(-group, -age))
