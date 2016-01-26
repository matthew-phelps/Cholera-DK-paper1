# Author: Matthew Phelps
# Desc: Pull data from CPH project and store in local directory
# Output datasets: Rdata in local directory

## intro
rm(list = ls())
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\CPH",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH")

ifelse(grepl("wrz741", getwd()),
       save.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\Cholera-DK-paper1\\Rdata\\Data-1_transfer-from-CPH-project.Rdata",
       save.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/Rdata/Data-1_transfer-from-CPH-project.Rdata")

setwd(data.path)

load("Data\\Rdata\\all_age.Rdata")
load('Data\\Rdata\\age1855.Rdata')
load('Data\\Rdata\\age1850.Rdata')
load("Data\\Rdata\\quarter_combined.Rdata")

# Save in local directory
save(age1850, age1855, all_age, combined, file = save.path)
