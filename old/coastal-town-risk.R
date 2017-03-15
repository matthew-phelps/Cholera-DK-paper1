# Author: Matthew Phelps
# Desc: Comparing costal cities vs non-coastal cities for cholera in DK


## Intro
rm(list = ls())
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1")
ifelse(grepl("wrz741", getwd()),
       gis.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/Data/GIS",
       gis.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data/GIS")

setwd(data.path)

graphics.off()

# devtools::install_github('matthew-phelps/CholeraDataDK')
library(ggplot2)
library(rgdal)
library(tidyr)
library(CholeraDataDK)
library(fmsb) # for Odds Ratio

# LOAD --------------------------------------------------------------------
setwd(gis.path)
dk <- readOGR(dsn = ".", layer = "dk_towns_cholera")
towns <- dk@data

# Set towns with 1 cholera case (coded as: cholera == 3) to "no cholera"
towns$cholera <- ifelse(towns$cholera == 3 | towns$cholera == 0, 0, 1)

# # Set towns with "unkown" port status to have a port
towns$Port <- ifelse(towns$Port == 3, 1, 0)


# Remove towns with unknown "Port" status
towns <- towns[which(towns$Port != 3), 2:5]

# Subset that does not inlcude small villages:
large_towns <- towns[which(towns$size != 1), ]
table(large_towns[2:3])


# CREATE VARIABLE ---------------------------------------------------------
objt <- large_towns

sick_exp <- nrow(objt[which(objt$Port == 1 & objt$cholera == 1), ])
sick_not_exp <- nrow(objt[which(objt$Port == 0 & objt$cholera == 1), ])

pop_exp <- nrow(objt[which(objt$Port == 1), ])
pop_not_exp <- nrow(objt[which(objt$Port == 0), ])

not_sick_exp <- nrow(towns[which(towns$Port == 1 & towns$cholera == 0), ])
not_sick_not_exp <- nrow(towns[which(towns$Port == 0 & towns$cholera == 0), ])


# RISK RATIO with fmsb package --------------------------------------------------------------

fmsb::riskratio(sick_exp, sick_not_exp, pop_exp, pop_not_exp)

# RISK RATIO by "hand"--------------------------------------


(sick_exp / pop_exp) / (sick_not_exp / pop_not_exp)

#####
## NO ASSOCIATION BETWEEN PORT STATUS AND RISK OF CHOLERA ##
#####