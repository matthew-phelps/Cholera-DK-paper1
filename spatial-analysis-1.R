# Author: Matthew Phelps
# Desc: Regerssion for coastal towns

rm(list = ls())
ifelse(grepl("wrz741", getwd()),
       data.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1")

ifelse(grepl("wrz741", getwd()),
       gis.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/CPH/Data/GIS",
       gis.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/CPH/Data/GIS")
setwd(gis.path)
graphics.off()

library('rgdal')
library(dplyr)
# LOAD --------------------------------------------------------------------

neg1 <- readOGR(".", "cholera_negative_dkcoastline")
pos1 <- readOGR(".", "cholera_positive_dkcoastline")
pos <- pos1@data
neg <- neg1@data
rm(pos1, neg1)

pos <- dplyr::rename(pos, dist_to_pos = pos_to_p_1)
neg <- dplyr::rename(neg, dist_to_pos = neg_to_p_1)
pos <- dplyr::select(pos, -pos_to_pos, -join_FID)
neg <- dplyr::select(neg, -neg_to_pos, -join_FID)

# Recode cholera levels to binary
neg$cholera <- 0
pos$cholera <- 1

# Convert to km
neg$dist_to_pos <- neg$dist_to_pos / 1000
pos$dist_to_pos <- pos$dist_to_pos / 1000

neg$distance <- neg$distance / 1000
pos$distance <- pos$distance / 1000

all <- rbind(neg, pos)



# EXPLORE DATA ------------------------------------------------------------

hist(all$distance)
hist(all$dist_to_pos)
hist(neg$dist_to_pos)
hist(pos$dist_to_pos)
median(neg$dist_to_pos)
median(pos$dist_to_pos)



# GROUP PREDICTORS --------------------------------------------------------

all$coast <- ifelse(all$distance < 5, 1, 0)


# REGRESSION --------------------------------------------------------------

x <- glm(all$cholera ~ all$coast + log(all$dist_to_pos) + all$size, family = binomial)

summary(x)
plot(x)
  
