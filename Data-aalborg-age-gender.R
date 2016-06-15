# Author: Matthew Phelps
# Desc: Calculate age-gender rates for Aalborg
# Output datasets: Rdata in local directory


## Intro

ifelse(grepl("wrz741", getwd()),
       data.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\Cholera-DK-paper1",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1")
setwd(data.path)
rm(list = ls())
graphics.off()

devtools::install_github('matthew-phelps/CholeraDataDK', force = T)
devtools::install_github('matthew-phelps/grouping', force = F)
library(ggplot2)
library(dplyr)
library(grid)
library(tidyr)
library(CholeraDataDK)
library(epitools)
library(grouping)


aal_pop <- aalborg_1850_census
aal_chol <- aalborg_age_gender



# GROUP CENSUS DATA -------------------------------------------------------
pop <- data.frame(table(aal_pop))
pop$age <- as.numeric(as.character(pop$age))
pop$age[pop$age=="98"] <- 94
pop2 <- pop[pop$age < 90, ]

pop_ls <- split(pop2, f = pop2$gender)
pop_grp <- lapply(pop_ls, group2, 10, age_labels = T)

pop3 <- do.call(rbind.data.frame, pop_grp)
pop3$gender[1:9] <- "f"
pop3$gender[10:18] <- "m"
pop3$gender[19:27] <- "u"
pop3$group <- pop3$age <- NULL
row.names(pop3) <- NULL
