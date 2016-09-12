# Author: Matthew Phelps
# Desc: Table data for 3 towns



## Intro
rm(list = ls())
ifelse(grepl("wrz741", getwd()),
       main.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1",
       main.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1")

ifelse(grepl("wrz741", getwd()),
       data.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/data",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/data")
setwd(main.path)

graphics.off()

devtools::install_github('matthew-phelps/CholeraDataDK', force = F)
devtools::install_github('matthew-phelps/grouping', force = F)
library(ggplot2)
library(dplyr)
library(grid)
library(tidyr)
library(CholeraDataDK)
library(epitools)
library(grouping)
library(xlsx)
source("functions.R")



# LOAD --------------------------------------------------------------------
setwd(data.path)

load(file = "aal_age_pop.Rdata")
all_cases_temp <- cholera_daily_data
mort <- cph_mort_rates_10yr
counts <- cph_counts_age
cph_pop <- cph_pop1853_10yr
attack <- cph_age_attack_rate
cases <- cholera_daily_data_towns
cen <- census
cph_pop_raw <- cph_age_pop1853_raw
cph_mort_raw <- cph_age_sick_dead_raw

aal <- cen[cen$place=="aalborg" & cen$year == 1853, ]
aal <- sum(aal$total)
cph <- sum(cph_pop$total1853)
kor <- 2000

cph_sick <- sum(counts$male_sick) + sum(counts$female_sick)
cph_dead <- sum(counts$male_dead) + sum(counts$female_dead)


# Under 5 mortality -------------------------------------------------------


cph_pop_u5 <- group2(cph_pop_raw[1:3, ], 3, FUN = "sum", F)$total1853 # under 5 together
cph_dead_u5 <- group2(cph_mort_raw[1:3, ], 3, FUN = "sum", F)$total_dead # 5 - 9 yr olds
cph_sick_u5 <- group2(cph_mort_raw[1:3, ], 3, FUN = "sum", F)$total_sick # 5 - 9 yr olds

u5_mort_rate <- cph_dead_u5 / cph_pop_u5 * 100
u5_attack <- cph_sick_u5 / cph_pop_u5 * 100

u5 <- data.frame(outcome = c("attac", "mort"), 
                 counts = c(cph_sick_u5, cph_dead_u5),
                 pe = c(u5_attack, u5_mort_rate))

u5$lower95 <- ci.rate(100, cph_pop_u5, u5$counts, upper = F)
u5$upper95 <- ci.rate(100, cph_pop_u5, u5$counts, upper = T)

u5_mort_rate/u5_attack


# TOTAL INFECTIONS INCLUDING ASYPMTOMATIC ---------------------------------
cph <- 0.052
aal <- 0.088
kor <- 0.112

mid <- 0.242
low <- 0.407
hi <- 0.144


cph / mid
cph/low
cph/hi

aal / mid
aal/low
aal/hi

kor/mid
kor/low
kor/hi
5/10
5/9



# KORSOER SES CI ----------------------------------------------------------
setwd(main.path)
x <- read.xlsx2("output/Table 2 - SES Korsoer.xlsx", sheetIndex = 1,
                colClasses = c("character", "numeric","numeric", "numeric",
                               "numeric", "numeric", "numeric", "numeric",
                               "numeric"))
ci.rate(100, x$Population, num_cases = x$Number.of.cases, upper = T) /100
ci.rate(100, x$Population, num_cases = x$Number.of.cases, upper = F)/100

ci.rate(100, x$Population, num_cases = x$Number.of.deaths, upper = T) /100
ci.rate(100, x$Population, num_cases = x$Number.of.deaths, upper = F)/100


