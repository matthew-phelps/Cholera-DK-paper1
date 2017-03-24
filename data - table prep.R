# Author: Matthew Phelps
# Desc: Table data for 3 towns



## Intro
rm(list = ls())
graphics.off()

devtools::install_github('matthew-phelps/CholeraDataDK', force = F)
devtools::install_github('matthew-phelps/grouping', force = F)
library(tidyverse)
library(grid)
library(CholeraDataDK)
library(epitools)
library(grouping)
library(xlsx)
library(pander)
library(googlesheets) # for spreadsheets into R. https://goo.gl/05US08
source("functions.R")

# LOAD --------------------------------------------------------------------

load("data/data-viz-prep.Rdata")
load(file = "data/aal_age_pop.Rdata")
load("data/r0.Rdata")
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

# PERCENTAGE OF CASES OVER 5 ----------------------------------------------

r <- nrow(cph_mort_raw)
cph_sick_tot <- sum(cph_mort_raw$total_sick)

cph_sick_u5 / cph_sick_tot

# Population % over 14 (to compare with Haiti data from world bank)
cph_pop_u14 <- sum(cph_pop_raw$total1853[1:6])
cph_pop_tot <- sum(cph_pop_raw$total1853)
cph_pop_u14 / cph_pop_tot

# # TOTAL INFECTIONS INCLUDING ASYPMTOMATIC ---------------------------------
# cph <- 0.052
# aal <- 0.088
# kor <- 0.112
# 
# mid <- 0.242
# low <- 0.407
# hi <- 0.144
# 
# 
# cph / mid
# cph/low
# cph/hi
# 
# aal / mid
# aal/low
# aal/hi
# 
# kor/mid
# kor/low
# kor/hi
# 5/10
# 5/9



# KORSOER SES CI ----------------------------------------------------------

z <- gs_ls()
x1 <- gs_title("Table 2 SES Korsoer")
gs_ws_ls(x1)
x <- x1 %>%
  gs_read(ws = "Sheet1")
x$`Population` <- as.integer(x$`Population`)
# 
# setwd(main.path)
# x <- read.xlsx2("output/Table 2 - SES Korsoer.xlsx", sheetIndex = 1,
#                 colClasses = c("character", "numeric","numeric", "numeric",
#                                "numeric", "numeric", "numeric", "numeric",
#                                "numeric"))
ci.rate(100, pop = x$`Population`, num_cases = x$`Number of cases`, upper = T) /100
ci.rate(100, x$Population, num_cases = x$`Number of cases`, upper = F)/100

ci.rate(100, x$Population, num_cases = x$`Number of deaths`, upper = T) /100
ci.rate(100, x$Population, num_cases = x$`Number of deaths`, upper = F)/100

# How do I re-create the data?? What model do I use?
x <- 0.08
z <- rnorm(1000, mean = x)
quantile(z, c(0.05, 0.95))



# OR confidence interval of infection: https://goo.gl/Wu3o4j
a <- 74 # exposed group
c <- 58 # control group
b <- (359-a)
d <- (717-c)

or_sick <- (a*d)/(b*c)

se_ln <- sqrt(1/a + 1/b + 1/c + 1/d)
exp(log(or) - 1.96*se_ln)
exp(log(or) + 1.96*se_ln)


# OR confidence interval of mortality
a <- 51
c <- 35
b <- 359-a
d <- 717-c

or_death <- (a*d)/(b*c)
or_death

se_ln_death <- sqrt(1/a + 1/b + 1/c + 1/d)
exp(log(or_death) - 1.96*se_ln)
exp(log(or_death) + 1.96*se_ln)



# CI AROUND CUM CASE AND CUM MORT -----------------------------------------

x1 <- gs_title("Table 1 - Summary statistics")
gs_ws_ls(x1)
x <- x1 %>%
  gs_read(ws = "Sheet1")

x$Copenhagen[2]
x$Copenhagen[3]

# high
cph_h <- ci.rate(100, 138030, num_cases = 7219, upper = TRUE) / 100
cph_l <- ci.rate(100, 138030, num_cases = 7219, upper = FALSE) / 100

x$Aalborg[2]
x$Aalborg[3]
aal_h <- ci.rate(100, 8621, num_cases = 759, upper = TRUE) / 100
aal_l <- ci.rate(100, 8621, num_cases = 759, upper = FALSE) / 100
aal_h
aal_l

x$Korsør[2]
x$Korsør[3]
k_h <- ci.rate(100, 2258, num_cases = 294, upper = TRUE) / 100
k_l <- ci.rate(100, 2258, num_cases = 294, upper = FALSE) / 100

k_h
k_l



# CI AROUND CFR -----------------------------------------------------------
# We use the number infected as the "population" data here 

x$Copenhagen[3] # the "population"
x$Copenhagen[5]


cph_h <- ci.rate(100, 7219, num_cases = 4737, upper = TRUE) / 100
cph_l <- ci.rate(100, 7219, num_cases = 4737, upper = FALSE) / 100

x$Aalborg[3]
x$Aalborg[5]
aal_h <- ci.rate(100, 759, num_cases = 409, upper = TRUE) / 100
aal_l <- ci.rate(100, 759, num_cases = 409, upper = FALSE) / 100
aal_h
aal_l

x$Korsør[3]
x$Korsør[5]
k_h <- ci.rate(100, 294, num_cases = 201, upper = TRUE) / 100
k_l <- ci.rate(100, 294, num_cases = 201, upper = FALSE) / 100

k_h
k_l




# CI (AR & CFR) FOR OTHER CITIES  -----------------------------------------
# Oslo AR
ci.rate(100, pop = 48000, num_cases = 2453, upper = TRUE)
ci.rate(100, pop = 48000, num_cases = 2453, upper = FALSE)

# Oslo CFR
ci.rate(100, pop = 2453, num_cases = 1597, upper = TRUE)
ci.rate(100, pop = 2453, num_cases = 1597, upper = FALSE)

# Stokholm AR
ci.rate(100, pop = 97952, num_cases = 7906, upper = TRUE)
ci.rate(100, pop = 97952, num_cases = 7906, upper = FALSE)
7906/97952*100

# Stokholm CFR
3284 / 7906*100

ci.rate(100, pop = 7906, num_cases = 3284, upper = TRUE)
ci.rate(100, pop = 7906, num_cases = 3284, upper = FALSE)


# EXCSESS DEATHS ----------------------------------------------------------
# Subset data to calculate baseline and outbreak mortality over epidemic time of
# year. Find mean mortality for epi period in 1852 & 1854, then mean mort
d1 <- all_monthly_mort[all_monthly_mort$age == "total", ]

d52 <- dplyr::filter(d1, date >= "1852-06-01" & date <= "1852-09-01")
d53 <- dplyr::filter(d1, date >= "1853-06-01" & date <= "1853-09-01")
d54 <- dplyr::filter(d1, date >= "1854-06-01" & date <= "1854-09-01")
sum(d53$mortality)
# 1st find baseline mean mortality per month
base <- rbind(d52, d54)
cph_base <- sum(base[base$area=="Copenhagen",]$mortality) / 2


# 2nd find mean monthly mortality during outbreak
exc <- sum(d53[d53$area=="Copenhagen", ]$mortality)

# Difference in mortality between off and on years
excess_mort <- exc - cph_base

# Excess killed in terms of percentage of population
excess_mort / sum(cph_pop$total1853)


# Excess deaths in Haiti from: "Mortality Rates during Cholera Epidemic, Haiti,
# 2010–2011"
# Using Gonaives study site from paper:
pop <- 2.28725e5
ex_death <- 1028
ex_death_hi <- 1567
ex_death_lo <- 606

ex_death/pop * 100
ex_death_hi / pop * 100
ex_death_lo / pop * 100


# Population of port-au-prince in 2015 from pdf : https://goo.gl/1kXVkY

port_pop <- 987310 + 395260 + 265072 + 130283 + 511345 + 376834 +57434
port_cases <- 164423
port_dead <- 1065

port_cases / port_pop * 100


# U5 PORT AU PRINCE ATTACK RATE -------------------------------------------

u5_pp <- 25518
609 / (sum(counts$male_sick) + sum(counts$female_sick))



# R0 values ---------------------------------------------------------------

pander(r0 %>%
  filter(method=="EG"))
