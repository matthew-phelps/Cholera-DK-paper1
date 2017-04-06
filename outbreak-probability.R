# Author: Matthew Phelps
# Desc: Compare expected number of outbreaks to take hold against the actual number
rm(list = ls())
devtools::install_github('matthew-phelps/CholeraDataDK', force = F)
library(CholeraDataDK)
library(tidyverse)
source("functions.R")
load("data/r0.Rdata")
par_cases <- CholeraDataDK::parish_cases

par_cases <- parishDataPrep(par_cases, 0.5)

prob_data <- par_cases %>%
  filter(is.na(AR) & Cases > 10)

par_cases <- par_cases[!is.na(par_cases[["outbreak"]]),] 

prob_outbreak <- sum(par_cases[["outbreak"]]) / nrow(par_cases)

expProb <- function(r0) {
  1 - 1 / r0
}

r0 <- r0[r0[["method"]]!="TD", ]
r0_lng <- gather(r0, est, value, 1:3)
prob_expected <- expProb(r0_lng$value)

min(prob_expected)
max(prob_expected)

hist(prob_expected)
