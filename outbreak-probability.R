# Author: Matthew Phelps
# Desc: Compare expected number of outbreaks to take hold against the actual number
rm(list = ls())
devtools::install_github('matthew-phelps/CholeraDataDK', force = F)
library(CholeraDataDK)
library(tidyverse)
source("functions.R")
load("data/r0.Rdata")


par_cases <- CholeraDataDK::parish_cases
par_cases <- parishDataPrep(par_cases)
thresh <- seq(0.1, 5, length.out = 100)
obs_prob <- unlist(lapply(thresh, probOutbreak, par_cases))
prob_data <- par_cases %>%
  filter(is.na(AR) & Cases > 10)


expProb <- function(r0) {
  1 - 1 / r0
}

r0 <- r0[r0[["method"]]!="TD", ]
r0_vec <- seq(min(r0$ci_l), max(r0$ci_u), length.out = 100)
prob_expected <- expProb(r0_vec)
