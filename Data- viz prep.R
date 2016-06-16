# Author: Matthew Phelps
# Desc: Calculate age-gender rates for Aalborg
# Output datasets: Rdata in local directory


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


# LOAD --------------------------------------------------------------------
all_cases_temp <- cholera_daily_data
mort <- cph_mort_rates_10yr
counts <- cph_counts_age
pop <- cph_pop1853_10yr
attack <- cph_age_attack_rate
cases <- cholera_daily_data_towns
cen <- census

aal_pop <- aalborg_1850_census
aal_chol <- aalborg_age_gender




# AALBORG-GENDER-AGE ------------------------------------------------------
# Group census data
pop <- data.frame(table(aal_pop))
pop$age <- as.numeric(as.character(pop$age))
pop$age[pop$age=="98"] <- 94
pop2 <- pop[pop$age < 90, ]

# Split into gender groups and apply group2 fn to each gender
pop_ls <- split(pop2, f = pop2$gender)
pop_grp <- lapply(pop_ls, group2, 10, age_labels = T)

# Re-group into 1 df
pop3 <- do.call(rbind.data.frame, pop_grp)
pop3$gender[1:9] <- "f" # Re-label since labels got messed up in group2
pop3$gender[10:18] <- "m"
pop3$gender[19:27] <- "u"
pop3$group <- pop3$age <- NULL # Remove un-used variables
row.names(pop3) <- NULL

# Manually recode 80+ variable
pop3$labels[pop3$labels == "80 - 89"] <- "80+"
pop3$Freq[pop3$gender == "f" & pop3$labels=="80+"] <- 45 + 2
pop3$Freq[pop3$gender == "m" & pop3$labels=="80+"] <- 14 + 2
aal_age_pop <- pop3
# Save
setwd(data.path)
save(aal_age_pop, file = "aal_age_pop.Rdata")



# DEATH RR -----------------------------------------------------------------------

# Male is reference. If >1 risk is higher for females
rr <- matrix(NA, nrow = length(mort$age_range))
rr <- data.frame(rr)
rr$age_range <- mort$age_range
rr$rr <- mort$male_mort2 / mort$female_mort2

m_d <- counts$male_dead2
f_d <- counts$female_dead2
m_pop <- pop$men1853
f_pop <- pop$women1853

# Bonferonni correction for multiple testing: http://goo.gl/Drhjly (pdf)
alpha <- 0.05
n <- 9 # number of "test", in this case, age groups being compared
bonf_c <- alpha / n
z_crit <- qnorm(1 - (bonf_c/2)) # critical z-value from: http://goo.gl/BXDBFL
notes <- "*95%CI corrected for multiple tests\n using Bonferroni correction"
# 95% CI: https://goo.gl/JE6C4t
se <- sqrt(1/m_d + 1/f_d - 1/m_pop - 1/f_pop)
rr$low95 <- exp(log(rr$rr) - z_crit * se)
rr$up95 <- exp(log(rr$rr) + z_crit * se)
