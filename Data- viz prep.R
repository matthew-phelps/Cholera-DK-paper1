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
cph_pop <- cph_pop1853_10yr
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

# Remove oldest age group because 
aal_age_pop <- pop3
aal_age_pop <- spread(aal_age_pop, gender, Freq)
aal_age_pop$total <- aal_age_pop$f + aal_age_pop$m + aal_age_pop$u
rm(pop, pop2, pop3, pop_grp, pop_ls)

# Remove oldest age group - there's not enough data = huge 95%CI
aal_age_pop <- aal_age_pop[aal_age_pop$labels != "80 - 89", ]
aal_chol <- aal_chol[aal_chol$age_group != "80+", ]

# Save
setwd(data.path)
save(aal_age_pop, file = "aal_age_pop.Rdata")


# AALBORG MORT AND MORB RATES ---------------------------------------------

aal_chol$m_attck_rt <- aal_chol$male_sick / aal_age_pop$m
aal_chol$f_attck_rt <- aal_chol$female_sick / aal_age_pop$f
aal_chol$tot_attck_rt <- aal_chol$total_sick / aal_age_pop$total

aal_chol$m_mort_rt <- aal_chol$male_dead / aal_age_pop$m
aal_chol$f_mort_rt <- aal_chol$female_dead / aal_age_pop$f
aal_chol$tot_mort_rt <- aal_chol$total_dead / aal_age_pop$total



# BONFERONI CORRECTION Z-CRIT VALUE\ --------------------------------------
# Bonferonni correction for multiple testing: http://goo.gl/Drhjly (pdf)
alpha <- 0.05
n <- 9 # number of "test", in this case, age groups being compared
bonf_c <- alpha / n
z_crit <- qnorm(1 - (bonf_c/2)) # critical z-value from: http://goo.gl/BXDBFL



# CPH DEATH RR -----------------------------------------------------------------------

# Male is reference. If >1 risk is higher for females
cph_rr_mrt <- matrix(NA, nrow = length(mort$age_range))
cph_rr_mrt <- data.frame(cph_rr_mrt)
colnames(cph_rr_mrt) <- "rr"
cph_rr_mrt$age_range <- mort$age_range
cph_rr_mrt$rr <- mort$male_mort2 / mort$female_mort2

m_d <- counts$male_dead2
f_d <- counts$female_dead2
m_pop <- cph_pop$men1853
f_pop <- cph_pop$women1853



# 95% CI: https://goo.gl/JE6C4t
se <- sqrt(1/m_d + 1/f_d - 1/m_pop - 1/f_pop)
cph_rr_mrt$low95 <- exp(log(cph_rr_mrt$rr) - z_crit * se)
cph_rr_mrt$up95 <- exp(log(cph_rr_mrt$rr) + z_crit * se)



# AALBORG DEATH RR --------------------------------------------------------

aal_rr_mrt <- matrix(NA, nrow = length(aal_chol$age_group))
aal_rr_mrt <- data.frame(aal_rr_mrt)
colnames(aal_rr_mrt) <- "rr"
aal_rr_mrt$age_range <- aal_chol$age_group
aal_rr_mrt$rr <- aal_chol$m_mort_rt / aal_chol$f_mort_rt

m_d <- aal_chol$male_dead
f_d <- aal_chol$female_dead
m_pop <- aal_age_pop$m
f_pop <- aal_age_pop$f

# Bonferonni correction for multiple testing: http://goo.gl/Drhjly (pdf)
alpha <- 0.05
n <- 9 # number of "test", in this case, age groups being compared
bonf_c <- alpha / n
z_crit <- qnorm(1 - (bonf_c/2)) # critical z-value from: http://goo.gl/BXDBFL
notes <- "*95%CI corrected for multiple tests\n using Bonferroni correction"
# 95% CI: https://goo.gl/JE6C4t
se <- sqrt(1/m_d + 1/f_d - 1/m_pop - 1/f_pop)
aal_rr_mrt$low95 <- exp(log(aal_rr_mrt$rr) - z_crit * se)
aal_rr_mrt$up95 <- exp(log(aal_rr_mrt$rr) + z_crit * se)


# COMBINE RR_MRT ----------------------------------------------------------
aal_rr_mrt$city <- "aalborg"
cph_rr_mrt$city <- "cph"
rr_mrt <- rbind(cph_rr_mrt, aal_rr_mrt)
save(rr_mrt, file = "rr_mrt.Rdata")






# CPH RR ATTACK RATE ---------------------------------------------------------

# Male is reference. If >1 risk is higher for females
cph_rr_sic <- matrix(NA, nrow = length(mort$age_range))
cph_rr_sic <- data.frame(cph_rr_sic)
colnames(cph_rr_sic) <- "rr"
cph_rr_sic$age_range <- mort$age_range
cph_rr_sic$rr <- attack$male_attack_rate / attack$female_attack_rate
m_d <- counts$male_sick
f_d <- counts$female_sick
m_pop <- cph_pop$men1853
f_pop <- cph_pop$women1853

# 95% CI: https://goo.gl/JE6C4t
se <- sqrt(1/m_d + 1/f_d - 1/m_pop - 1/f_pop)
cph_rr_sic$low95 <- exp(log(cph_rr_sic$rr) - z_crit * se)
cph_rr_sic$up95 <- exp(log(cph_rr_sic$rr) + z_crit * se)



# AALBORG RR ATTACK RATE --------------------------------------------------

aal_rr_sic <- matrix(NA, nrow = length(aal_chol$age_group))
aal_rr_sic <- data.frame(aal_rr_sic)
colnames(aal_rr_sic) <- "rr"
aal_rr_sic$age_range <- aal_chol$age_group
aal_rr_sic$rr <- aal_chol$m_attck_rt / aal_chol$f_attck_rt

m_d <- aal_chol$male_sick
f_d <- aal_chol$female_sick
m_pop <- aal_age_pop$m
f_pop <- aal_age_pop$f
# 95% CI: https://goo.gl/JE6C4t
se <- sqrt(1/m_d + 1/f_d - 1/m_pop - 1/f_pop)
aal_rr_sic$low95 <- exp(log(aal_rr_sic$rr) - z_crit * se)
aal_rr_sic$up95 <- exp(log(aal_rr_sic$rr) + z_crit * se)



# COMBINE RR_SIC ----------------------------------------------------------
aal_rr_sic$city <- "aalborg"
cph_rr_sic$city <- "cph"

rr_sic <- rbind(cph_rr_sic, aal_rr_sic)
save(rr_sic, file = "rr_sic.Rdata")
