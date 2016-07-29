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
source("functions.R")

# LOAD --------------------------------------------------------------------

counts <- cph_counts_age
cph_pop <- cph_pop1853_10yr
attack <- cph_age_attack_rate
cases <- cholera_daily_data_towns

aal_1850 <- aalborg_1850_census
aal_1855 <- aalborg_1855_census
aal_chol <- aalborg_age_gender




# MERGE AGE GROUPS - CPH & AALBORG -----------------------------------------------
# Merge the last two age groups into a 70+ category
aal_chol <- row.merge(aal_chol)
cph_pop <- row.merge(cph_pop)
counts <- row.merge(counts)




# AALBORG-POPULATION-GENDER-AGE ------------------------------------------------------

# Since largest age group == 70+, set everyone >79 to be 79 for ease of
# processing
aal_1850$age[aal_1850$age > 79] <- 79

# Group census data 1850
pop <- data.frame(table(aal_1850))
pop$age <- as.numeric(as.character(pop$age))

# Split into gender groups and apply group2 fn to each gender
pop_ls <- split(pop, f = pop$gender)
pop_grp <- lapply(pop_ls, group2, 10, age_labels = T)
rm(pop_ls)

# Re-group into 1 df
pop3 <- do.call(rbind.data.frame, pop_grp)
pop3$gender[pop3$gender==10] <- "f50" # Re-label since labels got messed up in group2
pop3$gender[pop3$gender==20] <- "m50"
pop3$gender[pop3$gender==30] <-"u50"
pop3$group <- pop3$age <- NULL # Remove un-used variables
row.names(pop3) <- NULL


aal_age_pop <- pop3
aal_age_pop <- spread(aal_age_pop, gender, Freq)
aal_age_pop$Total <- aal_age_pop$f + aal_age_pop$m + aal_age_pop$u
rm(pop, pop3, pop_grp)
aal_age_pop$year <- 1850 # add year for when datasets are merged



# Do the same for 1855 census
aal_1855$age[aal_1855$age > 79] <- 79
pop1855 <- data.frame(table(aal_1855))
pop1855 <- pop1855[complete.cases(pop1855),]

pop1855$age <- as.numeric(as.character(pop1855$age))

# Split into gender groups and apply group2 fn to each gender
pop1855_ls <- split(pop1855, f = pop1855$gender)
pop1855_grp <- lapply(pop1855_ls, group2, 10, age_labels = T)
rm(pop1855_ls)
# Re-group into 1 df
pop1855_3 <- do.call(rbind.data.frame, pop1855_grp)
pop1855_3$gender[pop1855_3$gender==10] <- "f55" # Re-label:group2() labeled wrong
pop1855_3$gender[pop1855_3$gender==20] <- "m55"
pop1855_3$gender[pop1855_3$gender==30] <- "u55"
pop1855_3$group <- pop1855_3$age <- NULL # Remove un-used variables
row.names(pop1855_3) <- NULL
colnames(pop1855_3) <- c("gender", "Freq", "lable1855") 

# Turn data to wide for easy reading 
aal_age_pop1855 <- pop1855_3
aal_age_pop1855 <- spread(aal_age_pop1855, gender, Freq)
aal_age_pop1855$total1855 <- aal_age_pop1855$f + aal_age_pop1855$m + aal_age_pop1855$u
rm(pop1855, pop1855_3, pop1855_grp, aal_1855, aal_1850)


# Impute 1853:
aal_comb <- cbind(aal_age_pop, aal_age_pop1855)
aal_comb$f1853 <- (aal_comb$f55 - aal_comb$f50) / 5 * 3 + aal_comb$f50
aal_comb$m1853 <- (aal_comb$m55 - aal_comb$m50) / 5 * 3 + aal_comb$m50
aal_comb$u1853 <- (aal_comb$u55 - aal_comb$u50) / 5 * 3 + aal_comb$u50
aal_comb$total1853 <- aal_comb$f1853 + aal_comb$m1853 + aal_comb$u1853
rm(aal_age_pop1855)
aal_age_pop <- dplyr::select(aal_comb, labels, f1853, m1853, u1853, total1853)
colnames(aal_age_pop) <- c("age_group","f", "m", "u", "Total")

# City-wide summations using my funtionc from "functions.R"

aal_age_pop <- citywide(x = aal_age_pop) 
aal_chol <- citywide(x = aal_chol)

# AALBORG MORT AND MORB RATES ---------------------------------------------

aal_chol$m_attck_rt <- aal_chol$male_sick / aal_age_pop$m
aal_chol$f_attck_rt <- aal_chol$female_sick / aal_age_pop$f
aal_chol$tot_attck_rt <- aal_chol$total_sick / aal_age_pop$Total

aal_chol$m_mort_rt <- aal_chol$male_dead / aal_age_pop$m
aal_chol$f_mort_rt <- aal_chol$female_dead / aal_age_pop$f
aal_chol$tot_mort_rt <- aal_chol$total_dead / aal_age_pop$Total



# BONFERONI CORRECTION Z-CRIT VALUE\ --------------------------------------
# Bonferonni correction for multiple testing: http://goo.gl/Drhjly (pdf)
alpha <- 0.05
n <- 9 # number of "test", in this case, age groups being compared
bonf_c <- alpha / n
z_crit <- qnorm(1 - (bonf_c/2)) # critical z-value from: http://goo.gl/BXDBFL




# CITYWIDE POPULATION, CASES AND DEATHS -----------------------------------
cph_pop <- citywide(cph_pop)
counts <- citywide(counts)



# CPH MORBIDITY & MORTALITY RATES -----------------------------------------
# This was calculated in CholeraDataDK, but need to tweek it
cph_chol <- data.frame(matrix(cph_pop$age_range, nrow = nrow(cph_pop)))
colnames(cph_chol) <- "age_range"
cph_chol$male_mort_rate <- counts$male_dead / cph_pop$men1853
cph_chol$female_mort_rate <- counts$female_dead / cph_pop$women1853
cph_chol$male_attack_rate <- counts$male_sick / cph_pop$men1853
cph_chol$female_attack_rate <- counts$female_sick / cph_pop$women1853
cph_chol$total_mort_rate <- counts$total_dead / cph_pop$total1853



# CPH DEATH RR -----------------------------------------------------------------------

# Male is reference. If >1 risk is higher for females
cph_rr_mrt <- matrix(NA, nrow = length(cph_chol$age_range))
cph_rr_mrt <- data.frame(cph_rr_mrt)
colnames(cph_rr_mrt) <- "rr"
cph_rr_mrt$age_range <- cph_pop$age_range
cph_rr_mrt$rr <- cph_chol$male_mort_rate / cph_chol$female_mort_rate

m_d <- counts$male_dead
f_d <- counts$female_dead
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







# CPH RR ATTACK RATE ---------------------------------------------------------

# Male is reference. If >1 risk is higher for females
cph_rr_sic <- matrix(NA, nrow = length(cph_chol$age_range))
cph_rr_sic <- data.frame(cph_rr_sic)
colnames(cph_rr_sic) <- "rr"
cph_rr_sic$age_range <- cph_chol$age_range
cph_rr_sic$rr <- cph_chol$male_attack_rate / cph_chol$female_attack_rate
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


rm(n, alpha, bonf_c, f_d, f_pop, m_d, m_pop, notes, se, z_crit, aal_rr_sic,
   aal_rr_mrt, aal_comb, cases, cph_rr_sic, cph_rr_mrt)






# AGE-STRAITIFED MORT MORB --------------------------------------------
# CPH
counts$total_sick <- counts$male_sick + counts$female_sick
chol_burden <- cph_chol[, c("age_range", "total_mort_rate")]
chol_burden$total_attack <- counts$total_sick / cph_pop$total1853 * 100
chol_burden$total_mort_rate <- chol_burden$total_mort_rate * 100
chol_burden$city <- "cph"

# Aalborg
aal_burden <- aal_chol[, c("age_group", "tot_mort_rt", "tot_attck_rt")]
aal_burden$city <- "aalborg"
aal_burden$tot_mort_rt <- aal_burden$tot_mort_rt * 100
aal_burden$tot_attck_rt <- aal_burden$tot_attck_rt * 100
colnames(aal_burden) <- c(colnames(chol_burden))

# Calculate 95% CI

# Bind CPH and Aalborg together so can do 95%CI vectorized on 1 df
aal_burden$pop <- aal_age_pop$Total
aal_burden$num_dead <- aal_chol$total_dead
aal_burden$num_sick <- aal_chol$total_sick
chol_burden$pop <- cph_pop$total1853
chol_burden$num_dead <- counts$total_dead
chol_burden$num_sick <- counts$total_sick

chol_burden <- rbind(chol_burden, aal_burden)
chol_burden <- gather(chol_burden, outcome, pe, c(2,3))


# 95% CI based on pdf here: http://goo.gl/lKozQq (pdf)
chol_burden$lower95  <- ifelse(chol_burden$outcome=="total_mort_rate",
                               ci.rate(rate_unit = 100, pop = chol_burden$pop,
                                       num_cases =  chol_burden$num_dead, upper = F),
                               ci.rate(rate_unit = 100, pop = chol_burden$pop,
                                       num_cases =  chol_burden$num_sick, upper = F))

chol_burden$upper95  <- ifelse(chol_burden$outcome=="total_mort_rate",
                               ci.rate(rate_unit = 100, pop = chol_burden$pop,
                                       num_cases =  chol_burden$num_dead, upper = T),
                               ci.rate(rate_unit = 100, pop = chol_burden$pop,
                                       num_cases =  chol_burden$num_sick, upper = T))


chol_burden <- chol_burden[order(chol_burden$city, chol_burden$outcome), ]
rownames(chol_burden) <- NULL
chol_burden$num_dead <- chol_burden$num_sick <- chol_burden$pop <- NULL

chol_burden$plot_var <- interaction(chol_burden$city, chol_burden$outcome, lex.order = T)


# SAVE --------------------------------------------------------------------
setwd(data.path)
save(chol_burden, aal_age_pop, rr_mrt, rr_sic, counts,
     file = "data-viz-prep.Rdata")
