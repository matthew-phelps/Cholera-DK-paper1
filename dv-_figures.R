# Author: Matthew Phelps
# Desc: Comparing the 3 cities' outbreals
# Output datasets: Rdata in local directory


## Intro

ifelse(grepl("wrz741", getwd()),
       data.path <- "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\Cholera-DK-paper1",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1")
setwd(data.path)
rm(list = ls())
graphics.off()

devtools::install_github('matthew-phelps/CholeraDataDK', force = T)
library(ggplot2)
library(dplyr)
library(grid)
library(tidyr)
library(CholeraDataDK)
library(epitools)

# LOAD --------------------------------------------------------------------
all_cases_temp <- cholera_daily_data
mort <- cph_mort_rates_10yr
counts <- cph_counts_age
pop <- cph_pop1853_10yr
attack <- cph_age_attack_rate
cases <- cholera_daily_data_towns
cen <- census


# Re-order levels of factor so that plotting works: http://goo.gl/CD2fEC
age_char <- as.character(mort$age_range)
mort$age_range <- factor(age_char, levels = c(age_char))

# To long format data
mort_gender <- gather(mort[, c("age_range", "male_mort2", "female_mort2")],
                      key = gender,
                      value = mort_rate,
                      c(male_mort2, female_mort2))


# 
# # AGE MORTALITY BY GENDER -------------------------------------------------------------------
# ggplot(data = mort_gender, aes(group = gender)) +
#   geom_bar(aes(x = age_range, y = mort_rate, fill = gender),
#            stat = "identity",
#            position = "dodge") +
#   xlab("Age group") +
#   ylab("Mortality rate") +
#   ggtitle ("How did mortality vary with age? \n") +
#   scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 0.4),
#         axis.text.y = element_text(size = 18),
#         axis.title.x = element_text(size = 22,
#                                     face = "bold",
#                                     vjust = -0.1),
#         axis.title.y = element_text(size = 22,
#                                     face = "bold",
#                                     vjust = 1.3),
#         plot.title = element_text(size = 28, face="bold"),
#         plot.margin = unit(c(0,0.5,0.5,0.3), 'lines'))
# 
# age_mortality_plot



# DEATH RELATIVE RISK -----------------------------------------------------------
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

# set limits for error bars: http://goo.gl/4QE74U
limits = aes(ymax = up95, ymin = low95)


title <- "Relative risk of cholera mortality"
sub_title <- "male is reference group"


plot_mort <- ggplot(data = rr,
       aes(x = age_range, y = rr)) +
  geom_point() +
  geom_errorbar(limits, width = 0.2) +
  geom_hline(yintercept = 1) +
  xlab("Age group") +
  ylab("Relative risks of death vs. male") +
  annotate("text", x = 7, y = 1.9, label = notes) +
  ggtitle (bquote(atop(.(title), atop(italic(.(sub_title)), ""))))  +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14, angle = 45, vjust = 0.4),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16,
                                    face = "bold",
                                    vjust = -0.1),
        axis.title.y = element_text(size = 18,
                                    face = "bold",
                                    vjust = 1.3),
        plot.title = element_text(size = 18, face="bold"))
plot_mort

ggsave(filename = "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\Cholera-DK-paper1\\Output\\F5-RR-mortality.jpg",
       plot = plot_mort,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 600)

# ATTACK RELATIVE RISK -----------------------------------------------------------
# Male is reference. If >1 risk is higher for females
rr <- matrix(NA, nrow = length(mort$age_range))
rr <- data.frame(rr)
rr$age_range <- mort$age_range
rr$rr <- attack$male_attack_rate / attack$female_attack_rate
m_d <- counts$male_sick
f_d <- counts$female_sick
m_pop <- pop$men1853
f_pop <- pop$women1853

# 95% CI: https://goo.gl/JE6C4t
se <- sqrt(1/m_d + 1/f_d - 1/m_pop - 1/f_pop)
rr$low95 <- exp(log(rr$rr) - z_crit * se)
rr$up95 <- exp(log(rr$rr) + z_crit * se)

# set limits for error bars: http://goo.gl/4QE74U
limits = aes(ymax = up95, ymin = low95)

title <- "How did attack rates vary between genders?"
sub_title <- "male is reference group"

plot_attack <- ggplot(data = rr,
       aes(x = age_range, y = rr)) +
  geom_point() +
  geom_errorbar(limits, width = 0.2) +
  geom_hline(yintercept = 1) +
  xlab("Age group") +
  ylab("Relative risk of infection vs. male") +
  annotate("text", x = 7, y = 1.229*max(rr$rr), label = notes) +
  ggtitle (bquote(atop(.(title), atop(italic(.(sub_title)), ""))))  +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16,
                                    face = "bold",
                                    vjust = -0.1),
        axis.title.y = element_text(size = 18,
                                    face = "bold",
                                    vjust = 1.3),
        plot.title = element_text(size = 18, face="bold"))
plot_attack

ggsave(filename = "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\Cholera-DK-paper1\\Output\\F6-RR-attack.jpg",
       plot = plot_attack,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 600)


# Cholera Deaths -------------------------------------------------
cho_pct <- matrix(nrow = nrow(counts))
cho_pct <- data.frame(cho_pct)
cho_pct[,1] <- counts$age_range 
colnames(cho_pct) <- "age_range" # rename var1
cho_pct$mortality <- (counts$male_dead2 + counts$female_dead2) / (counts$male_all_cause + counts$female_all_cause)

plot_chol_pct <- ggplot(data = cho_pct) +
  geom_line( aes(x = age_range, y = mortality, group = 1),
            size = 1.5) +
  geom_point( aes(x = age_range, y = mortality),
             size = 3.5) +
  xlab("Age group") +
  ylab("% of all deaths attributed to cholera") +
  ggtitle ("Portion of all mortality \ndue to cholera") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 16, angle = 45, vjust = 0.9),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18,
                                    face = "bold",
                                    vjust = 0),
        axis.title.y = element_text(size = 18,
                                    face = "bold"),
        plot.title = element_text(size = 18, face="bold"),
        plot.margin = unit(c(0,0,0.5,0.5), 'lines'))
plot_chol_pct


ggsave(filename = "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\Cholera-DK-paper1\\Output\\F7-cholera-of-total.jpg",
       plot = plot_chol_pct,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 600)


# Total mortality due to cholera ------------------------------------------
# DF with cholera mort as % of total mort & cholera mort rates in 1 df
chol_mort <- mort[, c("age_range", "total_mort_2")]
plot_chol_mort <- ggplot(data = chol_mort) +
  geom_line( aes(x = age_range, y = total_mort_2, group = 1),
             size = 1.5) +
  geom_point( aes(x = age_range, y = total_mort_2),
              size = 3.5) +
  xlab("Age group") +
  ylab("Mortality rate \n per 100 people") +
  ggtitle ("Cholera mortality rate \n") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 16, angle = 45, vjust = 0.9),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18,
                                    face = "bold",
                                    vjust = 0),
        axis.title.y = element_text(size = 18,
                                    face = "bold"),
        plot.title = element_text(size = 18, face="bold"),
        plot.margin = unit(c(0,0,0.5,0.5), 'lines'))
plot_chol_mort

ggsave(filename = "C:\\Users\\wrz741\\Google Drive\\Copenhagen\\DK Cholera\\Cholera-DK-paper1\\Output\\F8-cholera-mort-rate.jpg",
       plot = plot_chol_mort,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 600)

# Frederikshavn -----------------------------------------------------------
frederikshavn <- cases[cases$location == "frederikshavn",]
nyk <- cases[cases$location == "nykoebing",]
plot(frederikshavn$value/2000*10000, type = 'l')
plot(nyk$value, col="red", type ='l')


# Aalborg age-stratified mortality rates ----------------------------------

aal <- census[census$place == 'aalborg' & census$year == 1853, ]
