# Author: Matthew Phelps
# Desc: Comparing the 3 cities' outbreals
# Output datasets: Rdata in local directory


## Intro
rm(list = ls())
ifelse(grepl("wrz741", getwd()),
       main.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1",
       main.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1")

ifelse(grepl("wrz741", getwd()),
       data.path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/data",
       data.path <-"/Users/Matthew/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/data")
setwd(data.path)

graphics.off()

devtools::install_github('matthew-phelps/CholeraDataDK', force = F)
library(ggplot2)
library(dplyr)
library(grid)
library(tidyr)
library(CholeraDataDK)
library(epitools)
library(scales) # required for some ggplot functions

# LOAD --------------------------------------------------------------------

load("data-viz-prep.Rdata")
all_cases_temp <- cholera_daily_data
mort <- cph_mort_rates_10yr
pop <- cph_pop1853_10yr
attack <- cph_age_attack_rate
cases <- cholera_daily_data_towns
cen <- census
cph_allcause <- CPH_allcause
dk_allcause <- dk_cities_allcause
rownames(cen) <- NULL

# Re-order levels of factor so that plotting works: http://goo.gl/CD2fEC
age_char <- as.character(mort$age_range)
mort$age_range <- factor(age_char, levels = c(age_char))

# To long format data
mort_gender <- gather(mort[, c("age_range", "male_mort2", "female_mort2")],
                      key = gender,
                      value = mort_rate,
                      c(male_mort2, female_mort2))


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

# set limits for error bars: http://goo.gl/4QE74U
limits = aes(ymax = up95, ymin = low95, x = age_range, color = city)
notes <- "*95%CI corrected for multiple tests\n using Bonferroni correction"
title <- "How mortality due to cholera varried by gender"
sub_title <- "male is reference group"
pd <- position_dodge(0.4)

plot_mort <- ggplot() +
  geom_point(data = rr_mrt[rr_mrt$age_range != "Total", ],
             aes(x = age_range,
                 y = rr,
                 color = city,
                 shape = city),
             position = pd, size = 2.2) +
  geom_errorbar(data = rr_mrt[rr_mrt$age_range != "Total", ],
                limits,
                width = 0.3,
                size = 0.8,
                position = pd) +
  # Separate series for the "Total" RR so that it can have different styling
  geom_point(data = rr_mrt[rr_mrt$age_range == "Total", ],
             aes(x = age_range,
                 y = rr,
                 color = city,
                 shape = city),
             position = pd, size = 3.0) +
  geom_errorbar(data = rr_mrt[rr_mrt$age_range == "Total", ],
                limits,
                width = 0.4,
                size = 1.3,
                position = pd) +
  coord_cartesian(ylim = c(0, 4)) +
  scale_color_manual(values = c("orange", "royalblue2"),
                     breaks = c('aalborg', 'cph'),
                     labels = c('Aalborg', 'Copenhagen')) +
  scale_shape_manual(values = c(19, 17),
                     breaks = c('aalborg', 'cph'),
                     labels = c('Aalborg', 'Copenhagen')) +
  geom_hline(yintercept = 1) +
  xlab("Age group") +
  ylab("Relative risk of cholera death") +
  annotate("text", x = 7, y = 3.8, label = notes) +
  ggtitle (bquote(atop(.(title), atop(italic(.(sub_title)), ""))))  +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = c(x = 0.7, y = .8),
        axis.text.x = element_text(size = 14, angle = 45, vjust = 0.4),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16,
                                    face = "bold",
                                    vjust = -0.1),
        axis.title.y = element_text(size = 16,
                                    face = "bold",
                                    vjust = 1.3),
        plot.title = element_text(size = 16, face="bold"))
plot_mort

ggsave(filename = "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/Output/F5-RR-mortality.jpg",
       plot = plot_mort,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 600)



# CITY-WIDE GENDER ATTACK RELATIVE RISK  --------------------------------------------------------

fem_chol <- sum(counts$female_sick)
men_chol <- sum(counts$male_sick)
fem_pop <- sum(pop$women1853)
men_pop <- sum(pop$men1853)

fem_risk <- fem_chol / fem_pop
men_risk <- men_chol / men_pop


rr_gender <- fem_risk / men_risk

# 95% CI: https://goo.gl/JE6C4t
se <- sqrt(1/fem_chol + 1/men_chol - 1/men_pop - 1/fem_pop)
rr_gender_l <- exp(log(rr_gender) - 1.96 * se)
rr_gender_u <- exp(log(rr_gender) + 1.96 * se)

# CITY-WIDE GENDER ATTACK RELATIVE RISK  -----------------------------------------------------
# 
# # Attach city-wide data:
# rr <- rbind(c(rr_gender, rr_gender_l, rr_gender_u), rr)
# rr$age_range <- factor(c("Total", age_char))
# rr$age_range <- relevel(rr$age_range, "Total")
# rr$plotting_var <- factor(c(0,1))
# rr$plotting_var[2:10] <- factor(0)
# rr$plotting_var[1] <- factor(1)
# set limits for error bars: http://goo.gl/4QE74U

limits = aes(ymax = up95, ymin = low95, color = city, x = age_range)
title <- "How attack rates varried between genders"
sub_title <- "male is reference group"
notes <- "*95%CI corrected for multiple tests\n using Bonferroni correction"


plot_attack <- ggplot() +
  geom_point(data = rr_sic[rr_sic$age_range !="Total", ],
             aes(x = age_range,
                 y = rr,
                 color = city,
                 shape = city),
             position = pd,
             size = 2.3) +
  
  geom_errorbar(data = rr_sic[rr_sic$age_range !="Total", ],
                limits,
                width = 0.3,
                size = 0.8,
                position = pd) +
  # Separate series for the "Total" RR so that it can have different styling
  geom_point(data = rr_sic[rr_sic$age_range =="Total", ],
             aes(x = age_range,
                 y = rr,
                 color = city,
                 shape = city),
             position = pd,
             size = 3.0) +
  geom_errorbar(data = rr_sic[rr_sic$age_range =="Total", ],
                limits,
                width = 0.4,
                size = 1.3,
                position = pd) +
  coord_cartesian(ylim = c(0, 4)) +
  scale_color_manual(values = c("orange", "royalblue2"),
                     breaks = c('aalborg', 'cph'),
                     labels = c('Aalborg', 'Copenhagen')) +
  scale_shape_manual(values = c(19, 17),
                     breaks = c('aalborg', 'cph'),
                     labels = c('Aalborg', 'Copenhagen')) +
  geom_hline(yintercept = 1) +
  xlab("Age group") +
  ylab("Relative risk of infection") +
  annotate("text", x = 7, y = 3.8, label = notes) +
  ggtitle (bquote(atop(.(title), atop(italic(.(sub_title)), ""))))  +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = c(x = 0.7, y = .80),
        axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16,
                                    face = "bold",
                                    vjust = -0.1),
        axis.title.y = element_text(size = 18,
                                    face = "bold",
                                    vjust = 1.3),
        plot.title = element_text(size = 18, face="bold"))
plot_attack

ggsave(filename = "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/Output/F6-RR-attack.jpg",
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

plot_chol_pct <- ggplot() +
  geom_line(data = cho_pct[1:8, ], aes(x = age_range, y = mortality, group = 1),
            size = 1.5) +
  geom_point(data = cho_pct[1:8, ], aes(x = age_range, y = mortality),
             size = 3.5) +
  geom_point(data = cho_pct[9, ], aes(x = age_range, y = mortality),
             size = 3.5, color = "dark red") +
  xlab("Age group") +
  ylab("Proportion of all 1853 deaths\n attributed to cholera") +
  ggtitle ("Middle-aged ") +
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


ggsave(filename = "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/Output/F7-cholera-of-total.jpg",
       plot = plot_chol_pct,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 600)




# Total mortality & total attack rate due to cholera -----------------------------------------
# DF with cholera mort as % of total mort & cholera mort rates in 1 df
chol_burden$lower95

limits <- aes(ymax = upper95, ymin = lower95,
              x= age_range, color = plot_var)
dodge <- position_dodge(width=- 0.4)

plot_chol_mort <- ggplot() +
  # Style the age 0 - 70 +. Will style the "total" group separately
  geom_line(data = chol_burden[chol_burden$age_range != "Total", ],
            position = dodge,
            aes(x = age_range, y = pe, group = plot_var, color = plot_var,
                linetype = plot_var), size = 0.9, alpha = 0.5) +
  geom_point(data = chol_burden[chol_burden$age_range != "Total", ], 
             position = dodge,
             aes(x = age_range, y = pe, group = plot_var, color = plot_var,
                 shape = plot_var),  size = 2.8) +
  geom_errorbar(data = chol_burden[chol_burden$age_range != "Total", ],
                limits, position = dodge, width = 0.2) +
  
  # Same but only for the "Total" group
  geom_point(data = chol_burden[chol_burden$age_range == "Total", ], 
             position = dodge,
             aes(x = age_range, y = pe, group = plot_var, color = plot_var,
                 shape = plot_var),  size = 3.5) +
  geom_errorbar(data = chol_burden[chol_burden$age_range == "Total", ],
                limits, position = dodge, size = 1.0, width = 0.4) +
  
  # Create symbology
  scale_color_manual(values = c("red2",  "red4",
                                "steelblue1" , "royalblue4"),
                     breaks = c('aalborg.total_attack', 'aalborg.total_mort_rate',
                                'cph.total_attack', 'cph.total_mort_rate'),
                     labels = c('Aalborg Attack rate ', "Aalborg Mortality rate",
                                'CPH Attack rate ', 'CPH Mortality rate')) +
  scale_shape_manual(values = c(4, 17, 19, 8),
                     breaks = c('aalborg.total_attack', 'aalborg.total_mort_rate',
                                'cph.total_attack', 'cph.total_mort_rate'),
                     labels = c('Aalborg Attack rate ', "Aalborg Mortality rate",
                                'CPH Attack rate ', 'CPH Mortality rate')) +
  scale_linetype_manual(values = c(5,5,1,1),
                        breaks = c('aalborg.total_attack', 'aalborg.total_mort_rate',
                                   'cph.total_attack', 'cph.total_mort_rate'),
                        labels = c('Aalborg Attack rate ', "Aalborg Mortality rate",
                                   'CPH Attack rate ', 'CPH Mortality rate')) +
  xlab("Age group") +
  ylab("Rate per 100 people\n") +
  ggtitle ("Cholera morbidity and \nmortality rate by age") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = c(x = 0.25, y = .80),
        axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5),
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

ggsave(filename = "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/Output/F8-cholera-mort-rate.jpg",
       plot = plot_chol_mort,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 600)



# FACET PLOT MONTHLY ALL_CAUSE MORTALITY  ---------------------------------

# Make into long format for ggplot
cph_allcause_long <- tidyr::gather(cph_allcause, "age", "mortality", 2:5)
dk_long <- tidyr::gather(dk_allcause, "age", "mortality", 2:5)

# Specify variable for facet_wrap
dk_long$area <- "All other cities"
cph_allcause_long$area <- "Copenhagen"

all_monthly_mort <- rbind(cph_allcause_long, dk_long)
all_monthly_mort$age <- ifelse(all_monthly_mort$age == ">50", "50+", all_monthly_mort$age)
# To shade 1853, need a rectangle that covers only this year for all y
all_monthly_mort$year <- format(all_monthly_mort$date, "%Y")
yrng <- range(all_monthly_mort$mortality)
start_53 <- as.Date("1853-01-01") # start of shading
end_53 <- as.Date("1853-12-31") # End of shading

# Re-level factor to make CPH plot first
all_monthly_mort$area <- as.factor(all_monthly_mort$area)
all_monthly_mort$age <- as.factor(all_monthly_mort$age)
all_monthly_mort$area <- relevel(all_monthly_mort$area, ref = "Copenhagen")
levels(all_monthly_mort$age)


allcause_plot <- ggplot(data = all_monthly_mort) +
  geom_line(size = 1,
            aes(x = date, y = mortality,
                group = age, color = age, linetype = age)) +
  # Color code the years - annotate is easier for some reasons:
  # http://goo.gl/7snZ8T
  annotate("rect", fill = "grey", alpha = 0.3,
           xmin = start_53, xmax = end_53,
           ymin = yrng[1], ymax = yrng[2])+
  facet_wrap(~ area, switch = "x") +
  xlab("") +
  ylab("Mortality") +
  scale_x_date(date_breaks = "4 month", date_labels = "%b %Y")+
  
  scale_color_manual(name = "Age group",
                       breaks = c("<10", "10-25", "26-50", "50+"),
                       values = c("red2", "black", "blue2", "green3" ))+
  scale_linetype_manual(name = "Age group",
                       breaks = c("<10", "10-25", "26-50", "50+"),
                       values = c("longdash",  "dotted", "solid", "twodash" ))+
  #ggtitle ("Monthly all-cause mortality") +
  theme_classic() +
  theme(legend.title = element_text(size = 16),
        legend.position = c(0.5, 0.6),
        legend.text = element_text(size = 15),
        axis.text.x = element_text(size = 13, angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = "bold", vjust = -0.1),
        axis.title.y = element_text(size = 18, face = "bold"),
        plot.title = element_text(size = 20, face="bold"),
        plot.margin = unit(c(-.5,0,-1.5,0), 'lines'),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14, face = "bold"))

allcause_plot