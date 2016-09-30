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
library(gridExtra) # required to remove empty plots in facet_wrap()
source("C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/functions.R") # required for rotating text in ggplot 
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

# 1 - FACET PLOT MONTHLY ALL_CAUSE MORTALITY  ---------------------------------

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
start_53 <- as.Date("1853-06-01") # start of shading
end_53 <- as.Date("1853-10-01") # End of shading

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
  ylab("All-cause mortality") +
  scale_x_date(date_breaks = "4 month", date_labels = "%b %Y")+
  scale_y_continuous(breaks = seq(0, 1250, 250)) +
  
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


ggsave(filename = "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/Output/1-monthly all-cause mort.jpg",
       plot = allcause_plot,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 600)


# 2 - INCIDENCE PER 10K ----------------------------------------------------

all_cases_temp <- all_cases_temp[order(all_cases_temp$city, all_cases_temp$date), ]

# To plot all epidemics on same calendar year - put them on a "dummy" year
all_cases_temp$season <- paste("100",
                               format(all_cases_temp$date, "%m"),
                               format(all_cases_temp$date, "%d"),
                               sep = "-")
all_cases_temp$season <- as.Date(all_cases_temp$season)
all_cases <- all_cases_temp[all_cases_temp$city != "brandholm", ]



plot_season <- ggplot(data = all_cases,
                      aes(x = season, y = cases_norm,
                          group = city, color = city, linetype = city)) +
  geom_line(size= 1) +
  xlab("Date") +
  ylab("Incidence per 10,000 people") +
  ggtitle ("Daily cholera case incidence") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.5),
        legend.text = element_text(size = 17),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, vjust = -0.1),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 20),
        plot.margin = unit(c(0,0,0.5,0), 'lines')) +
  coord_cartesian(ylim = c(5,max(all_cases$cases_norm))) +
  scale_color_brewer(breaks = c('copenhagen', 'aalborg', 'korsoer'),
                     labels = c('Copenhagen (1853)', 'Aalborg (1853)', 'Korsør (1857)'),
                     palette = "Dark2") +
  scale_linetype_discrete(breaks = c('copenhagen', 'aalborg', 'korsoer'),
                          labels = c('Copenhagen (1853)', 'Aalborg (1853)', 'Korsør (1857)')) +
  guides(colour = guide_legend(keywidth = 2.5)) # Makes legend symbole wider

plot_season


ggsave(filename = 'Output/2-seasonality.jpg',
       plot = plot_season,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 300)




# 3 - Total mortality & total attack rate due to cholera -----------------------------------------
# DF with cholera mort as % of total mort & cholera mort rates in 1 df
limits <- aes(ymax = upper95, ymin = lower95,
              x= age_range, color = plot_var)
dodge <- position_dodge(width=- 0.3)


# In order to have facet_wrap fill the bottom row, first we have to create a empty plot, then move that plot to the top row, then make that plot invisible
# Creat empty plot to be plotted by adding an dummy "city" to be plotted
chol_burden$city2 <- factor(chol_burden$city,
                            levels = c("", "aalborg", "korsoer", "cph"))
levels(chol_burden$city2)
levels(chol_burden$city2) <- c("", "Aalborg", "Korsør (1857)", "Copenhagen")
plot_chol_mort <- ggplot() +
  # Style the age 0 - 70 +. Will style the "total" group separately
  geom_line(data = chol_burden[chol_burden$age_range != "Total", ],
            position = dodge,
            aes(x = age_range, y = pe, group = plot_var, color = plot_var),
            size = 0.9, alpha = 0.5) +
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
  
  # facet wrap and put labels below x-axis (swtich="x"). Use city2 var to plot
  # empty plot so that bottom row is filled
  facet_wrap(~ city2, ncol = 2, switch = "x", drop = FALSE) +
  
  # Create symbology
  scale_color_manual(values = c("red2",  "red4",
                                "green3", "green4",
                                "steelblue1" , "royalblue4"),
                     breaks = c('korsoer.total_attack', 'korsoer.total_mort_rate',
                                'aalborg.total_attack', 'aalborg.total_mort_rate',
                                'cph.total_attack', 'cph.total_mort_rate')) +

  scale_shape_manual(values = c(16, 17, 16, 17, 16, 17),
                     breaks = c('korsoer.total_attack', 'korsoer.total_mort_rate'),
                     labels = c('Attack rate', 'Mortality rate')) +
  
  guides(color = FALSE,
         shape = guide_legend(keywidth = 2.5,
                              keyheight = 1.5)) + # Removes color part of legend
  
  xlab("Age group") +
  ylab("Rate per 100 people\n") +
  coord_cartesian(ylim = c(0, 60)) +
  ggtitle ("Cholera morbidity and \nmortality rate by age") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = c(x = 0.2, y = .35),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 15,
                                   angle = 45, vjust = 1.5, hjust = 1.0,
                                   margin = margin(0,0,-10,0)),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 18,
                                    face = "bold",
                                    margin = margin(0, 0, 10, 0)),
        axis.title.y = element_text(size = 18,
                                    face = "bold"),
        plot.title = element_text(size = 18, face="bold"),
        plot.margin = unit(c(0,0,0,0.5), 'lines'),
        strip.text.x = element_text(size = 14,
                                    margin = margin(0,0,5,0)),
        panel.margin.y = unit(0, "lines"))
 
plot_chol_mort

## Use grob to remove empty panels from plot. If you work with another set of
## plots, look at the output of names(g$grobs) and g$layout$name to figure out,
## which elements have to be removed. https://goo.gl/4AK5Ey
g <- ggplotGrob(plot_chol_mort)

# remove empty panels
g$grobs[names(g$grobs) %in% c("panel1", "strip_t1")] <- NULL

# remove panel from layout
g$layout <- g$layout[!(g$layout$name %in% c('panel-1', 'strip_t-1')), ]

# move axis closer to panel
g$layout[g$layout$name == "axis_b-9", c("t", "b")] = c(9,9)

# plot output

grid.newpage()
grid.draw(g)
ggsave(filename = "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/Output/3-cholera-mort-rate2.jpg",
       plot = g,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 600)

plot_chol_mort
ggsave(filename = "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/Output/3-cholera-mort-rate.jpg",
       plot = plot_chol_mort,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 600)



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
limits = aes(ymax = hi95, ymin = low95, x = age_range)

plot_chol_pct <- ggplot() +
  geom_line(data = cho_pct[1:8, ], aes(x = age_range, y = mortality, group = 1),
            size = 1.1) +
  geom_point(data = cho_pct[1:8, ], aes(x = age_range, y = mortality),
             size = 3.1) +
  geom_errorbar(data = cho_pct[cho_pct$age_range !="Total", ],
                limits,
                width = 0.3,
                size = 0.8) +
  geom_point(data = cho_pct[9, ], aes(x = age_range, y = mortality),
             size = 3.5, color = "dark red") +
  geom_errorbar(data = cho_pct[cho_pct$age_range =="Total", ],
                limits,
                width = 0.3,
                size = 0.8, color = "darkred") +
  xlab("Age group") +
  ylab("Proportion") +
  scale_y_continuous(breaks = seq(0, 0.8, 0.2),
                     limits = c(0, 0.8)) + 
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







