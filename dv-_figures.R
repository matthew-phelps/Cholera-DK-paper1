# Author: Matthew Phelps
# Desc: Comparing the 3 cities' outbreals
# Output datasets: Rdata in local directory

# INTRO -------------------------------------------------------------------
rm(list = ls())
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
source("functions.R") # required for rotating text in ggplot 
source("plots/plotHelperFunctions.R")

# LOAD --------------------------------------------------------------------

load("data/data-viz-prep.Rdata")
load("data/r0.Rdata")
all_cases_temp <- cholera_daily_data
mort <- cph_mort_rates_10yr
pop <- cph_pop1853_10yr
attack <- cph_age_attack_rate
cases <- cholera_daily_data_towns
cen <- census
dead_all_cause <- all_cause_1852_1854

rownames(cen) <- NULL

# CHECK DATA SUMMATIONS ---------------------------------------------------
cen %>%
  filter(place == "korsoer", year == "1857") %>%
  select(total) %>%
  sum()


# Re-order levels of factor so that plotting works: http://goo.gl/CD2fEC
age_char <- as.character(mort$age_range)
mort$age_range <- factor(age_char, levels = c(age_char))

# To long format data
mort_gender <- gather(mort[, c("age_range", "male_mort2", "female_mort2")],
                      key = gender,
                      value = mort_rate,
                      c(male_mort2, female_mort2))

# 1 - ALL_CAUSE MORTALITY  ---------------------------------

# To shade 1853, need a rectangle that covers only this year for all y
all_cause <- gather(dead_all_cause, cause, dead, 4:6)
yrng <- range(0:4000)
start_53 <- as.Date("1853-06-01") # start of shading
end_53 <- as.Date("1853-10-01") # End of shading



all_cause_plot <- all_cause %>%
  deathsBasePlot()%>%
  deathPoints(p_size = 2.2, space = FALSE) %>%
  deathsStyles()
all_cause_plot


ggsave(filename = "Output/2-monthly-all-cause-mort.jpg",
       plot = all_cause_plot,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 300)


allcause_plot <- all_monthly_mort[all_monthly_mort$age != "total" & all_monthly_mort$area=="Copenhagen", ] %>%
  deathsBasePlot() %>%
  deathsStyles()
allcause_plot






# 2 - DAILY INCIDENCE PER 10K ----------------------------------------------------
all_cases <- all_cases_temp %>%
  arrange(city, date) %>%
  filter(city != "brandholm")
# To plot all epidemics on same calendar year - put them on a "dummy" year
all_cases$season <- paste("100",
                          format(all_cases$date, "%m"),
                          format(all_cases$date, "%d"),
                          sep = "-")
broad_st$season <-  paste("100",
                          format(broad_st$date, "%m"),
                          format(broad_st$date, "%d"),
                          sep = "-")
all_cases$season <- as.Date(all_cases$season)
broad_st$season <- as.Date(broad_st$season)

lab_x <- min(all_cases$season)


plot_season <- all_cases %>%
  plotSeason(lab_x = lab_x, broad_street = TRUE, broad_street_data = broad_st)
plot_season

ggsave(filename = "Output/1-seasonality-temp.jpg",
       plot = plot_season,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 300)




# 3 - Total mortality & total attack rate due to cholera -----------------------------------------
# DF with cholera mort as % of total mort & cholera mort rates in 1 df
limits <- aes(ymax = upper95, ymin = lower95,
              x= age_range, color = plot_var2)
dodge <- position_dodge(width=- 0.3)
dodge2 <- position_dodge(width=- 0.5)


# In order to makes space between cities in legend, need to plot empty "dummy" variables that will be white space. Creat empty factors:
chol_burden$plot_var2 <- chol_burden$plot_var
chol_burden$plot_var2 <- factor(chol_burden$plot_var2,
                                levels = c("korsoer.total_attack", 
                                           "korsoer.total_mort_rate",
                                           "a",
                                           "aalborg.total_attack",
                                           "aalborg.total_mort_rate",
                                           "b",
                                           "cph.total_attack",
                                           "cph.total_mort_rate"))

plot_chol_mort <- cholMortPlot(p_size = 0) %>%
  mortPlotStyle()
plot_chol_mort
ggsave(filename = "Output/4-cholera-mort-rate.jpg",
       plot = plot_chol_mort,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 600)


# 4 - Cholera Deaths -------------------------------------------------
limits = aes(ymax = hi95, ymin = low95, x = age_range)



chol_proportion_death_plot <- cholProportionDeaths() %>%
  proportionDeathStyle()
chol_proportion_death_plot



ggsave(filename = "Output/5-cholera-of-total.jpg",
       plot = chol_proportion_death_plot,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 600)


# 5 - RELATIVE RISK ---------------------------------------
limits = aes(ymax = up95, ymin = low95, color = city, x = age_range)
title <- "Relative risk of cholera morbidity by age & gender \n"
notes <- "*male is reference group"
pd <- position_dodge(0.4)
pd2 <- position_dodge(0.6)

# To specify that CPH is plotted first changed factors
rr_sic$city <- factor(rr_sic$city, levels = c("cph", "aalborg", "korsoer"))
levels(rr_sic$city) <- c("Copenhagen", "Aalborg", "Korsør (1857)")

# 5 ATTACK RR PLOT
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
             position = pd2,
             size = 3.0) +
  geom_errorbar(data = rr_sic[rr_sic$age_range =="Total", ],
                limits,
                width = 0.4,
                size = 1.3,
                position = pd2) +
  coord_cartesian(ylim = c(0, 4)) +
  scale_color_manual(values = c("#E69F00","#006DDB", "green4"),
                     breaks = c('Copenhagen', 'Aalborg', 'Korsør (1857)'),
                     labels = c('Copenhagen', 'Aalborg', "Korsør (1857)")) +
  scale_shape_manual(values = c(8, 17, 15),
                     breaks = c('Copenhagen', 'Aalborg', 'Korsør (1857)'),
                     labels = c('Copenhagen', 'Aalborg', "Korsør (1857)"))  +
  geom_hline(yintercept = 1) +
  xlab("") +
  ylab("Relative risk of cholera infection") +
  # annotate("text", x = 8.5, y = 4.0, label = notes) +
  # ggtitle (title)  +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(x = 0.88, y = .8),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, vjust = 1.4, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15,
                                    vjust = -0.1),
        axis.title.y = element_text(size = 15,
                                    margin = margin(0,20,0,0)),
        plot.title = element_text(size = 16),
        plot.margin = unit(c(1.9,0.3,0.2,1.0), 'lines'))
plot_attack
setwd(main.path)
# ggsave(filename = "Output/6-RR-attack.jpg",
#        plot = plot_attack,
#        width = 26,
#        height = 20,
#        units = 'cm',
#        dpi = 600)



# 6 - DEATH RELATIVE RISK DATA-
# set limits for error bars: http://goo.gl/4QE74U
limits = aes(ymax = up95, ymin = low95, x = age_range, color = city)
notes <- "*male is reference group"
title <- "Relative risk of cholera mortality by age & gender \n"
pd <- position_dodge(0.4)
pd2 <- position_dodge(0.6)

# To specify that CPH is plotted first changed factors
rr_mrt$city <- factor(rr_mrt$city, levels = c("cph", "aalborg", "korsoer"))
levels(rr_mrt$city) <- c("Copenhagen", "Aalborg", "Korsør (1857)")

# 6 - DEATH RR PLOT -
plot_mort <- ggplot() +
  geom_point(data = rr_mrt[rr_mrt$age_range != "Total", ],
             aes(x = age_range,
                 y = rr,
                 color = city,
                 shape = city),
             position = pd, size = 2.5) +
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
             position = pd2, size = 3.0) +
  geom_errorbar(data = rr_mrt[rr_mrt$age_range == "Total", ],
                limits,
                width = 0.4,
                size = 1.3,
                position = pd2) +
  coord_cartesian(ylim = c(0, 4)) +
  scale_color_manual(values = c("#E69F00","#006DDB", "green4"),
                     breaks = c('Copenhagen', 'Aalborg', 'Korsør (1857)'),
                     labels = c('Copenhagen', 'Aalborg', "Korsør (1857)")) +
  scale_shape_manual(values = c(8, 17, 15),
                     breaks = c('Copenhagen', 'Aalborg', 'Korsør (1857)'),
                     labels = c('Copenhagen', 'Aalborg', "Korsør (1857)")) +
  geom_hline(yintercept = 1) +
  xlab("Age") +
  ylab("Relative risk of cholera death") +
  # annotate("text", x = 8.5, y = 4.0, label = notes) +
  #ggtitle (title)  +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(x = 0.88, y = .8),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, vjust = 1.4, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15,
                                    vjust = -0.1),
        axis.title.y = element_text(size = 15,
                                    margin = margin(0,20,0,0)),
        plot.title = element_text(size = 16),
        plot.margin = unit(c(1.9,0.3,0.2,1.0), 'lines'))
plot_mort

      

# MULTIPLOT ---------------------------------------------------------------

setwd(main.path)
jpeg(filename = "Output/5-RR-gender.jpg",
     width = 20,
     height = 26,
     units = "cm",
     quality = 100,
     res = 600)
multiplot(plot_attack, plot_mort, cols = 1)
dev.off()







# 4 - R0 PLOTS ------------------------------------------------------------

pd <- position_dodge(0.4)

# Remove time-dependent
r0 <- r0[r0$method != "TD", ]
R0 <- ggplot(data = r0,
             aes(x = city, y = pe, color = method)) +
  geom_point(position = pd, aes(shape = method),
             size = 3.5) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u),
                width = 0.15,
                size = 1.1,
                position = pd) +
  xlab("Town") + 
  ylab(expression(bold(Repdocutive~number~"("*R[0]*")"))) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.89),
        legend.text = element_text(size = 15),
        axis.text.x = element_text(size = 12,
                                   margin = margin(5,0,25,0)),
        axis.text.y = element_text(size = 12,
                                   margin = margin(0, 2, 0, 0)),
        axis.title.x = element_text(size = 14, face = "bold",
                                    margin = margin(0, 0, 4, 0)),
        axis.title.y = element_text(size = 14, face = "plain",
                                    margin = margin(0,15,0,0)),
        plot.margin = unit(c(1.9,0.3,0.2,1.0), 'lines')) +
  coord_cartesian(ylim = c(0, max(r0$ci_u))) +
  scale_color_manual(values = c("orange", "dodgerblue4"),
                     breaks = c("EG", "ML"),
                     labels = c("Exponential \nGrowth\n",
                                "Maximum \nLikelihood")) +
  scale_shape_manual(values = c(19, 17),
                     breaks = c("EG", "ML"),
                     labels = c("Exponential \nGrowth\n",
                                "Maximum \nLikelihood")) +
  guides(color = guide_legend(keywidth = 2.3, keyheight = 1.8)) 

R0


ggsave(filename = "Output/3 - R0.jpg",
       plot = R0,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 300)



# LONDON ------------------------------------------------------------------

all_cases <- all_cases_temp %>%
  arrange(city, date) %>%
  filter(city != "brandholm")

# To plot all epidemics on same calendar year - put them on a "dummy" year
all_cases$season <- paste("100",
                          format(all_cases$date, "%m"),
                          format(all_cases$date, "%d"),
                          sep = "-")
all_cases$season <- as.Date(all_cases$season)
lab_x <- min(all_cases$season)
lab_size <- 6

broad_st$city <- "london"
head(broad_st)
head(all_cases)


plot_london <- ggplot() +
  geom_line(data = all_cases,
            aes(x = season, y = cases_norm,
                group = city, color = city),
            size = 1.1) +
  xlab("Date") +
  ylab("Death counts") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15, vjust = -0.1),
        axis.title.y = element_text(size = 15,
                                    margin = margin(0,20,0,0)),
        plot.margin = unit(c(1.9,0.3,0.2,1.0), 'lines')) +
  coord_cartesian(ylim = c(5,max(broad_st$fatal_attacks)))
plot_london




# QUARTER PANEL PLOTS -----------------------------------------------------


quarter_panel_incidence <- function(combined) {
  ggplot (combined,
          aes( x = week.id,
               y = sick.total.week / est.pop.1853*1000,
               group = quarter))+
    geom_line(size = 1) +
    geom_vline( xintercept = 5, linetype = 2, color = "black", alpha = 0.5) +
    facet_wrap(~quarter) +
    xlab("Week index") +
    ylab("Incidence per 1000 people") +
    theme_classic() +
    theme(legend.position = 'none',
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 12, vjust = -0.1),
          axis.title.y = element_text(size = 12, vjust = 0.5),
          strip.background = element_rect(color = '#F0F0F0', fill = '#F0F0F0')) 
}

cph_quarter_for_plot <- cph_quarters %>%
  filter(quarter != "Fra skibe" & quarter != "Ladegaarden" &
           quarter != "Vesterbro" & quarter != "Noerrebro" & quarter != "Oesterbro")
quarters <- quarter_panel_incidence(cph_quarter_for_plot)

setwd(main.path)
ggsave(filename = "Output/S1 - quarters.tiff",
       plot = quarters,
       width = 26,
       height = 20,
       units = 'cm',
       dpi = 300)
