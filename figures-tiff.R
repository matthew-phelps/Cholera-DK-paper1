# JID requires all figures in separate TIFF files

# INTRO -------------------------------------------------------------------
rm(list = ls())
graphics.off()
devtools::install_github('matthew-phelps/CholeraDataDK', force = F)

library(tidyverse)
library(CholeraDataDK)
library(scales) # required for some ggplot functions
library(cowplot)
source("functions.R") # required for rotating text in ggplot 
source("plots/plotHelperFunctions.R")

figure_path <- "C:/Users/wrz741/Google Drive/Copenhagen/DK Cholera/Cholera-DK-paper1/submission/figures"

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

# Re-order levels of factor so that plotting works: http://goo.gl/CD2fEC
age_char <- as.character(mort$age_range)
mort$age_range <- factor(age_char, levels = c(age_char))


# FIG 1 -------------------------------------------------------------------


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
  SeasonPlot(lab_x = lab_x, lab_size = 4, txt_size = 12, line_size = 0.8,
             broad_street = TRUE, broad_street_data = broad_st)
plot_season
ggsave(paste(figure_path, "/Fig1.tiff", sep=""),
       plot_season, dpi = 400,
       width = 8, height = 4, units = "in")



# FIG 2 -------------------------------------------------------------------
# To shade 1853, need a rectangle that covers only this year for all y
all_cause <- gather(dead_all_cause, cause, dead, 4:6)
yrng <- range(0:4000)
start_53 <- as.Date("1853-06-01") # start of shading
end_53 <- as.Date("1853-10-01") # End of shading



all_cause_plot <- all_cause %>%
  deathsBasePlot(l_size = 0.7)%>%
  deathPoints(p_size = 1.5, space = FALSE) %>%
  deathsStyles(txt_size = 12)
all_cause_plot

ggsave(paste(figure_path, "/Fig2.tiff", sep=""),
       all_cause_plot, dpi = 400,
       width = 8, height = 4, units = "in")




# FIG 3 -------------------------------------------------------------------



# Remove time-dependent
r0 <- r0[r0$method != "TD", ]
R0 <- r0 %>%
  r0Plot(pd = 0.4, line_size = 0.7) %>%
  r0PlotStyle(txt_size = 12)
ggsave(paste(figure_path, "/Fig3.tiff", sep=""),
     R0, dpi = 400,
     width = 7, height = 5, units = "in")

