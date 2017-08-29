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
cph_quarters$date <- seq(as.Date("1853-06-12"), by =7, len = 16)
cph_quarters$daily_ar <- (cph_quarters$sick.total.week / cph_quarters$est.pop.1853)/7
st_annae_oe <- cph_quarters %>%
  filter( quarter == "St. Annae Oester")


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
st_annae_oe$season <-  paste("100",
                             format(st_annae_oe$date, "%m"),
                             format(st_annae_oe$date, "%d"),
                             sep = "-")



all_cases$season <- as.Date(all_cases$season)
broad_st$season <- as.Date(broad_st$season)
st_annae_oe$season <- as.Date(st_annae_oe$season)
lab_x <- min(all_cases$season)
all_cases$city_fac <- factor(all_cases$city,
                             levels = c("copenhagen", "aalborg", "korsoer"))

plot_season <- all_cases %>%
  SeasonPlot(lab_x = lab_x, lab_size = 4, txt_size = 12, line_size = 0.8,
             broad_street = TRUE, broad_street_data = broad_st)
plot_season

# Add CPH Quarters

plot_season2 <- plot_season +
  geom_line(data = st_annae_oe,
            aes(x = season, y = daily_ar * 10000),
            size = 0.8,
            color = "#E69F00") +
  annotate("text", x = lab_x + 9 , y = 17,
           color="#E69F00", label="St. Annae Oester \n (Copenhagen)", size = 4)


ggsave(paste(figure_path, "/PhelpsFig1.tiff", sep=""),
       plot_season2, dpi = 400,
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

ggsave(paste(figure_path, "/PhelpsFig2.tiff", sep=""),
       all_cause_plot, dpi = 400,
       width = 8, height = 4, units = "in")




# FIG 3 -------------------------------------------------------------------
# Remove time-dependent
r0 <- r0[r0$method != "TD", ]
R0 <- r0 %>%
  r0Plot(pd = 0.4, line_size = 0.7) %>%
  r0PlotStyle(txt_size = 12)
ggsave(paste(figure_path, "/PhelpsFig3.tiff", sep=""),
       R0, dpi = 400,
       width = 7, height = 5, units = "in")



# FIG 4 -------------------------------------------------------------------


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

plot_chol_mort <- cholMortPlot(p_size = 1.8, line_size = 0.7) %>%
  mortPlotStyle(txt_size = 11)
ggsave(file = paste(figure_path, "/PhelpsFig4.tiff", sep=""),
       dpi = 400,
       plot_chol_mort, width = 8, height = 4.5)


# FIG 5 -------------------------------------------------------------------


limits = aes(ymax = hi95, ymin = low95, x = age_range)
chol_proportion_death_plot <- cholProportionDeaths(line_size = 1) %>%
  proportionDeathStyle(txt_size = 12)
ggsave(file = paste(figure_path, "/PhelpsFig5.tiff", sep=""),
       dpi = 400,
       chol_proportion_death_plot, width = 8, height = 4.5)




# Supp FIG 1 --------------------------------------------------------------


cph_quarter_for_plot <- cph_quarters %>%
  filter(quarter != "Fra skibe" & quarter != "Ladegaarden" &
           quarter != "Vesterbro" & quarter != "Noerrebro" & quarter != "Oesterbro")
quarters <- quarterPanelIncidence(cph_quarter_for_plot)
ggsave(file = paste(figure_path, "/PhelpsSuppFig1.tiff", sep=""),
       dpi = 400,
       plot = quarters, width = 8, height = 4.5)



# Supp FIG 2 --------------------------------------------------------------

title <- "Relative risk of cholera morbidity by age & gender \n"
notes <- "*male is reference group"

# Attack RR
plot_attack <- rr_sic %>%
  rrAgeGender(pd = 0.4, pd2 = 0.6,
              line_size = 0.5) %>%
  rrAgeGenderStyle(txt_size = 12,  ylabel = "infection")



plot_mort <- rr_mrt %>%
  rrAgeGender(pd = 0.4, pd2 = 0.6,
              line_size = 0.5) %>%
  rrAgeGenderStyle(txt_size = 12,
                   ylabel = "death",
                   x_title = TRUE)

# MULTIPLOT 
plot_mort2 <- plot_grid(plot_attack, plot_mort, nrow = 2, labels = c("A", "B"))
cowplot::ggsave(file = paste(figure_path, "/PhelpsSuppFig2.tiff", sep=""),
                plot = plot_mort2,
                dpi = 400,
                width = 8, height = 7)
