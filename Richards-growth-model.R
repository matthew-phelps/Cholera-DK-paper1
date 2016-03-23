rm(list = ls())
library(CholeraDataDK)
library(grofit)

# LOAD --------------------------------------------------------------------
cases <- cholera_daily_data
aalborg <- cases[cholera_daily_data$city == "aalborg", ]
korsoer <- cases[cholera_daily_data$city == "korsoer", ]
cph <- cases[cholera_daily_data$city == "copenhagen", ]

aalborg$cum_cases <- cumsum(aalborg$cases)
korsoer$cum_cases <- cumsum(korsoer$cases)
cph$cum_cases <- cumsum(cph$cases)

# Check how cumulative cases look
plot(cph$cum_cases, type = 'l')
plot(aalborg$cum_cases, type = 'l')
plot(korsoer$cum_cases, type = 'l')


# Rrichards ---------------------------------------------------------------

x <- 1:112
cph_fit <- gcFitModel(x, cph$cum_cases)
b <- summary.gcFitModel(cph_fit)
fit_val <- cph_fit$fit.data
plot(fit_val)
lines(cph$cum_cases, col = "red")
