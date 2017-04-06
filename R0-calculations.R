
# INTRO -------------------------------------------------------------------
rm(list = ls())
graphics.off()
library(devtools)
install_github("matthew-phelps/CholeraDataDK")
library(CholeraDataDK)
library(R0)
library(tidyverse)
library(ggplot2)


# LOAD --------------------------------------------------------------------
cases <- cholera_daily_data
aalborg <- cases[cholera_daily_data$city == "aalborg", ]
korsoer <- cases[cholera_daily_data$city == "korsoer", ]
cph <- cases[cholera_daily_data$city == "copenhagen", ]

# Get generation time: 10.1371/journal.pntd.0001901

# est.GT uses in-package function to calculate generation time dist
# 110
si <- c(2,2,9,1,1,1,3,3,5,4,4,4,6,2,2,3,5,6,10)
mGT <- est.GT(serial.interval = si) 

# We use our own estimate, but the parameters match closely to in-package est.
mGT <- generation.time("weibull", c(3.73, 2.38))
# Check generation time
# plot(mGT)



# Aalborg------------------------------------------------------------
# Sensitivity
sens_time <-sensitivity.analysis(aalborg$cases, mGT, begin = 1:6, end = 15:20,
                                 est.method = c("EG", "ML"), sa.type = "time")
plot(sens_time)
sens_time <-sensitivity.analysis(aalborg$cases, mGT, begin = 1:6, end = 14:19,
                                 est.method = "ML", sa.type = "time")

 plot(sens_time, what = "criterion")

aal <- estimate.R(aalborg$cases, GT = mGT, t = aalborg$day_index, begin = 3, end = 16, methods = c("EG", "ML", "TD"), nsim = 1500)

# Put R0 and CI into data frame so can plot each citys estimates on one graph
# For TD we average over the exponential growth phase
pe <- c(aal$estimates$EG$R, aal$estimates$ML$R, mean(aal$estimates$TD$R))
ci_l<- c(aal$estimates$EG$conf.int[1],
         aal$estimates$ML$conf.int[1],
         mean(aal$estimates$TD$conf.int[[1]]))
ci_u <- c(aal$estimates$EG$conf.int[2],
          aal$estimates$ML$conf.int[2],
          mean(aal$estimates$TD$conf.int[[2]]))
method <- c("EG", "ML", "TD")
r0 <- data.frame(pe, ci_l, ci_u, method)
r0$city <- "Åalborg"
rm(aal, ci_u, ci_l, method, pe)


# Generation time Sensitivity
sens_GT <-sensitivity.analysis(aalborg$cases,
                               GT.type = "lognormal",
                               GT.mean = seq(3,7,0.5),
                               GT.sd.range = 2,
                               begin = 1, end = 17,
                               est.method = "EG",
                               sa.type = "GT")
# plot(x=sens_GT[,"GT.Mean"],
#      xlab="mean GT (days)",
#      y=sens_GT[,"R"],
#      ylim=c(1.2, 4.1),
#      ylab="R0 (95% CI)",
#      type="p", pch=19,
#      col="black",
#      main="Sensitivity of R0 to mean Generation Time")
# arrows(x0=as.numeric(sens_GT[,"GT.Mean"]),
#        y0=as.numeric(sens_GT[,"CI.lower"]),
#        y1=as.numeric(sens_GT[,"CI.upper"]),
#        angle=90,
#        code=3,
#        col="black",
#        length=0.05)


# KORSOER -----------------------------------------------------------------

sens_time <-sensitivity.analysis(korsoer$cases, mGT, begin = 1:6, end = 21:26,
                                 est.method = "EG", sa.type = "time")
plot(sens_time)
kor <- estimate.R(korsoer$cases, GT = mGT, t = korsoer$day_index, begin = 1, end = 23, methods = c("EG", "ML", "TD"), nsim = 1500)
 plot(kor)

pe <- c(kor$estimates$EG$R, kor$estimates$ML$R, mean(kor$estimates$TD$R))
ci_l<- c(kor$estimates$EG$conf.int[1],
         kor$estimates$ML$conf.int[1],
         mean(kor$estimates$TD$conf.int[[1]]))
ci_u <- c(kor$estimates$EG$conf.int[2],
          kor$estimates$ML$conf.int[2],
          mean(kor$estimates$TD$conf.int[[2]]))
method <- c("EG", "ML", "TD")
r0_2 <- data.frame(pe, ci_l, ci_u, method)
r0_2$city <- "Korsør"
r0 <- rbind(r0, r0_2)
rm(kor, r0_2, ci_u, ci_l, method, pe)



# plotfit(kor)
# plot(sens_time, what = "criterion")
# Sensitivity:
sens_GT <-sensitivity.analysis(korsoer$cases,
                               GT.type = "lognormal",
                               GT.mean = seq(2,7,0.5),
                               GT.sd.range = 1.5,
                               begin = 1, end = 17,
                               est.method = "EG",
                               sa.type = "GT")
# plot(x=sens_GT[,"GT.Mean"],
#      xlab="mean GT (days)",
#      y=sens_GT[,"R"],
#      ylim=c(1.2, 7.1),
#      ylab="R0 (95% CI)",
#      type="p", pch=19,
#      col="black",
#      main="Sensitivity of R0 to mean Generation Time")
# arrows(x0=as.numeric(sens_GT[,"GT.Mean"]),
#        y0=as.numeric(sens_GT[,"CI.lower"]),
#        y1=as.numeric(sens_GT[,"CI.upper"]),
#        angle=90,
#        code=3,
#        col="black",
#        length=0.05)

# CPH -----------------------------------------------------------------

sens_time <-sensitivity.analysis(cph$cases, mGT, begin = 1:11, end = 30:40,
                                 est.method = "EG", sa.type = "time")
plot(sens_time)
# Because the sensitivity matrix is not so clear, look at deviance R-sq data
# plot(sens_time, what = "criterion")

cph_r0 <- estimate.R(cph$cases, GT = mGT, t = cph$day_index, begin = 1, end = 39, methods = c("EG", "ML", "TD"), nsim = 1500)
# plotfit(cph_r0)
pe <- c(cph_r0$estimates$EG$R, cph_r0$estimates$ML$R, mean(cph_r0$estimates$TD$R))
ci_l<- c(cph_r0$estimates$EG$conf.int[1],
         cph_r0$estimates$ML$conf.int[1],
         mean(cph_r0$estimates$TD$conf.int[[1]]))
ci_u <- c(cph_r0$estimates$EG$conf.int[2],
          cph_r0$estimates$ML$conf.int[2],
          mean(cph_r0$estimates$TD$conf.int[[2]]))
method <- c("EG", "ML", "TD")
r0_2 <- data.frame(pe, ci_l, ci_u, method)
r0_2$city <- "Copenhagen"
r0 <- rbind(r0, r0_2)
rm(cph_r0, r0_2, ci_u, ci_l, method, pe)
r0$city <- factor(r0$city)



# Sensitivity:
sens_GT <-sensitivity.analysis(korsoer$cases,
                               GT.type = "lognormal",
                               GT.mean = seq(2,6,0.5),
                               GT.sd.range = 1,
                               begin = 1, end = 17,
                               est.method = "EG",
                               sa.type = "GT")
# plot(x=sens_GT[,"GT.Mean"],
#      xlab="mean GT (days)",
#      y=sens_GT[,"R"],
#      ylim=c(1.2, 4.1),
#      ylab="R0 (95% CI)",
#      type="p", pch=19,
#      col="black",
#      main="Sensitivity of R0 to mean Generation Time")
# arrows(x0=as.numeric(sens_GT[,"GT.Mean"]),
#        y0=as.numeric(sens_GT[,"CI.lower"]),
#        y1=as.numeric(sens_GT[,"CI.upper"]),
#        angle=90,
#        code=3,
#        col="black",
#        length=0.05)

# SAVE DATA ---------------------------------------------------------------

save(r0, file = "data/r0.Rdata")

r0 %>%
  filter(method != "TD")

# R0 plots for cities -----------------------------------------------------

