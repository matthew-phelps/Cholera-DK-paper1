rm(list = ls())
library(CholeraDataDK)
library(R0)


# LOAD --------------------------------------------------------------------
cases <- cholera_daily_data
aalborg <- cases[cholera_daily_data$city == "aalborg", ]
korsoer <- cases[cholera_daily_data$city == "korsoer", ]
cph <- cases[cholera_daily_data$city == "copenhagen", ]

# Get generation time: 10.1371/journal.pntd.0001901
mGT <- generation.time("lognorm", c(5, 1.5))
# Check generation time
plot(mGT)



# Aalborg------------------------------------------------------------
# Sensitivity
sens_time <-sensitivity.analysis(aalborg$cases, mGT, begin = 1:6, end = 15:20,
                                 est.method = "EG", sa.type = "time")
plot(sens_time)

r0 <- estimate.R(aalborg$cases, GT = mGT, t = aalborg$day_index, begin = 3, end = 17, methods = c("EG"), nsim = 1500)
summary(r0)
plot(r0)
plotfit(r0)

# Generation time Sensitivity
sens_GT <-sensitivity.analysis(aalborg$cases,
                             GT.type = "lognormal",
                             GT.mean = seq(2,6,0.5),
                             GT.sd.range = 1.5,
                             begin = 1, end = 17,
                             est.method = "EG",
                             sa.type = "GT")
sens_GT

plot(x=sens_GT[,"GT.Mean"],
     xlab="mean GT (days)",
     y=sens_GT[,"R"],
     ylim=c(1.2, 4.1),
     ylab="R0 (95% CI)",
     type="p", pch=19,
     col="black",
     main="Sensitivity of R0 to mean Generation Time")
arrows(x0=as.numeric(sens_GT[,"GT.Mean"]),
       y0=as.numeric(sens_GT[,"CI.lower"]),
       y1=as.numeric(sens_GT[,"CI.upper"]),
       angle=90,
       code=3,
       col="black",
       length=0.05)


# KORSOER -----------------------------------------------------------------

sens_time <-sensitivity.analysis(korsoer$cases, mGT, begin = 1:5, end = 14:18,
                                 est.method = "EG", sa.type = "time")
plot(sens_time)
r0 <- estimate.R(korsoer$cases, GT = mGT, t = korsoer$day_index, begin = 1, end = 16, methods = c("EG", "ML"), nsim = 1500)
plot(r0)
plotfit(r0)
plot(sens_time, what = "criterion")
# Sensitivity:
sens_GT <-sensitivity.analysis(korsoer$cases,
                               GT.type = "lognormal",
                               GT.mean = seq(2,7,0.5),
                               GT.sd.range = 1.5,
                               begin = 1, end = 17,
                               est.method = "EG",
                               sa.type = "GT")
sens_GT

plot(x=sens_GT[,"GT.Mean"],
     xlab="mean GT (days)",
     y=sens_GT[,"R"],
     ylim=c(1.2, 7.1),
     ylab="R0 (95% CI)",
     type="p", pch=19,
     col="black",
     main="Sensitivity of R0 to mean Generation Time")
arrows(x0=as.numeric(sens_GT[,"GT.Mean"]),
       y0=as.numeric(sens_GT[,"CI.lower"]),
       y1=as.numeric(sens_GT[,"CI.upper"]),
       angle=90,
       code=3,
       col="black",
       length=0.05)

# CPH -----------------------------------------------------------------

sens_time <-sensitivity.analysis(cph$cases, mGT, begin = 1:11, end = 30:40,
                                 est.method = "EG", sa.type = "time")
plot(sens_time)
# Because the sensitivity matrix is not so clear, look at deviance R-sq data
plot(sens_time, what = "criterion")

r0 <- estimate.R(cph$cases, GT = mGT, t = cph$day_index, begin = 1, end = 39, methods = c("EG", "ML"), nsim = 1500)
plot(r0)
plotfit(r0)

# Sensitivity:
sens_GT <-sensitivity.analysis(korsoer$cases,
                               GT.type = "lognormal",
                               GT.mean = seq(2,6,0.5),
                               GT.sd.range = 1,
                               begin = 1, end = 17,
                               est.method = "EG",
                               sa.type = "GT")
sens_GT

plot(x=sens_GT[,"GT.Mean"],
     xlab="mean GT (days)",
     y=sens_GT[,"R"],
     ylim=c(1.2, 4.1),
     ylab="R0 (95% CI)",
     type="p", pch=19,
     col="black",
     main="Sensitivity of R0 to mean Generation Time")
arrows(x0=as.numeric(sens_GT[,"GT.Mean"]),
       y0=as.numeric(sens_GT[,"CI.lower"]),
       y1=as.numeric(sens_GT[,"CI.upper"]),
       angle=90,
       code=3,
       col="black",
       length=0.05)
