# Author: Matthew Phelps
# Desc: Estimate serial interval from transmission chains


## Intro
rm(list = ls())

library(MASS)
library(fitdistrplus)
library(mixdist)
# DATA --------------------------------------------------------------------

si <- c(3,2,2,6,2,2,2,5,2,3,5,2,6,2,3,9,1,1,1,3,3,3,10,10,4,4,4)
length(si)
mean(si)
median(si)
hist(si)
plot(density(si))

fit.exp <- fitdistr(si, "exponential")

sim.exp <- rexp(1e5, rate = fit.exp$estimate[[1]])
plot(density(sim.exp))
lines(density(si), col = "red")

fit.wei <- fitdistr(si, "weibull", start = list(shape = 2, scale = 4))
fit.wei2 <- fitdist(si, "weibull", start = list(shape = 2, scale = 4))
sim.wei <- rweibull(1e6, shape = fit.wei$estimate[[1]],
                    scale = fit.wei$estimate[[2]])
plot(fit.wei2)

plot(density(sim.wei))
lines(density(si), col ="red")



# BOOTSTRAP ---------------------------------------------------------------
# http://goo.gl/LUeCli
d_range <- seq(0, 10, len = 600)

# Non-parametric bootsraping: We are making no assumptions about the underlying distribution of the data we are sampling from. i.e sampling from empirical distribution
set.seed(13)
boot.pdf <- sapply(1:1000, function(i) {
  si.boot <- sample(si, size=length(si), replace=TRUE) # re-sample data
  MLE.est <- suppressWarnings(fitdistr(si.boot, densfun="weibull", lower = 0))  # fit dist to each new sample
  dweibull(d_range, shape=as.numeric(MLE.est[[1]][1]), scale=as.numeric(MLE.est[[1]][2])) # create pdf using fitted params
  
}
)

# Plot all outputs
plot(d_range, boot.pdf[, 1], type = "l",
     col=rgb(.6, .6, .6, .1),
     ylim=range(boot.pdf),
     xlab="x", ylab="Probability density")
for(i in 1:ncol(boot.pdf)){
  lines(d_range, boot.pdf[, i], col = rgb(0.6, 0.6, 0.6, 0.1))
}

# 95% CI from bootstrap:
quants <- apply(boot.pdf, 1, quantile, c(0.025, 0.5, 0.975))
min.point <- apply(boot.pdf, 1, min, na.rm=T)
max.point <- apply(boot.pdf, 1, max, na.rm=T)
lines(d_range, quants[1, ], col = "red", lwd=1.5, lty=2)
lines(d_range, quants[2, ], col = "red", lwd=1.5, lty=2)
lines(d_range, quants[3, ], col = "red", lwd=1.5, lty=2)


dev.copy(png,
         file = "output/F-10 - serial interval.jpg",
         width = 20,
         height = 20,
         res = 300,
         units = "cm")
dev.off()


# 95CI around mean --------------------------------------------------------

set.seed(13)
mu <- sapply(1:10000, function(i) {
  si.boot <- sample(si, size=length(si), replace=TRUE) # re-sample data
  MLE.est <- suppressWarnings(fitdistr(si.boot, densfun="weibull", lower = 0))  # fit dist to each new sample
  dweibull(d_range, shape=as.numeric(MLE.est[[1]][1]), scale=as.numeric(MLE.est[[1]][2])) # create pdf using fitted params
  mu <- weibullparinv(shape = MLE.est$estimate[1],
                      scale = MLE.est$estimate[2])
  mu <- as.matrix(mu[[1]])
}
)
mean(mu)
mu_range <- quantile(mu,  c(0.025, 0.975))
mu_range
plot(density(mu))


# STANDARD DEVIATION ------------------------------------------------------
# Get shape and scale parameters from fit.wei object and run in WolframAlpha:
# weibull(shape, scale)
fit.wei2
confint(fit.wei2)

sd(si)/mean(si)
