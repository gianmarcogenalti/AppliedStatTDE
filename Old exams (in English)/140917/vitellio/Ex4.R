rm(list = ls())
data <- read.table("tide.txt")
load("mcshapiro.test.RData")

t0 <- 82
fm <- lm(h ~ I(sin(2*pi/28*t)) + I(sin(pi/365*(t-t0))) + t, data = data)
summary(fm)

x11()
par(mfrow=c(2,2))
plot(fm)
shapiro.test(residuals(fm))
shapiro.test(rstandard(fm))

library(car)
linearHypothesis(fm, rbind(c(0,1,0,0), c(0,0,1,0)), c(0,0))


fmr <- lm(h ~ I(sin(2*pi/28*t)) + I(sin(pi/365*(t-t0))), data = data)
summary(fmr)

Z0.new <- data.frame(t = 263)
Z1.new <- data.frame(t = 335)

# Pred. int. for a new obs
Pred1 <- predict(fmr, Z0.new, interval='prediction', level=1-0.1/2)
Pred2 <- predict(fmr, Z1.new, interval='prediction', level=1-0.1/2)
