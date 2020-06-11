rm(list = ls())
data <- read.table("albatross.txt")
head(data)
summary(data)
data$wind <- as.factor(data$wind)

fit <- lm(distance ~ wind*I(Vi^2) + wind*I(Va^2), data = data)
summary(fit)

library(car)

fit2 <- lm(distance ~ I(Vi^2) + I(Va^2) + wind:I(Vi^2) + wind:I(Va^2), data = data)
summary(fit2)

A <- rbind(c(0,1,1,1,1),
           c(0,1,1,0,0))
b <- rbind(0,0)
linearHypothesis(fit2, A, b)


fit3 <- lm(distance ~ I(Va^2-Vi^2) + wind:I(Va^2-Vi^2), data = data)
summary(fit3)


# d)
newdatum <- data.frame(Vi = 25, Va = 35, wind = as.factor("upwind"))
newdatum1 <- data.frame(Vi = 25, Va = 35, wind = as.factor("downwind"))

predict(fit3, newdatum, interval = "prediction", level = 0.995)
predict(fit3, newdatum1, interval = "prediction", level = 0.995)
