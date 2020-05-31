rm(list = ls()) 
data = read.table("Lumieres.txt")
head(data)
data$rain = as.factor(data$rain)

rm <- lm(N.people ~ rain + day + I(day^2) + temperature,data = data)
summary(rm)

library(car)
linearHypothesis(rm, rbind(c(0,0,1,0,0), c(0,0,0,1,0)), c(0,0))

# c)
linearHypothesis(rm, rbind(c(0,1,0,0,0), c(0,0,0,0,1)), c(0,0))
rmr <- lm(N.people ~ day + I(day^2),data = data)
summary(rmr)

# d)
A <- c(0,1,2*61)
b <- 0

linearHypothesis(rmr,A,b)

rm1 <- lm(N.people ~ I(day^2 - 122*day), data = data)
summary(rm1)

# e)
newdata <- data.frame(day = 58, temperature = 29, rain = as.factor("no"))
predict.lm(rm1, newdata, interval = "prediction", level = 0.95)
