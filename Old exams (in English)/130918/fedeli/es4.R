df <- read.table('Lumieres.txt')

fit <- lm(N.people ~ rain + day + I(day^2) + temperature, data = df)
summary(fit)
vif(fit)
shapiro.test(fit$residuals)
coefficients(fit)
sd(fit$residuals)
#b01 174.1
#b02 168.9
#b1 5
#b2 -0.05
#b3 -0.31
#sigma  20.36

# b
library(car)
linearHypothesis(fit, rbind(c(0,0,1,0,0), c(0,0,0,1,0)), c(0,0))

# c

linearHypothesis(fit, rbind(c(0,1,0,0,0), c(0,0,0,0,1)), c(0,0))
fit2 <- lm(N.people ~ day + I(day^2),data = df)
summary(fit2)

# d

# trovare il massimo -> derivata
A <- c(0,1,2*61)
b <- 0

linearHypothesis(fit2,A,b)

new <- data.frame(day = 58)
pred <- predict.lm(fit2, new, interval = 'prediction')
pred
