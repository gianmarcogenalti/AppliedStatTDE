df <- read.table('tide.txt')

fit <- lm(h ~ I(sin(2*pi/28*t))+I(sin(pi/365*(t-82)))+ t, data = df)
summary(fit)
vif(fit)
coefficients(fit)
sd(fit$residuals)
shapiro.test(fit$residuals)

# b

linearHypothesis(fit,rbind(c(0,1,0,0),c(0,0,1,0)))
linearHypothesis(fit,c(0,0,0,1))

# c
fit2 <- lm(h ~ I(sin(2*pi/28*t))+I(sin(pi/365*(t-82))), data = df)
summary(fit2)
vif(fit2)
coefficients(fit2)
sd(fit2$residuals)
shapiro.test(fit2$residuals)

# d

k <- 2
alpha = 0.1

newdata1 <- data.frame(t = 263)
newdata2 <- data.frame(t = 335)

predict(fit2,newdata1,interval='prediction',level = 1 - alpha/k)
predict(fit2,newdata2,interval='prediction',level = 1 - alpha/k)
