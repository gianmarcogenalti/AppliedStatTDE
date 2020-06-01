setwd("C:/Users/ffede/OneDrive/Desktop/AppliedStatTDE/Old exams (in English)/030717")
garden <- read.table("garden.txt")

attach(garden)

fit <- lm(extension ~ ., data = garden)
summary(fit)
#1442.86, 16.34, 28.69, 13.12, 14.02

plot(residuals(fit))
shapiro.test(residuals(fit))
# the residuals are normal!

plot(fit)

library(corrplot)
corrplot(cor(garden))

library(car)
vif(fit)

C <- rbind(c(0,0,1,0,0), c(0,0,0,1,0))
linearHypothesis(fit, C)

C <- rbind(c(0,1,0,0,0), c(0,0,0,0,1))
linearHypothesis(fit, C)
 
fit <- lm(extension ~ maple + stones, data = garden)
summary(fit)

## se proviamo una ridge
library(glmnet)
x = model.matrix(extension ~ ., data = garden)[,-1]
y = extension

lambda.grid <- 10^seq(5,-3,length=100)
cv.ridge <- cv.glmnet(x,y,lambda=lambda.grid, alpha = 0) # default: 10-fold CV

fit_ridge <- glmnet(x,y,alpha = 0,lambda = cv.ridge$lambda.min)
fit_ridge$dev.ratio
fit_ridge$beta

## se no pc regression

Pc <- princomp(garden[,1:4], scores = T)
summary(Pc)

fit_pc <- lm(extension ~ Pc$scores[,1] + Pc$scores[,2])
summary(fit_pc)

plot(residuals(fit_pc))
shapiro.test(residuals(fit_pc))
plot(fit_pc)
vif(fit_pc)

fit_pc$coefficients[2]*Pc$loadings[,1] + fit_pc$coefficients[3]*Pc$loadings[,2]
# 5259.214 15.64 19.25 21.38 15.08


