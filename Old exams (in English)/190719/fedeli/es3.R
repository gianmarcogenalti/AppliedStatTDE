df <- read.table('piadeina.txt')

fit <- lm(Sales ~ . , data = df)
summary(fit)
plot(fit)
vif(fit)
# vif più bassi di quanto pensassi
shapiro.test(fit$residuals)
# son pure quasi normali

# b
attach(df)
x <- model.matrix(Sales ~ ., data = df)[,-1]
y <- Sales
library(glmnet)
fit.lasso <- glmnet(x,y, alpha = 1, lambda = 5)
fit.lasso$beta

# c

lambda.grid <- seq(0,100,length=10000)
cv.lasso <- cv.glmnet(x,y, alpha = 1, nfolds = 47, lambda = lambda.grid)

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

plot(cv.lasso)
abline(v=log(bestlam.lasso), lty=1)

## Lasso instabile e stupida
# devo fare loocv perché se no, trovandosi su un plateau di merda
# va a selezionare a caso le lambda

fit.lasso <- glmnet(x,y, lambda = bestlam.lasso)
fit.lasso$beta

var <- ifelse(Day.of.Week=="Tue",1,0)
fit <- lm(Sales ~ var + Wraps.Sold + Piadina.Sold + Total.Soda.and.Coffee.Sold, data = df)
summary(fit)
