rm(list = ls())
df <- read.table('piadeina.txt')
head(df)

mod <- lm(Sales ~ Day.of.Week + Bread.Sold + Wraps.Sold + Sandwich.Sold + Focaccia.Sold + 
            Piadina.Sold + Chips.Sold + Juices.Sold + Total.Soda.and.Coffee.Sold + Max.Daily.Temperature,
            data = df)

summary(mod)
vif(mod)
shapiro.test(mod$residuals)
par(mfrow = c(2,2))
plot(mod)

coefs <- coef(mod)

beta.0 <- coefs[1] + coefs[2]
beta.1 <- coefs[1] + coefs[4]
beta.2 <- coefs[1] + coefs[5]
beta.3 <- coefs[1] + coefs[3]
beta.4 <- coefs[1]
beta.5  <- coefs[6]
beta.6  <- coefs[7]
beta.7  <- coefs[8]
beta.8  <- coefs[9]
beta.9  <- coefs[10]
beta.10  <- coefs[11]
beta.11  <- coefs[12]
beta.12 <- coefs[13]
beta.13 <- coefs[14]
sigma <- sd(mod$residuals)

library(glmnet)

x_vars <- model.matrix(Sales ~. , df)[,-1]
y_var <- df$Sales
lambda <- 5

lasso <- glmnet(x_vars, y_var, alpha = 1, lambda = lambda)
pred <- predict(lasso, s = lambda, type = 'coefficients')

# 14 x 1 sparse Matrix of class "dgCMatrix"
# (Intercept)                29.5749012
# Day.of.WeekMon              .        
# Day.of.WeekThu              0.5874999
# Day.of.WeekTue             10.4478449
# Day.of.WeekWed              .        
# Bread.Sold                  .        
# Wraps.Sold                  0.3740994
# Sandwich.Sold               0.2264938
# Focaccia.Sold               .        
# Piadina.Sold                5.4463405
# Chips.Sold                  .        
# Juices.Sold                 .        
# Total.Soda.and.Coffee.Sold  1.9728695
# Max.Daily.Temperature       .  

lambda.grid <- seq(0,100,length=100)

cv.lasso <- cv.glmnet(x_vars,y_var,alpha=1,nfolds=3,lambda=lambda.grid)

bestlam.lasso <- cv.lasso$lambda.min
# lambda* = 14.14141

lasso.best <- glmnet(x_vars, y_var, alpha = 1, lambda = bestlam.lasso)
pred.best <- predict(lasso.best, s = bestlam.lasso, type = 'coefficients')

# 14 x 1 sparse Matrix of class "dgCMatrix"
# (Intercept)                62.00027686
# Day.of.WeekMon              .         
# Day.of.WeekThu              .         
# Day.of.WeekTue              .         
# Day.of.WeekWed              .         
# Bread.Sold                  .         
# Wraps.Sold                  0.01786128
# Sandwich.Sold               .         
# Focaccia.Sold               .         
# Piadina.Sold                0.34453197
# Chips.Sold                  .         
# Juices.Sold                 .         
# Total.Soda.and.Coffee.Sold  1.67172268
# Max.Daily.Temperature       .  




