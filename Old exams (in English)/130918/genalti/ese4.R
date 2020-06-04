library(car)

df <- read.table("Lumieres.txt")
head(df)
attach(df)
r <- ifelse(rain == 'yes', 1, 0)
day.2 <- day^2
mod <- lm(N.people ~  r + day + day.2 + temperature)
detach(df)

coefs <- coef(mod)
# (Intercept)            r          day        day.2  temperature 
# 174.10940963  -5.25620281   5.00795485  -0.04295472  -0.31875373 

beta.00 <- coefs[1]
beta.01 <- coefs[1] + coefs[2]
beta.1  <- coefs[3]
beta.2  <- coefs[4]
beta.3  <- coefs[5]
sigma   <- sd(mod$residuals)

shapiro.test(mod$residuals)
par(mfrow = c(2,2))
plot(mod)

linearHypothesis(mod, c(0,0,1,0,0), 0) # pvalue = 0, day is absolutely significative
summary(mod)
# Call:
#   lm(formula = N.people ~ r + day + day.2 + temperature)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -43.051 -15.770   0.137  15.442  41.816 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 174.109410  20.543962   8.475 3.09e-11 ***
#   r            -5.256203   6.680240  -0.787    0.435    
#   day           5.007955   0.388882  12.878  < 2e-16 ***
#   day.2        -0.042955   0.004258 -10.087 1.20e-13 ***
#   temperature  -0.318754   0.795187  -0.401    0.690    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 21.16 on 50 degrees of freedom
# Multiple R-squared:  0.8437,	Adjusted R-squared:  0.8312 
# F-statistic: 67.48 on 4 and 50 DF,  p-value: < 2.2e-16

# anyways temperature and presence of rain seem not to influence the model
attach(df)
mod2 <- lm(N.people ~  day + day.2)
detach(df)
summary(mod2)

coefs2 <- coef(mod2)
beta.0  <- coefs[1]
beta.1  <- coefs[2]
beta.2  <- coefs[3]
sigma   <- sd(mod2$residuals)

shapiro.test(mod2$residuals)
par(mfrow = c(2,2))
plot(mod2)

# faccio la derivata del modello e verifico che in day = 61 si annulla

linearHypothesis(mod2, c(0,1,2*61), 0) # pval = 0.20 quindi la derivata è nulla in day = 61

new <- data.frame(day = 58, day.2 = 58^2)
pred <- predict.lm(mod2, new, interval = 'prediction')
pred


