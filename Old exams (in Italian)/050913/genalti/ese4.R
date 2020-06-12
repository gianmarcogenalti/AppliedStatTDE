library(car)
rm(list = ls())
r <- read.table('red.txt')
b <- read.table('black.txt')
df <- rbind(r,b)
df$col <- rep(NA,62)
df[1:31,3] <- 'red'
df[32:62,3] <- 'black'


mod1 <- lm(like ~ col + I(1/giorno) + I(1/giorno):col, data = df)
summary(mod1)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -128.556  -49.431    5.702   42.411  117.671 
# 
# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)          318.78      12.67  25.167  < 2e-16 ***
#   colred               -12.16      17.91  -0.679      0.5    
#   I(1/giorno)         1513.13      55.53  27.250  < 2e-16 ***
#   colred:I(1/giorno)  -506.06      78.53  -6.444  2.5e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 57.97 on 58 degrees of freedom
# Multiple R-squared:  0.9499,	Adjusted R-squared:  0.9473 
# F-statistic: 366.5 on 3 and 58 DF,  p-value: < 2.2e-16
shapiro.test(mod1$residuals)
par(mfrow = c(2,2))
plot(mod1)
vif(mod1)

coefs1 <- coef(mod1)
sigma <- sd(mod1$residuals)
# (Intercept)           colred          I(1/giorno)       colred:I(1/giorno) 
#  318.78201          -12.15989         1513.13303         -506.06392 

linearHypothesis(mod1, c(0,1,0,1), 0)
# Hypothesis:
#   colred  + colred:I(1/giorno) = 0
# 
# Model 1: restricted model
# Model 2: like ~ col + I(1/giorno) + I(1/giorno):col
# 
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1     59 379679                                  
# 2     58 194926  1    184752 54.973 5.897e-10 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# l'apprezzamento è diverso tra rossi e neri all'istante t = 1

linearHypothesis(mod1, c(0,1,0,0), 0)

# Hypothesis:
#   colred = 0
# 
# Model 1: restricted model
# Model 2: like ~ col + I(1/giorno) + I(1/giorno):col
# 
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     59 196475                           
# 2     58 194926  1    1548.6 0.4608    0.5

# mentre per t -> infinito abbiamo che non c'è differenza nell'andamento dell'apprezzamento

mod2 <- lm(like ~ col + I(1/giorno), data = df)
summary(mod2)
shapiro.test(mod2$residuals)
par(mfrow = c(2,2))
plot(mod2)
vif(mod2)

alpha = 0.1
k <- 2
newdat1 <- data.frame(giorno = 36, col = 'red')
newdat2 <- data.frame(giorno = 36, col = 'black')
guess1 <- predict(mod2, newdat1, interval = 'prediction', level = 1-alpha/k)
#    fit      lwr     upr
# 308.7533 155.3206 462.186
guess2 <- predict(mod2, newdat2, interval = 'prediction', level = 1-alpha/k)
#    fit      lwr      upr
# 386.6565 233.2238 540.0892




