df <- read.table("tide.txt")
head(df)
attach(df)
plot(t,h)
mod <- lm(h ~ I(sin(2*pi*t/28)) + I(sin(pi*(t-82)/365)) + t)
detach(df)
par(mfrow = c(2,2))
plot(mod)
shapiro.test(mod$residuals)
coefs <- coef(mod)
# (Intercept)            I(sin(2 * pi * t/28))       I(sin(pi * (t - 82)/365))             t 
# 66.49527511                 19.21718913                   1.69733063                0.02371382 
sigma <- sd(mod$residuals)
beta0 <- coefs[1]
beta1 <- coefs[2]
beta2 <- coefs[3]
beta3 <- coefs[4]

linearHypothesis(mod, rbind(c(0,1,0,0), c(0,0,1,0)), c(0,0))
# Linear hypothesis test
# 
# Hypothesis:
# I(sin(2 * pi * t/28)) = 0
# I(sin(pi * (t - 82)/365)) = 0
# 
# Model 1: restricted model
# Model 2: h ~ I(sin(2 * pi * t/28)) + I(sin(pi * (t - 82)/365)) + t
# 
# Res.Df   RSS Df Sum of Sq      F    Pr(>F)    
# 1    201 50128                                  
# 2    199 15779  2     34348 216.59 < 2.2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# seems like periodic components influence the sea level

linearHypothesis(mod, c(0,0,0,1), 0)
# Linear hypothesis test
# 
# Hypothesis:
#   t = 0
# 
# Model 1: restricted model
# Model 2: h ~ I(sin(2 * pi * t/28)) + I(sin(pi * (t - 82)/365)) + t
# 
# Res.Df   RSS Df Sum of Sq      F  Pr(>F)  
# 1    200 16071                              
# 2    199 15779  1     292.1 3.6838 0.05637 .
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# with a confidence of 90% we can say that also global sea level influence the sea level in venice lagoon,
# but we cannot say that with confidences of 95% and 99% so it's pretty borderline but we have to accept the
# null hypothesis
attach(df)
mod2 <- lm(h ~ I(sin(2*pi*t/28)) + I(sin(pi*(t-82)/365))) 
detach(df)
par(mfrow = c(2,2))
plot(mod2)
shapiro.test(mod2$residuals)
coefs2 <- coef(mod2)
# (Intercept)       I(sin(2 * pi * t/28))       I(sin(pi * (t - 82)/365)) 
# 68.749513                 19.251118                  6.066023 
sigma <- sd(mod2$residuals)
beta0 <- coefs2[1]
beta1 <- coefs2[2]
beta2 <- coefs2[3]

t1 <- 263
t2 <- 335

new1 <- data.frame(t= t1)
new2 <- data.frame(t= t2)

predict.lm(mod2, new1, interval = 'prediction',level = 0.9)
#   fit      lwr      upr
# 86.81791 71.89998 101.7358
predict.lm(mod2, new2, interval = 'prediction',level = 0.9)
#   fit      lwr      upr
# 69.44883 54.58102 84.31664

# On the first of December there will not be problems of high water, since even the upper bound of the prediction
# is less than the treshold of 90 from which we consider the water too high. Anyways, on the 20th of September we
# have the fit very near to 90 (86.81) and the upper bound even bigger than 100. So there's a solid chance that we
# will have high water on that day.





