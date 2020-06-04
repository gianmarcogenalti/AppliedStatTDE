library(car)

df <- read.table("mickey.txt")
head(df)

t <- df$day
g <- ifelse(df$day.of.the.week == 'weekdays', 1, 0)
w <- df$waiting.time

plot(t, w)

mod <- lm(w ~ g + I(1 + cos(4*pi*t/365)) + I(1 + cos(4*pi*t/365)):g)
summary(mod)
# Call:
#   lm(formula = w ~ g + I(1 + cos(4 * pi * t/365)) + I(1 + cos(4 * 
#                                                                 pi * t/365)):g)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -19.6727  -5.3630  -0.9767   5.5343  24.2404 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    34.336      1.820  18.861  < 2e-16 ***
#   g                             -17.454      2.088  -8.361 5.01e-15 ***
#   I(1 + cos(4 * pi * t/365))     13.023      1.521   8.561 1.34e-15 ***
#   g:I(1 + cos(4 * pi * t/365))    2.483      1.741   1.426    0.155    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 8.412 on 241 degrees of freedom
# Multiple R-squared:  0.6969,	Adjusted R-squared:  0.6931 
# F-statistic: 184.7 on 3 and 241 DF,  p-value: < 2.2e-16

sigma <- sd(mod$residuals)

# sigma = 8.360269
# alpha0 = 34.336          residents during weekends
# alpha1 = 34.336 -17.454  residents during weekdays
# beta0 = 13.023           tourists during weekends
# beta1 = 13.023 + 2.483   tourists during weekdays 

# tourists contribute more during weekdays, while residents go to disneyland much more during weekends


par(mfrow = c(2,2))
plot(mod)
shapiro.test(mod$residuals)

vif(mod)

# ho buona evidenza per rimuovere l'interazione tra giorno della settimana e tempo, in quanto presenta collinearità
# e poca significatività, questo significa che il giorno della settimana è non influisce sul contributo dei turisti
# ma ha buona significatività per quanto riguarda i residenti

mod2 <- lm(w ~ g + I(1 + cos(4*pi*t/365)))
summary(mod2)

# Call:
#   lm(formula = w ~ g + I(1 + cos(4 * pi * t/365)))
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -19.177  -5.417  -1.149   5.460  23.847 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 32.5155     1.3006   25.00   <2e-16 ***
#   g                          -15.0697     1.2524  -12.03   <2e-16 ***
#   I(1 + cos(4 * pi * t/365))  14.9183     0.7414   20.12   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 8.43 on 242 degrees of freedom
# Multiple R-squared:  0.6943,	Adjusted R-squared:  0.6918 
# F-statistic: 274.8 on 2 and 242 DF,  p-value: < 2.2e-16
# 
plot(t, w)

par(mfrow = c(2,2))
plot(mod2)
shapiro.test(mod2$residuals)

vif(mod2)

# modello migliorato

sigma <- sd(mod2$residuals)
# sigma = 8.395469
# alpha0 = 32.5155           residents during weekends
# alpha1 = 32.5155 - 15.0697 residents during weekdays
# beta = 14.9183             tourists (always)

tm <- which.max(t*cos(4*pi*t/365)) # 122
isweekday <- g[122]
t.max <- data.frame(t =  350, g = isweekday)
Conf <- predict(mod2, t.max, interval='prediction', level=1-0.05) 
Conf


# abbiamo verificato che i residui si distribuiscono in modo normale con varianza sigma^2: possiamo procedere
# alla prediction per t = 238

t0.new <- data.frame(t = 238, g = 1)
Pred <- predict(mod2, t0.new, interval='prediction', level=1-0.05)  
# lwr          fit          upr
# 10.73324     27.38931     44.04538
abline(h = 44.04)
abline(v = 238)
abline(h = 10.7)
