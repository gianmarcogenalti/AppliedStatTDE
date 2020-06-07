rm(list = ls())
df <- read.table('kimonos.txt')
head(df)

mod <- lm(price ~ q + l:q + ncolors:q + l + ncolors, data = df)
summary(mod)
# Call:
# lm(formula = price ~ q + l:q + ncolors:q + l + ncolors, data = df)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -446.22  -91.16    5.06  100.66  377.22 
# 
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       127.78     101.67   1.257    0.210    
# qmedium            64.16     130.05   0.493    0.622    
# l                 612.04      12.99  47.134   <2e-16 ***
# ncolors            10.46      14.93   0.700    0.484    
# qmedium:l        -381.32      15.96 -23.888   <2e-16 ***
# qmedium:ncolors    23.11      22.13   1.044    0.298    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 143.1 on 242 degrees of freedom
# Multiple R-squared:  0.9911,	Adjusted R-squared:  0.9909 
# F-statistic:  5386 on 5 and 242 DF,  p-value: < 2.2e-16

par(mfrow = c(2,2))
plot(mod)

shapiro.test(mod$residuals) #pvalue = 0.8437

vif(mod)
#     q         l      ncolors    q:l    q:ncolors 
#51.160701  3.076621  1.859862 41.280003 15.387722 

coefs <- coef(mod)
# (Intercept)         qmedium           l             ncolors       qmedium:l      qmedium:ncolors 
#   127.77614        64.16103       612.04154        10.45616      -381.31690        23.10725 

alpha.1 <- coefs[1]
alpha.2 <- coefs[1] + coefs[2]
beta.1  <- coefs[3]
beta.2  <- coefs[3] + coefs[5]
gamma.1 <- coefs[4]
gamma.2 <- coefs[4] + coefs[6]
sigma   <- sd(mod$residuals)

linearHypothesis(mod, c(0,1,0,0,0,0),0, conf=0.9) # non significativa pval = 0.6222

linearHypothesis(mod, c(0,0,0,1,0,1),0, conf = 0.9) # non significativa pval = 0.4843

mod2 <- lm(price ~ l:q + l + ncolors, data = df)
summary(mod2)

shapiro.test(mod2$residuals)

vif(mod2)

par(mfrow = c(2,2))
plot(mod2)

# tutte le ipotesi sono verificate

coefs <- coef(mod2)
#(Intercept)     l         ncolors   l:qmedium 
#186.58755   599.87412    21.81065  -363.55096 

alpha  <- coefs[1]
beta.1 <- coefs[2]
beta.2 <- coefs[2] + coefs[4]
gamma  <- coefs[3]
sigma  <- sd(mod2$residuals)

alpha = 0.1
k <- 3

newdat1 <- data.frame(l = 0, ncolors = 0, q = 'medium')
pred1 <- predict(mod2, newdat1, interval = 'confidence', level = 1-alpha/k)
#    fit      lwr      upr
# 186.5876 64.85475 308.3204

newdat2 <- data.frame(l = 6.5, ncolors = 3, q = 'medium')
pred2 <- predict(mod2, newdat2, interval = 'confidence', level = 1-alpha/k)
#   fit      lwr      upr
# 1788.12 1762.359 1813.881

newdat3 <- data.frame(l = 6.5, ncolors = 3, q = 'high')
pred3 <- predict(mod2, newdat3, interval = 'confidence', level = 1-alpha/k)
#    fit     lwr      upr
# 4151.201 4124.03 4178.372


