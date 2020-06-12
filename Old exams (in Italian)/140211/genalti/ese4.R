library(car)
rm(list = ls())
df1 <- read.table('BL1.txt')
head(df1)
dim(df1)
df2 <- read.table('BL2.txt')
head(df2)
dim(df2)
df3 <- read.table('BL3.txt')
head(df3)
dim(df3)
df <- rbind(df1,df2,df3)
df$pozzo <- rep(NA,90)
df[1:30,3] <- '1'
df[31:60,3] <- '2'
df[61:90,3] <- '3'
mod <- lm(temperatura ~ profondita + pozzo + pozzo:profondita, data = df)
x11()
plot(df[,1],df[,2],pch = 16, col = df[,3])
legend(20,1500, c('1','2','3'), c('red','black','green')) # a occhio ci sembrano molto simili 1 e 2 ma non 3
summary(mod)
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)       20.3854713  0.3506802  58.131  < 2e-16 ***
#   profondita         0.0097947  0.0003951  24.792  < 2e-16 ***
#   pozzo2            -0.4514483  0.4959367  -0.910 0.365272    
#   pozzo3             1.9411724  0.4959367   3.914 0.000184 ***
#   profondita:pozzo2  0.0001748  0.0005587   0.313 0.755198    
#   profondita:pozzo3 -0.0044002  0.0005587  -7.876 1.08e-11 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9365 on 84 degrees of freedom
# Multiple R-squared:  0.9463,	Adjusted R-squared:  0.9431 
# F-statistic: 295.8 on 5 and 84 DF,  p-value: < 2.2e-16

shapiro.test(mod$residuals)
sigma <- sd(mod$residuals)
vif(mod)
par(mfrow = c(2,2))
plot(mod)

# le ipotesi del modello sono verificate, tuttavia abbiamo collinearità e fattori poco rilevanti
coefs <- coef(mod)

linearHypothesis(mod, c(0,1,0,0,1,1), 0)# pval = 3.198e-12 la profondità influisce sulla temperatura

linearHypothesis(mod, c(0,0,0,0,1,0),0)# pval  = 0.7552, 1 e 2 pescano dallo stesso giacimento
linearHypothesis(mod, c(0,0,0,0,0,1),0)# pval  = 1.075e-11, 1 e 3 non pescano dallo stesso giacimento
linearHypothesis(mod, c(0,0,0,0,-1,1),0)# pval = 2.542e-12, 2 e 3 non pescano dallo stesso giacimento
linearHypothesis(mod, c(0,0,1,0,0,0),0)# pval = 0.3653, il pozzo 2 non influisce nemmeno sulla temperatura base
# ci sono due giacimenti: 1 e 2, 3

df[1:60,3] <- '1'
df[61:90,3] <- '3'
mod2 <- lm(temperatura ~ profondita + pozzo + pozzo:profondita, data = df)
x11()
plot(df[,1],df[,2],pch = 16, col = df[,3])
legend(20,1500, c('1','3'), c('red','green')) # a occhio ci sembrano molto simili 1 e 2 ma non 3
summary(mod2)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       20.1597471  0.2476883  81.392  < 2e-16 ***
#   profondita         0.0098820  0.0002790  35.415  < 2e-16 ***
#   pozzo3             2.1668966  0.4290086   5.051 2.44e-06 ***
#   profondita:pozzo3 -0.0044876  0.0004833  -9.285 1.29e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9354 on 86 degrees of freedom
# Multiple R-squared:  0.9451,	Adjusted R-squared:  0.9432 
# F-statistic: 493.4 on 3 and 86 DF,  p-value: < 2.2e-16
shapiro.test(mod2$residuals)
sigma <- sd(mod2$residuals)
vif(mod2)
par(mfrow = c(2,2))
plot(mod2)
# le ipotesi del modello sono verificate, l'R^2 è leggermente aumentato ed abbiamo rimosso collinearità
coefs2 <- coef(mod2)

k <- 3
alpha <- .1
n <- 90
r <- 3

Z0   <- data.frame(profondita = 200, pozzo = '1')
ICBmean1 <- predict(mod2, Z0, interval='confidence',level=1-alpha/k) 

Z0   <- data.frame(profondita = 200, pozzo = '3')
ICBmean2 <- predict(mod2, Z0, interval='confidence',level=1-alpha/k) 
ICBmean <- rbind(ICBmean1, ICBmean2)

e <- residuals(mod2)
ICBvar <- data.frame(L=t(e)%*%e/qchisq(1-alpha/(2*k),n-(r+1)),
                     U=t(e)%*%e/qchisq(alpha/(2*k),n-(r+1)))
ICBvar

