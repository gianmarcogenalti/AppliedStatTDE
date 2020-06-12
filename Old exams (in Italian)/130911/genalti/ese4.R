library(car)
rm(list = ls())
df <- read.table('letter.txt')
head(df)
levels(df$tipo)
plot(df[,2],df[,1],pch = 16, col = df[,3], ylab = 'Tempo', xlab = 'Distanza')
legend(0,9,levels(df$tipo), col = c('black', 'red','green'), pch = 16)

mod <- lm(tempo~tipo + distanza + distanza:tipo, data = df) 
summary(mod)
# Call:
#   lm(formula = tempo ~ tipo + distanza + distanza:tipo, data = df)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
#   -1.35197 -0.37081  0.00544  0.36412  1.90437 
# 
# Coefficients:
#                               Estimate  Std. Error t value Pr(>|t|)    
#   (Intercept)                0.8190054  0.1973661   4.150 5.67e-05 ***
#   tipoprioritaria            0.3384624  0.3072520   1.102    0.272    
#   tiporaccomandata           0.0194287  0.2820957   0.069    0.945    
#   distanza                   0.0022200  0.0001767  12.564  < 2e-16 ***
#   tipoprioritaria:distanza  -0.0012628  0.0001840  -6.862 1.87e-10 ***
#   tiporaccomandata:distanza -0.0010475  0.0002510  -4.174 5.16e-05 ***

par(mfrow = c(2,2))
plot(mod)
vif(mod)
coefs <- coef(mod)

linearHypothesis(mod, c(0,1,1,0,0,0), 0) # pvalue =  0.4769 l'affrancatura non influisce sul tempo base
linearHypothesis(mod, c(0,0,0,0,1,1), 0) # pvalue = 4.271e-08 mentre influisce insieme alla distanza
linearHypothesis(mod, rbind(c(0,0,0,0,1,1),
                            c(0,1,1,0,0,0)), c(0,0))# pvalue < 2.2e-16 nel complesso l'affrancatura influisce sul modello
linearHypothesis(mod, c(0,0,0,1,0,0), 0)# pvalue < 2.2e-16 anche la distanza influisce sul tempo di consegna

# rimuoviamo l'affrancatura in quanto a sé stante ma manteniamo l'interazione con la distanza

mod2 <- lm(tempo~distanza + distanza:tipo, data = df) 
summary(mod2)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                0.9153459  0.1207504    7.58 3.67e-12 ***
#   distanza                   0.0021412  0.0001221   17.54  < 2e-16 ***
#   distanza:tipoprioritaria  -0.0011343  0.0001046  -10.85  < 2e-16 ***
#   distanza:tiporaccomandata -0.0010311  0.0001010  -10.21  < 2e-16 ***

# l'R^2 migliora leggerment e tutte le variabili sono significative adesso
par(mfrow = c(2,2))
plot(mod2)
shapiro.test(mod$residuals)
vif(mod2)
# la diagnosi del modello è ok
alpha = 0.05
k <- 3

newdat1 <- data.frame(distanza = 1000, tipo = 'normale')
newdat2 <- data.frame(distanza = 1000, tipo = 'prioritaria')
newdat3 <- data.frame(distanza = 1000, tipo = 'raccomandata')
guess1 <- predict(mod, newdat1, interval = 'confidence', level = 1-alpha/(2*k))
guess2 <- predict(mod, newdat2, interval = 'confidence', level = 1-alpha/(2*k))
guess3 <- predict(mod, newdat3, interval = 'confidence', level = 1-alpha/(2*k))
guess1bis <- predict(mod2, newdat1, interval = 'confidence', level = 1-alpha/(2*k))
guess2bis <- predict(mod2, newdat2, interval = 'confidence', level = 1-alpha/(2*k))
guess3bis <- predict(mod2, newdat3, interval = 'confidence', level = 1-alpha/(2*k))



