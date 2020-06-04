df <- read.table('garden.txt')
head(df)

mod <- lm(extension ~ carps + maple + cherry + stones, data = df)
summary(mod)

# coefficienti stimati
mod$coefficients

# analisi dei residui
par(mfrow = c(2,2))
plot(mod)
shapiro.test(mod$residuals) # i residui sono normali e sufficientemente omoschedastici, non pare esserci effetto leva

# vediamo il contributo degli alberi (maple + cherry) sull'extension
linearHypothesis(mod,c("maple = 0", "cherry = 0"))
# Linear hypothesis test
# 
# Hypothesis:
#   maple = 0
# cherry = 0
# 
# Model 1: restricted model
# Model 2: extension ~ carps + maple + cherry + stones
# 
# Res.Df      RSS Df Sum of Sq      F    Pr(>F)    
# 1    153 27158445                                  
# 2    151 14899093  2  12259352 62.123 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# gli alberi contribuiscono

# vediamo il contributo delle carpe e dei sassi sull'extension
linearHypothesis(mod,c("carps = 0", "stones = 0"))

# Linear hypothesis test
# 
# Hypothesis:
#   carps = 0
# stones = 0
# 
# Model 1: restricted model
# Model 2: extension ~ carps + maple + cherry + stones
# 
# Res.Df      RSS Df Sum of Sq      F    Pr(>F)    
# 1    153 23127179                                  
# 2    151 14899093  2   8228086 41.695 3.822e-15 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# anche i laghetti contribuiscono

# i due set di regressori, elementi di laghetto e alberi, sono entrambi molto influenti sull'estensione del giardino
# tuttavia il modello completo a quattro regressori presenta forti collinearità come testimoniato dai vif molto alti
vif(mod)

# riduciamo il modello

mod1 <- lm(extension ~ carps + stones, data = df)
summary(mod1)
vif(mod1)

mod2 <- lm(extension ~ maple + cherry, data = df)
summary(mod2)
vif(mod2)

# sono ancora collineari, prendiamo la più significativa tra le variabili in ognuno: carps e maple

mod3 <- lm(extension ~ maple + carps, data = df)
summary(mod3)
vif(mod3)

# adesso abbiamo eliminato le collinearità e abbiamo forte signiticatività nei regressori del modello
# gli elementi di laghetto e gli alberi sono tra loro interconnessi, quindi ne abbiamo scelto uno solo
# come rappresentante della tipologia di elemento, stimandone i coefficienti

plot(mod3)
shapiro.test(mod3$residuals)

# il modello ridotto soddisfa inoltre tutte le ipotesi necessarie

linearHypothesis(mod3, c("carps = 0", "maple = 0"))

# e abbiamo ottima evidenza per dire che i coefficienti siano diversi da 0

