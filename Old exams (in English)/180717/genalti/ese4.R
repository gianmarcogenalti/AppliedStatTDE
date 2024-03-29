df <- read.table("albatross.txt")
head(df)

mod <- lm(distance ~ wind + I(Va^2) + I(Va^2):wind + I(Vi^2) + I(Vi^2):wind, data = df)
summary(mod)
coefs <- coef(mod)
alpha.1 <- coefs[1] + coefs[2]
alpha.2 <- coefs[1]
beta.1  <- coefs[3] + coefs[2]
beta.2  <- coefs[3]
gamma.1 <- coefs[4] + coefs[2]
gamma.2 <- coefs[4]
sigma   <- mod$residuals
summary(mod)

shapiro.test(mod$residuals)
par(mfrow = c(2,2))
plot(mod)

vif(mod)
#     wind         I(Va^2)      I(Vi^2)   wind:I(Va^2) wind:I(Vi^2) 
#   27.337153     2.454626     2.842735    44.262826    16.203174 

mod2 <- lm(distance ~ wind + I(Va^2) + I(Vi^2), data = df)
summary(mod2)

shapiro.test(mod2$residuals)
par(mfrow = c(2,2))
plot(mod2)

vif(mod2)

# abbiamo risolto i problemi di collinearit� e mantenuto un modello dalle ipotesi verificate

linearHypothesis(mod2, c(0,0,1,1), 0)

# rifiuto H1, quindi accetto che siano opposti beta.1 e gamma.1

mod3 <- lm(distance ~ wind + I(Va^2-Vi^2), data = df)
summary(mod3)

shapiro.test(mod3$residuals)
par(mfrow = c(2,2))
plot(mod3)

vif(mod3)

# ok il modello � lecito

coefs2 <- coef(mod3)

alpha.1.bis <- coefs[1] + coefs[2]
alpha.2.bis <- coefs[1]
beta.bis    <- coefs[3]
gamma.bis   <- coefs[4]

newdata.up <- data.frame(wind = "upwind", Va = 35, Vi = 25)
newdata.down <- data.frame(wind = "downwind", Va = 35, Vi = 25)

pred1 <- predict(mod3, newdata.up, interval = 'prediction', conf.level = 0.99)
pred2 <- predict(mod3, newdata.down, interval = 'prediction', conf.level = 0.99)

pred1 # con upwind l'atterraggio sar� sicuro in quanto l'upper bound nemmeno si avvicina ai 17 m necessari
pred2 # con downwind non si potr� garantire la sicurezza nell'atterraggio, l'upper bound � decisamente pi� alto
      # (22.26 m) e la predizione puntuale stessa � maggiore di 17 m essendo 17.92 m



