library(car)
rm(list = ls())
mao <- read.table('mao.txt')
panda <- read.table('panda.txt')
pagoda <- read.table('pagoda.txt')
df <- rbind(mao,pagoda,panda)
df$type <- rep(NA,30)
df[1:10,3] <- 'mao'
df[11:20,3] <- 'panda'
df[21:30,3] <- 'pagoda'
head(df)

mod1 <- lm(cost~ quantity + type + quantity:type, data = df )
summary(mod1)
par(mfrow = c(2,2))
plot(mod1)
vif(mod1)
coefs1 <- coef(mod1)

linearHypothesis(mod1, c(0,0,0,1,0,0), 0)

linearHypothesis(mod1, c(0,0,1,0,0,0), 0) # i pvalue sono molto alti e ci portano a pensare che i costi fissi siano gli stessi

linearHypothesis(mod1, c(0,0,0,0,1,0), 0)

linearHypothesis(mod1, c(0,0,0,0,0,1), 0) # qui invece sono abbastanza bassi da portarci a pensare che siano differenti

mod2 <- lm(cost~ quantity + quantity:type, data = df )
summary(mod2)
par(mfrow = c(2,2))
plot(mod2)
vif(mod2)
coefs2 <- coef(mod2)

newdata <- rbind(data.frame(quantity = 2500, type = 'mao'), data.frame(quantity = 4000, type = 'panda'), data.frame(quantity = 1000, type = 'pagoda'))

k <- 3
alpha <- 0.1
guess <- predict(mod2, newdata, interval = 'prediction', conf.level = 1 - alpha/k)

plot(df[,1], df[,2])



