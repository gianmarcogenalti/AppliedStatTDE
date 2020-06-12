rm(list = ls())
df <- read.table('mouse.txt')
head(df)

mod <- glm(type ~ time + I(time^2), data = df, family = binomial(link = 'logit'))
summary(mod)
plot(df$time, as.numeric(df$type)-1, col = df$type, pch = 16)
lines(1:12, predict(mod, data.frame(time = 1:12, type='response')), col = 'forestgreen')
abline(h = 0.5, col = 'red')

newdat <- data.frame(time = 5)
predict(mod, newdat)
# log(pR/pG) = 0.4428711 -> pR = pG * exp(0.4428711) circa 1.5 * pG