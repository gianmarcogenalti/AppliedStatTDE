rm(list = ls())
df <- read.table('tennis.txt')
head(df)
boxplot(velocita~ punteggio, data = df)
boxplot(velocita~ sole, data = df)
boxplot(velocita~ punteggio + sole, data = df)

mod1 <- aov(velocita~punteggio+sole, data = df)
summary(mod1)
mod1$coefficients

summary(aov(velocita~punteggio, data = df))
summary(aov(velocita~sole, data = df))

mod2 <- aov(velocita~punteggio, data = df)
summary(mod2)
mod2$coefficients

V <- df[which(df$punteggio == 'vantaggio'),1]
M <- mean(V)
t.test(V)
#95 percent confidence interval:
#   195.3193 208.5407
