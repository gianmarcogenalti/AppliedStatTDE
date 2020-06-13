df <- read.table('time.txt')

fit <- lm(tempo ~ km + mezzo*km + giorno*km, data = df)
summary(fit)

# b

linearHypothesis(fit,c(0,0,0,0,1),0) #si.

# c

linearHypothesis(fit,c(0,1,0,0,0),0) #si.

# d

mezzo_t <- ifelse(df$mezzo == "tram",1,0)
fit <- lm(tempo ~ km + mezzo_t + giorno, data = df)
summary(fit)

# e

newdata <- data.frame(km = 2, giorno = 'festivo', mezzo_t = 0)
predict(fit,newdata, interval = 'none')
