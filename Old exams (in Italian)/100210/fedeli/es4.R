df <- read.table('smog.txt')

fit <- lm(PM10 ~ giorno, data = df)
summary(fit)

df$disc <- ifelse((df$giorno>6.5),1,0)
fit2 <- lm(PM10 ~ giorno + giorno : disc, data = df )
summary(fit2)

fit3 <- lm(PM10 ~ giorno + disc + giorno:disc, data = df )
summary(fit3)
vif(fit3)

x11()
plot(x = df$giorno, df$PM10, xlim = c(0,11))
abline(a = coef(fit3)[1], b = coef(fit3)[2], col = 'blue')
abline(a = coef(fit3)[3]+coef(fit3)[1], b = coef(fit3)[2]+coef(fit3)[4], col = 'blue')
points(df$giorno, fitted(fit3), col='red', pch=19)

# fit3 è il migliore

# c 

library(multcomp)
summary(glht(fit3, linfct = c("giorno:disc >= 0")))
# evidenza per dire che è diminuito
linearHypothesis(fit3,c(0,0,0,1),0)

summary(glht(fit3, linfct = c("-0.5*giorno - giorno:disc == 0")))

linearHypothesis(fit3,c(0,-0.5,0,-1),0)
# non c'è evidenza

t <- (-coef(fit3)[4]-0)/sqrt(diag(vcov(fit3))[4])
t
n <- dim(df)[1]
#t_stat <- qt(1-0.05,n-2)

n_reg <- length(coefficients(fit3))

pval <- (1-pt(t,n-n_reg))
pval # unilatero
