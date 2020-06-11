df <- read.table('airport.txt')

fit <- lm(duration ~ time.of.the.day*distance, data = df )
summary(fit)
shapiro.test(fit$residuals)
vif(fit)
# barely gaussian

library(car)
linearHypothesis(fit, rbind(c(1,0,0,0,0,0),c(0,1,0,0,0,0),c(0,0,1,0,0,0)), c(0,0,0,0,0))
# non sembra influire il giorno da solo
linearHypothesis(m, rbind(c(0,0,0,1,0,0),c(0,0,0,0,1,0),c(0,0,0,0,0,1)), c(0,0,0))
# influisce!

fit <- lm(duration ~ time.of.the.day:distance, data = df )
summary(fit)
shapiro.test(fit$residuals)
# capire se si intercetta o no -> difficile perché R^2 perde valore

pred <- data.frame(distance = 57, time.of.the.day = '6-10')
z  <- predict(fit, pred, interval = 'confidence', level = 0.99)

# prendo l'upper bound -> devo essere lì per le 9:30 -> 114 minuti prima
