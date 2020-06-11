df <- read.table("mickey.txt")



df$v <- (1 + cos (4*pi/365*df$day))


fit <- lm(waiting.time ~ day.of.the.week:v + v + day.of.the.week, data = df )
summary(fit)
plot(fit)
coefficients(fit)
#alpha0
#
sd(fit$residuals)

shapiro.test(fit$residuals)

#b 

linearHypothesis(fit, rbind(c(0,1,0,1)), c(0)) # yes

#c

fit <- lm(waiting.time ~  v + day.of.the.week, data = df )
summary(fit)

#d

# H0: max = 60
# H1: max != 60

tm <- which.max(fit$coefficients[2]*df$v + fit$coefficients[3]*as.numeric(df$day.of.the.week))
Pred <- predict(fit, df[tm,], interval='confidence', level=1-0.05)  
Pred

#e

Z0.new <- data.frame(day=238, day.of.the.week="weekdays", v = 1 + cos(4*pi/365*238) )

Pred <- predict(fit, Z0.new, interval='prediction', level=1-0.05)  
Pred


