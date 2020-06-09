df <- read.table('focaccia.txt')

## a

df_end <- df[df$day=="weekend",]
df_week <- df[df$day!="weekend",]

fit_end <- lm(kg ~ t, data = df_end)
summary(fit_end)
plot(fit_end)
shapiro.test(fit_end$residuals)


fit_week <- lm(kg ~ t, data = df_week)
summary(fit_week)
plot(fit_week)
shapiro.test(fit_week$residuals)

D <- ifelse(df$day == "weekday", 1, 0)

fit <- lm(kg ~ D*t , data = df) # d e t sono messi automaticamente
summary(fit)
plot(fit)
shapiro.test(fit$residuals)
sigma(fit)
fit$coefficients


## b

C <- rbind(c(0,1,0,0), c(0,0,0,1))
C
linearHypothesis(fit, C)

C <- rbind(c(0,0,0,1))
C
linearHypothesis(fit, C)


#anova <- aov(kg ~ D, data = df)
#summary(anova)

#anova <- aov(kg ~ D*t - D - t, data = df)
#summary(anova)

# c
fit <- lm(kg ~ D + t, data = df)
summary(fit)
plot(fit)
shapiro.test(fit$residuals)
sigma(fit)
fit$coefficients

# d 


C <- rbind(c(0,1,0))
C
linearHypothesis(fit, C, 60)
# capire che è sta robaccia

new.kg <- kg - 60*D

fit <- lm(new.kg ~ t)
summary(fit)

#s2.1 <- sum(residuals(fit3)^2)/(n - 2)
#s2.1

# (e)

prediction <- predict(fit, data.frame(t = 61))
prediction

plot(t, new.kg)
abline(fit)



