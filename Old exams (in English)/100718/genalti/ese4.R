df <- read.table("focaccia.txt")

df$wd <- ifelse(df$day == 'weekday', 1,0) #dummy variable
#df$we <- ifelse(df$day == 'weekday', 0,1) #dummy variable
head(df)

mod <- lm(kg ~ wd + t + wd:t, data = df)
summary(mod)
mod$coefficients
# (Intercept)          wd           t        wd:t 
# 108.6863770 -73.4140787   1.6302864   0.5043036 
vif(mod)
# wd        t           wd:t 
# 3.991032 3.716542 7.805284 
sd(mod$residuals)
#13.1188

# beta0.0 = 108.6864 intercept for weekends
# beta0.1 = 108.6864-73.4140787 intercept for weekdays
# beta1.0 = 1.6303 coeff for weekends
# beta1.1 = 1.6303 + 0.5043036 coeff for weekdays
# sigma   = 13.1188 error std.dev

par(mfrow = c(2,2))
plot(mod)
shapiro.test(mod$residuals)

mod <- lm(kg ~ wd + t, data = df)
summary(mod)
vif(mod)
# wd        t 
# 1.047392 1.047392 
par(mfrow = c(2,2))
plot(mod)

linearHypothesis(mod,
                 rbind(c(0,0,1)),
                       0)
# il giorno della settimana influisce sicuramente sul commercio

















