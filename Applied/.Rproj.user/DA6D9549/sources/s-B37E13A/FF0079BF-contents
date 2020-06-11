df <- read.table('albatross.txt')

fit <- lm(distance ~ wind + wind:I(Vi^2) + wind:I(Va^2), data = df)
summary(fit)
vif(fit)
sd(fit$residuals)

# alpha2 = 3.288
# alpha1 = 4.651
# gamma2  = -0.017
# gamma1  = -0.097
# beta2 = 0.019
# beta1 = 0.011
# sigma  = 1.753

# b

linearHypothesis(fit,c(0,1,0,0,0,0),0) # windupwind inutile

fit2 <- lm(distance ~ wind:I(Vi^2) + wind:I(Va^2), data = df)
summary(fit2)
# tutto significativo

# c

linearHypothesis(fit2,c(0,1,0,-1,0),0)
linearHypothesis(fit2,c(0,0,1,0,-1),0)

#yey!

fit3 <- lm(distance ~ wind:I(Vi^2-Va^2),data = df)
summary(fit3)

# b0 : 4.543
# b11: -0.011
# b12: -0.019

newZ0 <- data.frame(Vi = 25, Va = 35, wind = "upwind")
newZ1 <- data.frame(Vi = 25, Va = 35, wind = "downwind")

k = 2
alpha = 0.01

predict(fit3, newZ0, interval = 'prediction', level = 1-alpha/(2*k))
predict(fit3, newZ1, interval = 'prediction', level = 1-alpha/(2*k))

# it is unsafe, but we do like a bit of pepper, don't we?