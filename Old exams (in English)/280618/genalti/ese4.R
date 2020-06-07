rm(list = ls())
df <- read.table('Hotels.txt')
head(df)
N <- dim(df)[1]

sf <- df$Position == 'Seafront'

C1.new <- rep(0,N)
C1.new[sf] <- 1

white <- df$Season=='WetSeason'
C2.new <- rep(0,N)
C2.new[white] <- 1

# 4 cases:
# females white
FB <- which(C1.new==0 & C2.new==1)
# females not white
FNB <- which(C1.new==0 & C2.new==0)
# males white
MB <- which(C1.new==1 & C2.new==1)
# males not white
MNB <- which(C1.new==1 & C2.new==0)


# colors for the plot
col <- rep(NA,N)
col[FB] <- 'pink'
col[FNB] <- 'red'
col[MB] <- 'light blue'
col[MNB] <- 'blue'
# shape of the dots for the plot
shape <- rep(0,N)
shape[FB] <- 21
shape[FNB] <- 22
shape[MB] <- 23
shape[MNB] <- 24

detach(df)
attach(df)

plot(t, price, main='Scatterplot price vs t', lwd=2,
     xlab='time of year', ylab='price', col = col, pch = shape)

result3 <- lm(price ~ I(1 + cos(4*pi*t/365)) + C1.new + C2.new + I(1 + cos(4*pi*t/365)):C1.new + I(1 + cos(4*pi*t/365)):C2.new)

summary(result3)

shapiro.test(result3$residuals)

vif(result3)

par(mfrow = c(2,2))
plot(result3)

coefs <- coef(result3)
dev.off()

linearHypothesis(result3, rbind(c(0,0,0,1,0,0),
                                c(0,0,0,0,0,1)), c(0,0)) #pvalue < 2.2e-16 la stagionalità sicuramente influisce

linearHypothesis(result3, rbind(c(0,0,1,0,0,0),
                                c(0,0,0,0,1,0)), c(0,0)) #pvalue < 2.2e-16 anche la posizione

linearHypothesis(result3, rbind(c(0,0,0,0,0,1),
                                c(0,0,0,0,1,0)), c(0,0)) # pvalue = 0.02982 non posso asserire al 99% che la 
                                                         # dinamica temporale influisce

result4 <- lm(price ~ I(1 + cos(4*pi*t/365)) + C1.new + C2.new)
summary(result4)

shapiro.test(result4$residuals)

vif(result4)

par(mfrow = c(2,2))
plot(result4)

coefs <- coef(result4)

t.max <- which.max(result4$fitted.values)
max <- predict(result4, df, interval = 'confidence', level = 0.99)[t.max,]
#   fit      lwr      upr 
# 247.4612 240.9923 253.9302 





