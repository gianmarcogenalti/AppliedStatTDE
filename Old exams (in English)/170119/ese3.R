library(MASS)

df <- read.table("trading.txt")
n <- dim(df)[1]
head(df)

class <- qda(df[,1:2], df[,3])
class$prior
#      0        1 
# 0.4661355 0.5338645 
M <- rbind(c(mean(df[which(df$gain == 0),1]),mean(df[which(df$gain == 1),1])),
           c(mean(df[which(df$gain == 0),2]),mean(df[which(df$gain == 1),2])))
#       gain = 0 gain = 1
# goog -3.736068 5.1162687
# aapl -0.830094 0.7081866
pred <- predict(class, df[,1:2])
table(pred$class, df[,3])
#    0  1
# 0 84 45
# 1 33 89

errors <- (pred$class != df[,3])
APER.qda <- sum(errors)/n
# 0.310757

x11()
par(mfrow = c(1,2))
plot(df$google, df$apple, pch = 16, col = factor(df$gain), main = 'true')
plot(df$google, df$apple, pch = 16, col = pred$clas, main = 'predicted')

dummy <- rep(1,n)
errors <- (dummy != df[,3])
APER.dummy <- sum(errors)/n
# 0.4661355

classCV <- qda(df[,1:2], df[,3], CV = TRUE)
table(classCV$class, df[,3])
#    0  1
# 0 82 46
# 1 35 88

errors <- (classCV$class != df[,3])
APER.qdaCV <- sum(errors)/n
# 0.3227092

newdatum <- data.frame(google = -3, apple = -1.2)
predict(class, newdatum)
#     0        1
# 0.698252 0.301748
