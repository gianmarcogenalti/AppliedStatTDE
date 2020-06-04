library(MASS)

df <- read.table("Sailing.txt")
head(df)

plot(df$water, df$sailing.time, col = df$type, pch = 16)

class <- lda(df[,1:2],df[,3])
pred <- predict(class, df[,1:2])
table(pred$class, df[,3])
pred$posterior[1]

plot(df$water, df$sailing.time, col = pred$class, pch = 16)

class2 <- qda(df[,1:2],df[,3])
pred2 <- predict(class2, df[,1:2])
table(pred2$class, df[,3])
pred2$posterior[1]

plot(df$water, df$sailing.time, col = pred2$class, pch = 16)

errors <- (pred$class != df[,3])
APER   <- sum(errors)/length(df[,3])
APER

errors2 <- (pred2$class != df[,3])
APER2   <- sum(errors)/length(df[,3])
APER2

# i due predittori performano nello stesso modo

new <- data.frame(water = 35, sailing.time = 168)
guess1 <- predict(class, new) # seadog with 0.94 of posterior
guess2 <- predict(class2, new) # vacationer with 0.57 of posterior

# so it depends on the classifier and on the assumptions over variances



