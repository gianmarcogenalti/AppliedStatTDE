df <- read.table('debris.txt')

mcshapiro.test(df[df$risk=="H",1:2])
mcshapiro.test(df[df$risk=="L",1:2])

# no normality for low

var(df[df$risk=="L",1:2])
var(df[df$risk=="H",1:2])

plot(df$x,df$y,col = df$risk)

library(MASS)

fit <- qda(df[,1:2], df[,3])
fit

pred <- predict(fit, df[,1:2])

plot(df[,1], df[,2], col=df[,3])
points(fit$means, pch=4,col=c('black','red') , lwd=2, cex=1.5)

x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(x=x, y=y)

z  <- predict(fit, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]  

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)

fit$prior
fit$means
fit$scaling


## Aper 

s = qda(df[,1:2], df[,3], CV=T) # x, grouping


# Compute the estimate of the AER by leave-out-out cross-validation 
table(class.true=df[,3], class.assignedCV=s$class)

errorCV <- (s$class != df[,3])
errorCV

AERCV   <- sum(errorCV)/length(df[,3])
AERCV

# the prediction is good, even though the second gaussian assumption is 
# not true

#knn
set.seed(321)
err = rep(1000, 30)

library(class)

for (k in 1:30) {
  df.knn <- knn.cv(train = df[,1:2], cl = df[,3], k = k)
  
  errorCV <- (df.knn != df[,3])
  err[k]   <- sum(errorCV)/length(df[,3])
}
min(err)
which.min(err)

best <- knn.cv(train = df[,1:2], cl = df[,3], k = 20)
errorCV <- (best != df[,3])
err_fin  <- sum(errorCV)/length(df[,3])

final <- knn(train = df[,1:2],test=xy, cl = df[,3], k = 20)
z  <- as.numeric(final)

plot(df[,1], df[,2], col=df[,3])
contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)



#d

testset <- data.frame(x = 1, y= -4)
knn(train = df[,1:2], test = testset, cl = df[,3], k=20)
predict(fit, testset)

# diverse predizioni, vince knn credo
