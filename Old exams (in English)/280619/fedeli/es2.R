df <- read.table('profiling.txt')

mcshapiro.test(df[which(df$type=='tourist'),1:2])
mcshapiro.test(df[which(df$type=='resident'),1:2])
var(df[which(df$type=='tourist'),1:2])
var(df[which(df$type=='resident'),1:2])
# normali, varianze stradifferenti

library(MASS)

qda.m <- qda(df[,1:2],df[,3])
qda.m

plot(df[,1:2],col=df[,3], main='Plot', xlab='x1', ylab='x2', pch=20)

points(qda.m$means, pch=4,col=c('red','blue') , lwd=2, cex=1.5)

x  <- seq(min(df[,1]), max(df[,1]), length=200)
y  <- seq(min(df[,2]), max(df[,2]), length=200)
xy <- expand.grid(t1=x, t2=y)

z  <- predict(qda.m, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]  

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)

# APER

mcv <- qda(df[,c(1,2)], df[,3], CV=T)
errorsqCV <- (mcv$class != df[,3])

AERqCV   <- sum(errorsqCV)/length(df[,3])
AERqCV

pred <- data.frame(t1 = 35, t2 = 3)
z  <- predict(qda.m, pred)
