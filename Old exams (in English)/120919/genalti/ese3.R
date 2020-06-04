library(MASS)

df <- read.table("debris.txt")
head(df)

x11()
plot(df$x,df$y,col = df$risk)

qda.deb <- qda(df[,1:2], df[,3])
summary(qda.deb)

pred <- predict(qda.deb, df[,1:2])

table(class.true=df[,3], class.assigned=pred$class)

errorsq <- (pred$class != df[,3])

APERq   <- sum(errorsq)/length(df[,3])
APERq

# Compute the estimate of the AER by leave-out-out cross-validation 
QdaCV <- qda(df[,1:2], df[,3], CV=T)
table(class.true=df[,3], class.assignedCV=QdaCV$class)

errorsqCV <- (QdaCV$class != df[,3])


AERqCV   <- sum(errorsqCV)/length(df[,3])
AERqCV

# Plot the partition induced by QDA
x11()
plot(df[,1:2], main='Iris Sepal', xlab='Sepal.Length', ylab='Sepal.Width', pch=20)
points(iris2[i1,], col='red', pch=20)
points(iris2[i2,], col='green', pch=20)
points(iris2[i3,], col='blue', pch=20)
legend("topright", legend=levels(species.name), fill=c('red','green','blue'))

points(qda.iris$means, col=c('red','green','blue'), pch=4, lwd=2, cex=1.5)

x  <- seq(min(iris[,1]), max(iris[,1]), length=200)
y  <- seq(min(iris[,2]), max(iris[,2]), length=200)
xy <- expand.grid(Sepal.Length=x, Sepal.Width=y)

z  <- predict(qda.iris, xy)$post  
z1 <- z[,1] - pmax(z[,2], z[,3])    
z2 <- z[,2] - pmax(z[,1], z[,3])    
z3 <- z[,3] - pmax(z[,1], z[,2])

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)

open3d()
points3d(iris2[i1,1], iris2[i1,2], 0, col='red', pch=15)
points3d(iris2[i2,1], iris2[i3,2], 0, col='green', pch=15)
points3d(iris2[i3,1], iris2[i2,2], 0, col='blue', pch=15)
surface3d(x,y,matrix(dmvnorm(xy, m1, S1) / 3, 50), alpha=0.4, color='red')
surface3d(x,y,matrix(dmvnorm(xy, m2, S2) / 3, 50), alpha=0.4, color='green', add=T)
surface3d(x,y,matrix(dmvnorm(xy, m3, S3) / 3, 50), alpha=0.4, color='blue', add=T)
box3d()
