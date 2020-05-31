rm(list = ls())
data <- read.table("trading.txt")
head(data)

library(MASS)
iris2 <- data[,1:2]
species.name <- as.factor(data$gain)
qda.iris <- qda(iris2, species.name)

plot(iris2, col = species.name)

i1 <- which(species.name==1)
i2 <- which(species.name==0)

n1 <- length(i1)
n2 <- length(i2)
n <- n1+n2


# b)
Qda.iris <- predict(qda.iris, iris2)
table(classe.vera=species.name, classe.allocata=Qda.iris$class)
erroriq <- (Qda.iris$class != species.name)
APERq   <- sum(erroriq)/length(species.name)
APERq

QdaCV.iris <- qda(iris2, species.name, CV=T)
table(classe.vera=species.name, classe.allocataCV=QdaCV.iris$class)
erroriqCV <- (QdaCV.iris$class != species.name)
AERqCV   <- sum(erroriqCV)/length(species.name)
AERqCV

n2/n # trivial classifier error?

x11()
plot(iris2, pch=20)
points(iris2[i1,], col='red', pch=20)
points(iris2[i2,], col='green', pch=20)

points(qda.iris$means, col=c('red','green'), pch=4, lwd=2, cex=1.5)

x  <- seq(min(iris2[,1]), max(iris2[,1]), length=200)
y  <- seq(min(iris2[,2]), max(iris2[,2]), length=200)
xy <- expand.grid(google=x, apple=y)

z  <- predict(qda.iris, xy)$post  
z1 <- z[,1] - z[,2]
z2 <- z[,2] - z[,1]

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)



# c)
newdatum <- data.frame(google = -3, apple = -1.2)
predict(qda.iris, newdatum)
