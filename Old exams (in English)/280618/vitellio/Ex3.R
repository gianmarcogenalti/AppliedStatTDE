rm(list = ls())
data <- read.table("Precolombian.txt")
head(data)
load("mcshapiro.test.RData")

i1 <- which(data$Civilization == "Aztec")
i2 <- which(data$Civilization == "Maya")
i3 <- which(data$Civilization == "Toltec")
mcshapiro.test(data[i1,1:2])
mcshapiro.test(data[i2,1:2])
mcshapiro.test(data[i3,1:2])

plot(data[,1:2], col = data$Civilization)

cov(data[i1,1:2])
cov(data[i2,1:2])
cov(data[i3,1:2])

prior <- c(0.2,0.1,0.7)

n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n <- n1+n2+n3
g <- 3

precolom <- data[,1:2]

m <-  colMeans(precolom)
m1 <- colMeans(precolom[i1,])
m2 <- colMeans(precolom[i2,])
m3 <- colMeans(precolom[i3,])

S1 <- cov(precolom[i1,])
S2 <- cov(precolom[i2,])
S3 <- cov(precolom[i3,])
Sp  <- ((n1-1)*S1+(n2-1)*S2+(n3-1)*S3)/(n-g)

library(MASS)
qda.prec <- qda(precolom, data$Civilization, prior = prior)
Qda.prec <- predict(qda.prec, precolom)

table(classe.vera=data$Civilization, classe.allocata=Qda.prec$class)

erroriq <- (Qda.prec$class != data$Civilization)

misc <- table(classe.vera=data$Civilization, classe.allocata=Qda.prec$class)
APER <- 0
G <- 2
for(g in 1:G)
  APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior[g]


x11()
plot(precolom, pch=20)
points(precolom[i1,], col='red', pch=20)
points(precolom[i2,], col='green', pch=20)
points(precolom[i3,], col='blue', pch=20)
legend(min(precolom[,1]), max(precolom[,2]), legend=levels(data$Civilization), fill=c('red','green','blue'))

points(qda.prec$means, col=c('red','green','blue'), pch=4, lwd=2, cex=1.5)

x  <- seq(min(precolom[,1]), max(precolom[,1]), length=200)
y  <- seq(min(precolom[,2]), max(precolom[,2]), length=200)
xy <- expand.grid(Year=x, Aspect.Ratio=y)

z  <- predict(qda.prec, xy)$post  
z1 <- z[,1] - pmax(z[,2], z[,3])    
z2 <- z[,2] - pmax(z[,1], z[,3])    
z3 <- z[,3] - pmax(z[,1], z[,2])

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)



newdatum <- data.frame(Year = 986, Aspect.Ratio = 1.4)
predict(qda.prec, newdatum)

