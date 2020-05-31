rm(list = ls())
data <- read.table("Sailing.txt")

library(MASS)
attach(data)
species.name <- factor(type, labels=c('seadog','vacationer'))

g=2

i1 <- which(species.name=='seadog')
i2 <- which(species.name=='vacationer')

n1 <- length(i1)
n2 <- length(i2)
n <- n1+n2

detach(data)

iris2 <- data[,1:2]


m <-  colMeans(iris2)
m1 <- colMeans(iris2[i1,])
m2 <- colMeans(iris2[i2,])

S1 <- cov(iris2[i1,])
S2 <- cov(iris2[i2,])
# Sp  <- ((n1-1)*S1+(n2-1)*S2+(n3-1)*S3)/(n-g)

# One-way MANOVA (See LAB 8)
fit <- manova(as.matrix(iris2) ~ species.name)
summary.manova(fit,test="Wilks")

prior = c(0.2,0.8)
# Linear Discriminant Analysis (LDA)
lda.iris <- lda(iris2, species.name, prior = prior)
lda.iris

Lda.iris <- predict(lda.iris, iris2)

# Compute the APER (apparent error rate)
misc <- table(classe.vera=species.name, classe.allocata=Lda.iris$class)
APER <- 0
G <- 2
for(g in 1:G)
  APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior[g]


# Compute the estimate of the AER by cross-validation 
LdaCV.iris <- lda(iris2, species.name, prior = prior, CV=TRUE)  # specify the argument CV
miscCV <- table(classe.vera=species.name, classe.allocata=LdaCV.iris$class)
APERCV <- 0
G <- 2
for(g in 1:G)
  APERCV <- APERCV + sum(miscCV[g,-g])/sum(miscCV[g,]) * prior[g]



# Plot the partition induced by LDA

x11()
plot(iris2, pch=20)
points(iris2[i1,], col='red', pch=20)
points(iris2[i2,], col='green', pch=20)
points(lda.iris$means, pch=4,col=c('red','green','blue') , lwd=2, cex=1.5) # centri

x  <- seq(min(data[,1]), max(data[,1]), length=200)
y  <- seq(min(data[,2]), max(data[,2]), length=200)
xy <- expand.grid(water=x, sailing.time=y)

z  <- predict(lda.iris, xy)$post  # these are P_i*f_i(x,y)  
z1 <- z[,1] - pmax(z[,2])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  

# Plot the contour line of level (levels=0) of z1, z2, z3: 
# P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
# where j realizes the max.
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  


### Quadratic Discriminand Analysis (QDA)
###---------------------------------------

qda.iris <- qda(iris2, species.name, prior = prior)
qda.iris
Qda.iris <- predict(qda.iris, iris2)
#Qda.iris

# compute the APER
miscQDA <- table(classe.vera=species.name, classe.allocata=Qda.iris$class)
APERQDA <- 0
G <- 2
for(g in 1:G)
  APERQDA <- APERQDA + sum(miscQDA[g,-g])/sum(miscQDA[g,]) * prior[g]


# Compute the estimate of the AER by cross-validation 
QdaCV.iris <- qda(iris2, species.name, CV=TRUE, prior = prior)  # specify the argument CV
miscQDACV <- table(classe.vera=species.name, classe.allocata=QdaCV.iris$class)
APERQDACV <- 0
G <- 2
for(g in 1:G)
  APERQDACV <- APERQDACV + sum(miscQDACV[g,-g])/sum(miscQDACV[g,]) * prior[g]


# Plot the partition induced by QDA
x11()
plot(iris2, pch=20)
points(iris2[i1,], col='red', pch=20)
points(iris2[i2,], col='green', pch=20)

points(qda.iris$means, col=c('red','green','blue'), pch=4, lwd=2, cex=1.5)

x  <- seq(min(data[,1]), max(data[,1]), length=200)
y  <- seq(min(data[,2]), max(data[,2]), length=200)
xy <- expand.grid(water=x, sailing.time=y)

z  <- predict(qda.iris, xy)$post  
z1 <- z[,1] - z[,2]

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)


Lda.iris$posterior[1,]
Qda.iris$posterior[1,]


load("mcshapiro.test.RData")
mcshapiro.test(iris2[i1,])
mcshapiro.test(iris2[i2,])

cov(iris2[i1,])
cov(iris2[i2,])

newdata <- data.frame(water = 35, sailing.time = 168)
predict(qda.iris, newdata)$post
