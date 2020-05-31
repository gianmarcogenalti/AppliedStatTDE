rm(list = ls ())
data <- read.table(file = "horsecolic.txt")
load("mcshapiro.test.RData")
head(data)

summary(data)

i1 <- which(data$Pain == "Yes")
i2 <- which(data$Pain == "No")

n1 <- length(i1)
n2 <- length(i2)
n <- dim(data)[1]
p <- dim(data)[2]-1

pain <- data[i1,1:p]
nopain <- data[i2,1:p]

mcshapiro.test(pain)
mcshapiro.test(nopain)

S1 <- cov(pain)
S2 <- cov(nopain)
S1
S2

x11(width=21)
par(mfrow=c(1,2))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
dev.off()

Spooled <- ((n1-1)*S1 + (n2-1)*S2)/(n1+n2-2)


alpha <- 0.01
k <- 4

x.mean1 <- sapply(pain, mean)
x.mean2 <- sapply(nopain, mean)
x.mean <- x.mean1 - x.mean2

cfr.t <- qt(1-alpha/(2*k), n1+n2-2)

Bf <- cbind(inf = x.mean - cfr.t*sqrt(diag(Spooled)*(1/n1+1/n2)),
            center = x.mean,
            sup = x.mean + cfr.t*sqrt(diag(Spooled)*(1/n1+1/n2)))
Bf


# b
plot(data[,2:3], col = data$Pain)

library(MASS)
classificator1 <- lda(data[,2:3], data$Pain)
classificator2 <- qda(data[,2:3], data$Pain)
iris2 <- data[,2:3]

x11()
plot(iris2, pch=20)
points(iris2[i1,], col='red', pch=20)
points(iris2[i2,], col='green', pch=20)

x  <- seq(min(iris2[,1]), max(iris2[,1]), length=200)
y  <- seq(min(iris2[,2]), max(iris2[,2]), length=200)
xy <- expand.grid(Pulse=x, Respiratory.rate=y)

z  <- predict(classificator2, xy)$post
z1 <- z[,1] - z[,2]
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  

# c)
reclass <- predict(classificator1, data[,2:3])
table(class.true=data$Pain, class.assigned=reclass$class)

errori <- (reclass$class != data$Pain)
length(errori)

APER   <- sum(errori)/n
APER   # 0.06 lda / 0.0533 qda 

# meglio qda che le varianze insomma...

