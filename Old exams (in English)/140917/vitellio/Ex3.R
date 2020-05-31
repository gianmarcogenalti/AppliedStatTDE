rm(list=ls())
data <- read.table("knossos.txt")

iris4 <- data
plot(iris4)

iris.e <- dist(iris4, method='euclidean')
iris.ea <- hclust(iris.e, method='complete')
plot(iris.ea, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(iris.ea, k=2)
cluster.ea <- cutree(iris.ea, k=2)

i1 <- which(cluster.ea == 1)
i2 <- which(cluster.ea == 2)

n1 = length(i1)
n2 = length(i2)

m1 = sapply(iris4[i1,],mean)
m2 = sapply(iris4[i2,],mean)

coph.ea <- cophenetic(iris.ea)
ea <- cor(iris.e, coph.ea)
ea





#b)
load("mcshapiro.test.RData")
Ps <- NULL
Ps <- c(Ps, mcshapiro.test(iris4[i1,])$p)
Ps <- c(Ps, mcshapiro.test(iris4[i2,])$p)
Ps

t1.mean <- sapply(iris4[i1,],mean)
t2.mean <- sapply(iris4[i2,],mean)
t1.cov  <- cov(iris4[i1,])
t2.cov  <- cov(iris4[i2,])
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)

alpha   <- .05
delta.0 <- c(0,0)
Spinv   <- solve(Sp)

T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)
p <- 2
cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
T2 < cfr.fisher

P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P  