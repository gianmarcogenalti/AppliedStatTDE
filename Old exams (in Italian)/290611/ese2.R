rm(list = ls())
df <- read.table('sand.txt')
head(df)
open3d()
plot3d(df[,1],df[,2],df[,3],pch = 16)

df.e<- dist(df, method='manhattan') # distance matrix
## single linkage

df.es <- hclust(df.e, method='ward.D') 

coph = cophenetic(df.es)
cophenetic_coef <- cor(coph,df.e)

x11() # dendrogramma
plot(df.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(df.es, k=2)

help(cutree)
cluster.es <- cutree(df.es, k=2) 
cluster.es 

dim1 <- length(cluster.es[which(cluster.es == 1)]) # cluster di dimensione 1
dim2 <- length(cluster.es[which(cluster.es == 2)]) # cluster di dimensione 159 
dim1
dim2


centroids <- apply (df, 2, function (x) tapply (x, cluster.es, mean))
centroids

open3d()
plot3d(df[,1],df[,2],df[,3],pch = 16, col = cluster.es)


n1 <- dim(t1)[1] # n1=3
n2 <- dim(t2)[1] # n2=4
p  <- dim(t1)[2] # p=2


# we compute the sample mean, covariance matrices and the matrix
# Spooled

t1 <- df[cluster.es == 1,]
n1 <- dim1
t2 <- df[cluster.es == 2,]
n2 <-dim2

t1.mean <- sapply(t1,mean)
t2.mean <- sapply(t2,mean)
t1.cov  <-  cov(t1)
t2.cov  <-  cov(t2)
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)
# we compare the matrices
list(S1=t1.cov, S2=t2.cov, Spooled=Sp)

# Test H0: mu1 == mu2  vs  H1: mu1 != mu2
# i.e.,
# Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)
p <- 3
alpha   <- .1
delta.0 <- c(0,0,0)
Spinv   <- solve(Sp)

T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)

cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
T2 < cfr.fisher 

P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P  # there's a difference






