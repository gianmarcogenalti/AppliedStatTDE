library(rgl)
rm(list = ls())
load('mcshapiro.test.RData')
df <- read.table('astra.txt')
head(df)
open3d()
plot3d(df[,1],df[,2],df[,3])

df.e<- dist(df, method='manhattan') 

## single linkage

df.es <- hclust(df.e, method='ward.D') 

x11() # dendrogramma
plot(df.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(df.es, k=2) # ok fa schifo sto clustering

help(cutree)
cluster.es <- cutree(df.es, k=2) 
cluster.es 

dim1 <- length(cluster.es[which(cluster.es == 1)]) # cluster di dimensione 1
dim2 <- length(cluster.es[which(cluster.es == 2)]) # cluster di dimensione 159 
dim1
dim2
# cluster osceno, solo il 70 viene classificato in un cluster diverso dagli altri
# per sport vediamo i centroidi dei cluster

centroids <- apply (df, 2, function (x) tapply (x, cluster.es, mean))
centroids

plot3d(df[,1],df[,2],df[,3], col = cluster.es)


### question b)
p  <- 3
n1 <- dim(df[cluster.es == 1,])[1]
n2 <- dim(df[cluster.es == 2,])[1]

mcshapiro.test(df[cluster.es == 1,])
mcshapiro.test(df[cluster.es == 2,])

# Test for independent Gaussian populations
t1.mean <- sapply(df[cluster.es == 1,],mean)
t2.mean <- sapply(df[cluster.es == 2,],mean)
t1.cov  <-  cov(df[cluster.es == 1,])
t2.cov  <-  cov(df[cluster.es == 2,])
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)

# Test: H0: mu.1-mu.2==0 vs H1: mu.1-mu.2!=0
delta.0 <- c(0,0,0)
Spinv   <- solve(Sp)
T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)
P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P

# there's statistical difference in mean concentrations

alpha <- 0.1
IC <- cbind(t2.mean-t1.mean - sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(p*2), n1+n2-2),
            t2.mean-t1.mean,
            t2.mean-t1.mean + sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(p*2), n1+n2-2))

# C -5.3698063 -5.06641337 -4.7630204
# H -0.3899556 -0.09701114  0.1959333
# O  4.8697289  5.16286727  5.4560056





