load("mcshapiro.test.RData")
df <- read.table("knossos.txt")
head(df)
attach(df)
plot(X,Y, pch = 16)
points(0,0, col = 'red', pch = 16) # knossos palace
detach(df)

df.e<- dist(df, method='euclidean') # distance matrix

## single linkage

df.es <- hclust(df.e, method='complete') # clustering gerarchico con complete linkage

coph.es <- cophenetic(df.es)
coph.es

x11()
par(mfrow= c(1,2))
image(as.matrix(df.e), main='Euclidean', asp=1 )
image(as.matrix(coph.es), main='Complete', asp=1 )

x11() # dendrogramma
plot(df.es, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(df.es, k=2) # ok fa schifo sto clustering

help(cutree)
cluster.es <- cutree(df.es, k=2) 
cluster.es 

dim1 <- length(cluster.es[which(cluster.es == 2)]) # cluster di dimensione 78
dim2 <- length(cluster.es[which(cluster.es == 1)]) # cluster di dimensione 123
dim1
dim2

centroids <- apply (df, 2, function (x) tapply (x, cluster.es, mean))
centroids
#       X           Y
#1 2.96292683  1.50398374
#2 0.03269231 -0.02320513

x11()
plot(df$X, df$Y, col = cluster.es, pch = 16)
points(centroids, col = 'blue', pch = 16)

df$clu <- cluster.es

mcshapiro.test(df[which(df$clu == 1),1:2])#pval = 0.6228
mcshapiro.test(df[which(df$clu == 2),1:2])#pval = 0.6052
var.test(X+Y~clu, data = df) #pval = 0.2206

# sono normali bivariate -> test per la differenza di medie di popolazioni normali con stessa varianza
n1 <- dim(df[which(df$clu == 1),1:2])[1] 
n2 <- dim(df[which(df$clu == 2),1:2])[1] 
p  <- dim(df)[2]


# we compute the sample mean, covariance matrices and the matrix
# Spooled

t1.mean <- sapply(df[which(df$clu == 1),1:2],mean)
t2.mean <- sapply(df[which(df$clu == 2),1:2],mean)
t1.cov  <-  cov(df[which(df$clu == 1),1:2])
t2.cov  <-  cov(df[which(df$clu == 2),1:2])
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)
# we compare the matrices
list(S1=t1.cov, S2=t2.cov, Spooled=Sp)

# Test H0: mu1 == mu2  vs  H1: mu1 != mu2
# i.e.,
# Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)

alpha   <- .01
delta.0 <- c(0,0)
Spinv   <- solve(Sp)

T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)

cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
T2 < cfr.fisher 

P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P  
# P-value is null -> I reject the null hypothesis and accept the alternative one: there are two sites

# Simultaneous T2 intervals
IC.T2.X1 <- c(t1.mean[1]-t2.mean[1]-sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)), t1.mean[1]-t2.mean[1]+sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)) )
IC.T2.X2 <- c(t1.mean[2]-t2.mean[2]-sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)), t1.mean[2]-t2.mean[2]+sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)) )
IC.T2 <- rbind(IC.T2.X1, IC.T2.X2)
dimnames(IC.T2)[[2]] <- c('inf','sup')                        
IC.T2

#            inf      sup
#IC.T2.X1 2.676154 3.184315
#IC.T2.X2 1.237878 1.816500

# after having concluded that there exist two different sites, I compute the 95% confidence intervals for the
# differences of the two coordinates between the two sites: we can see that those differences are different from zero:
# in particular being the first cluster centered in the knossos palace (0,0) I can imagine another site at a distance
# of 2.6-3 on X-axis and of 1.2-1.8 on Y-axis.







