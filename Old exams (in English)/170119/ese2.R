load("mcshapiro.test.RData")
df <- read.table("diamonds.txt")
head(df)
df <- data.frame(scale(df))
plot(df$Diameter, df$Carats, pch = 16)

df.e<- dist(df, method='euclidean') # distance matrix

## Ward linkage

df.es <- hclust(df.e, method='ward.D2') # clustering gerarchico con ward linkage

coph.es <- cophenetic(df.es)

x11()
par(mfrow= c(1,2))
image(as.matrix(df.e), main='Euclidean', asp=1 )
image(as.matrix(coph.es), main='ward', asp=1 )
dev.off()

x11() # dendrogramma
plot(df.es, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(df.es, k=2) 
dev.off()

help(cutree)
cluster.es <- cutree(df.es, k=2) 
cluster.es 

dim1 <- length(cluster.es[which(cluster.es == 2)]) # cluster di dimensione 38
dim2 <- length(cluster.es[which(cluster.es == 1)]) # cluster di dimensione 297 
dim1
dim2

centroids <- apply (df, 2, function (x) tapply (x, cluster.es, mean))
centroids

x11()
plot(df[,1], df[,2], pch = 16, col = cluster.es)
points(centroids, col = 'blue', pch = 16)
dev.off()

df$clu <- as.vector(cluster.es)

mcshapiro.test(df[which(df$clu == 1),1:2])#pval = 0.2552
mcshapiro.test(df[which(df$clu == 2),1:2])#pval = 0.0968

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

alpha   <- .05
delta.0 <- c(0,0)
Spinv   <- solve(Sp)

T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)

cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
T2 < cfr.fisher 

P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P  
# P-value is null -> I reject the null hypothesis and accept the alternative one: there are two type of diamonds

# Simultaneous T2 intervals
IC.T2.X1 <- c(t1.mean[1]-t2.mean[1]-sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)), t1.mean[1]-t2.mean[1]+sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)) )
IC.T2.X2 <- c(t1.mean[2]-t2.mean[2]-sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)), t1.mean[2]-t2.mean[2]+sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)) )
IC.T2 <- rbind(IC.T2.X1, IC.T2.X2)
dimnames(IC.T2)[[2]] <- c('inf','sup')                        

#             inf       sup
# IC.T2.X1 -2.259121 -1.476079
# IC.T2.X2 -3.016882 -2.567309

