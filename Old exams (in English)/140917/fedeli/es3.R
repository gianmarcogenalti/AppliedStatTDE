df <- read.table('knossos.txt')

eu <- dist(df, method='euclidean')
complete <- hclust(eu, method='complete')

plot(complete, labels = F, sub = "")
tree <- cutree(complete, k = 2)

plot(df, col = tree)

i1 <- which(tree == 1)
i2 <- which(tree == 2)
n1 <- length(i1)
n2 <- length(i2)
colMeans(df[i1,])
colMeans(df[i2,])


coph <- cophenetic(complete)
ew <- cor(eu, coph)
ew

# b


mcshapiro.test(df[i1,])
mcshapiro.test(df[i2,])
# really normal much wow

t1.mean <- sapply(df[i1,],mean)
t2.mean <- sapply(df[i2,],mean)
t1.cov  <- cov(df[i1,])
t2.cov  <- cov(df[i2,])
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
