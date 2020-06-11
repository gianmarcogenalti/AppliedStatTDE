df <- read.table("england.txt")

dist.eucl <- dist(df, method = "manhattan")
hclust.single <- hclust(dist.eucl, method = "single")

plot(hclust.single, labels = F, sub = "")
tree.single <- cutree(hclust.single, k = 3)

plot(df, col = tree.single)

i1 <- which(tree.single == 1)
i2 <- which(tree.single == 2)
i3 <- which(tree.single == 3)

n1 <- length(i1)
length(i2)
n2 <- length(i3)

t1.mean <- sapply(df[i1,],mean)
t2.mean <- sapply(df[i3,],mean)
t1.cov  <-  cov(df[i1,])
t2.cov  <-  cov(df[i3,])
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)
Spinv <- solve(Sp)

alpha   <- .01
delta.0 <- c(0,0)
Spinv   <- solve(Sp)
p <- 3

T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)

cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
T2 < cfr.fisher # TRUE: no statistical evidence to reject H0 at level 1%

P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P  

# son t^2, se faccio bonferroni più stretti

IC.T2.X1 <- c(t1.mean[1]-t2.mean[1]-sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)), t1.mean[1]-t2.mean[1]+sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)) )
IC.T2.X2 <- c(t1.mean[2]-t2.mean[2]-sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)), t1.mean[2]-t2.mean[2]+sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)) )
IC.T2.X3 <- c(t1.mean[3]-t2.mean[3]-sqrt(cfr.fisher*Sp[3,3]*(1/n1+1/n2)), t1.mean[3]-t2.mean[3]+sqrt(cfr.fisher*Sp[3,3]*(1/n1+1/n2)) )

IC.T2 <- rbind(IC.T2.X1, IC.T2.X2, IC.T2.X3)
dimnames(IC.T2)[[2]] <- c('inf','sup')                        
IC.T2

df[i3,]

# C'è differenza solo nei borseggi. Le contee del cluster più piccolo 
# subiscono molti più borseggi. Sono in effetti le contee
# dove si trovano le città pù grosse del Regno Unito,
# dunque è prevedibile ci sia un numero di borseggi più alto.

df[i3,]
