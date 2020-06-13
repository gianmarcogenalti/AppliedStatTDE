df1 <- read.table('fifties.txt')
df2 <- read.table('seventies.txt')



pval <- rep(NA,dim(df1)[2])
gauss1 <- rep(NA,dim(df1)[2])
gauss2 <- rep(NA,dim(df1)[2])

for (k in 1:dim(df1)[2]){
  gauss1[k] <- shapiro.test(df1[,k])$p.value
  gauss2[k] <- shapiro.test(df2[,k])$p.value

}
which(gauss1<0.01)
which(gauss2<0.01)

# sono generalmente gaussiani


# D <- df1-df2
# gauss <- rep(NA,dim(D)[2])
# for (k in 1:dim(df1)[2]){
#   gauss[k] <- shapiro.test(D[,k])$p.value
#   pval[k] <- t.test(D[,k],alternative = "less")$p.value
# }
pval_2 <- rep(NA,dim(df1)[2])

for (k in 1:dim(df1)[2]){
  t1.mean <- mean(df1[,k])
  t2.mean <- mean(df2[,k])
  t1.cov <- var(df1[,k])
  t2.cov <- var(df2[,k])
  n1 = 10
  n2 = 10
  p = 1
  Sp <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)
  delta.0 <- 0
  Spinv   <- solve(Sp)
  
  T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)
  
  P <- 0.5*(1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p))
  pval[k]<-P
  
  pval_2[k] <- t.test(df1[,k],df2[,k],alternative = "less")$p.value
}

which(gauss<0.05)
min(pval)
which(pval<2.83e-5)
max(pval)
which(pval>0.992)
p_bonf <- p.adjust(pval,"bonferroni")
which(p_bonf<0.05)

p_bh <- p.adjust(pval,"BH")
which(p_bh<0.05)

which(gauss<0.05)
min(pval_2)
which(pval_2<2.83e-5)
max(pval_2)
which(pval_2>0.992)
p_bonf <- p.adjust(pval_2,"bonferroni")
which(p_bonf<0.05)

p_bh <- p.adjust(pval_2,"BH")
which(p_bh<0.05)
