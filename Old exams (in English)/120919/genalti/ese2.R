rm(list = ls())
df <- read.table("stress.txt")
head(df)
tail(df)
set.seed(123)
n1 <- 10
n2 <- 5
n <- n1+n2
B <- 10000
p_val <- rep(NA,8)
stat <- rep(NA,8)
Tzero <- rep(NA,8)
for(i in 1:8){
  x1 <- df[1:10,i]
  x2 <- df[11:15,i]
  x_pooled <- df[,i]
  T0 <- median(x1) - median(x2)
  T_stat <- rep(NA,B)
  for(perm in 1:B){
    # permutation:
    permutation <- sample(1:n)
    x_perm <- x_pooled[permutation]
    x1_perm <- x_perm[1:n1]
    x2_perm <- x_perm[(n1+1):n]
    # test statistic:
    T_stat[perm] <- median(x1_perm) - median(x2_perm)
  }
  # Permutational distribution of T
  #hist(T_stat,xlim=range(c(T_stat,T0)),breaks=10)
  #abline(v=T0,col=3,lwd=2)
  
  #plot(ecdf(T_stat))
  #abline(v=T0,col=3,lwd=2)
  
  # p-value
  p_val[i] <- sum(T_stat < T0)/B
  stat[i] <- mean(T_stat)
  Tzero[i] <- T0
}
p_val
p_adj <- p.adjust(p_val, 'fdr', n = 8)
p_adj


