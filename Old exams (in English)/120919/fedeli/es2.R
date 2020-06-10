df <- read.table("stress.txt")


# H0 : x1 <= x2 
# H1 : x1 > x2


perm_test <- function(g,n1,it=10000,seed=123){
  i = 1
  stat = rep(NA,it)
  for (i in 1:it){
    perm <- sample(1:length(g))
    g <- g[perm]
    g1 <- g[1:n1]
    g2 <- g[(n1+1):length(g)]
    stat[i] <- median(g1) - median(g2)
  }
  return(stat)
}

it <- 10000
p_val <- rep(NA,length(df[1,]))
true_stat <- rep(NA,length(df[1,]))

for (k in 1:length(df[1,])){
  true_stat[k] <- median(df[1:10,k])-median(df[11:15,k])
  stat <- perm_test(df[,k],10,it)
  par(mfrow=c(1,2))
  hist(stat,breaks=10)
  abline(v=true_stat[k],col=3,lwd=2)
  plot(ecdf(stat))
  abline(v=true_stat[k],col=3,lwd=2)
  
  p_val[k] <- sum(stat>=true_stat[k])/it

}

true_stat
p_val

p.adjust(p_val, method="bh", n=length(df[1,]))
