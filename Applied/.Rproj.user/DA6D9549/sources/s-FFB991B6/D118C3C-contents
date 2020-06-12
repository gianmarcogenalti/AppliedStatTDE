df <- read.table('wellness.txt')

set.seed(321)
it <- 5000

# testare delta > 0 

perm_test <- function(f,n,it=5000){
  i = 1
  stat = rep(NA,it)
  for (i in 1:it){
    n1 <- sample(1:n-1,1)
    g <- f
    perm <- sample(1:length(g))
    g <- g[perm]
    g[1:n1] <- -g[1:n1]
    g[n1+1:length(g)] <- g[(n1+1):length(g)]
    stat[i] <- mean(g)
  }
  return(stat)
}

p_val <- rep(NA,length(df[1,]))
true_stat <- rep(NA,length(df[1,]))

for (k in 1:(length(df[1,]))){
  true_stat[k] <- mean(df[,k])
  stat <- perm_test(df[,k],length(df[,k]),it)
  
  par(mfrow=c(1,2))
  hist(stat,breaks=10)
  abline(v=true_stat[k],col=3,lwd=2)
  plot(ecdf(stat))
  abline(v=true_stat[k],col=3,lwd=2)
  
  p_val[k] <- sum(stat>=true_stat[k])/it
}


true_stat
p_val

p.adjust(p_val, method="BH", n=length(df[1,]))

p_t <- rep(NA,length(df[1,]))

for (k in 1:length(df[1,])){
  p_t[k] <- t.test(df[,k],alternative= 'greater',mu=0,alpha = 0.99)$p.value
}
