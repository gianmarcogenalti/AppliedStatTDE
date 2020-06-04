library(car)

mcshapiro.test <- function(X, devstmax = 0.01, sim = ceiling(1/(4*devstmax^2)))
{
  library(mvnormtest)
  n   <- dim(X)[1]
  p   <- dim(X)[2]
  mu  <- rep(0,p)
  sig <- diag(p)
  W   <- NULL
  for(i in 1:sim)
  {
    Xsim <- rmvnorm(n, mu, sig)
    W   <- c(W, mshapiro.test(t(Xsim))$stat)
    # mshapiro.test(X): compute the statistics min(W) for the sample X
  }
  Wmin   <- mshapiro.test(t(X))$stat   # min(W) for the given sample
  pvalue <- sum(W < Wmin)/sim          # proportion of min(W) more extreme than the observed Wmin
  devst  <- sqrt(pvalue*(1-pvalue)/sim)
  list(Wmin = as.vector(Wmin), pvalue = pvalue, devst = devst, sim = sim)
}


df = read.table("stress.txt", header=T)
head(df)
dim(df)
n = dim(df)[1]
p = dim(df)[2]

p_ad = rep(0, 8)
for (k in 1:8) {
  B = 10000
  T_stat = rep(-1000, B)
  n1 = 10
  set.seed(123)
  for(perm in 1:B){
    # permutation:
    permutation <- sample(1:n)
    x_perm <- df[permutation, k]
    x1_perm <- x_perm[1:n1]
    x2_perm <- x_perm[(n1+1):n]
    # test statistic:
    T_stat[perm] <- median(x1_perm) - median(x2_perm)
  }
  x1_perm <- df[1:n1, k]
  x2_perm <- df[(n1+1):n, k]
  # test statistic:
  T_ <- median(x1_perm) - median(x2_perm)
  porco = rep(0, B)
  for (j in 1:B) {
    porco[j] = 1-sum(T_stat>=T_stat[j])/B
  }
  
  p_val <- sum(T_stat>=T_)/B
  porco = c(porco, p_val)
  battona = p.adjust(porco, method="BH")
  print(paste(k, battona[1001], sep=" Pval: "))
  p_ad[k] = p_val
}

p.adjust(p_ad, method="fdr", n=8) #???
