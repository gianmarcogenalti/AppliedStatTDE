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


df = read.table("Running.txt", header=T)
head(df)
dim(df)

# a)
df.e <- dist(df, method='euclidean')

help(hclust)

df.es <- hclust(df.e, method='single')
df.ea <- hclust(df.e, method='average')
df.ec <- hclust(df.e, method='complete')


# Fix k=2 clusters:
clu <- cutree(df.ec, k=2) # euclidean-complete:
clu

plot(df, col=clu)

# compute the cophenetic matrices 
coph.ec <- cophenetic(df.ec)

es <- cor(df.e, coph.ec)



# c)

mcshapiro.test(df)

# Let's try with Bonferroni intervals ##########4
alpha = 0.05
p = 2
n = dim(df)[1]
k <- p # number of intervals I want to compute (set in advance)
cfr.t <- qt(1-alpha/(2*k),n-1)
df.mean = sapply(df, mean)
df.cov = cov(df)
Bf <- cbind(inf = df.mean - cfr.t*sqrt(diag(df.cov)/n),
            center = df.mean, 
            sup = df.mean + cfr.t*sqrt(diag(df.cov)/n))
Bf

df.cov[1,1]
df.cov[2,2]
