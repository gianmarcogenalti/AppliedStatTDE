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


df = read.table("sequoia.txt", header=T)
head(df)
dim(df)

# a
df.e = dist(df, method="euclidean")
df.w <- hclust(df.e, method='ward.D2')


# Fix k=2 clusters:
cl <- cutree(df.w, k=5) # euclidean-complete:

plot(df, col=cl)

table(cl)

attach(df)
maov = manova(as.matrix(df) ~ cl)

# variances are different
bartlett.test(df, cl)



n_intervals <- 10
alpha <- .1

ICmeanvar <- function(x, alpha, n_int, type = 'mean'){
  n <- length(x)
  M <- mean(x)
  S <- var(x)
  IC.mean <- c(M - qt(1-alpha/(2*n_int), n-1)* sqrt(S/n), M + qt(1-alpha/(2*n_int), n-1)* sqrt(S/n))
  names(IC.mean) <- c('Inf', 'Sup')
  
  IC.var <- c(S* (n-1) /qchisq(alpha/(2*n_int), n-1), S* (n-1) /qchisq(1 - alpha/(2*n_int), n-1))
  names(IC.var) <- c('Inf', 'Sup')
  
  if(type == 'mean'){
    return(IC.mean)
  }
  if(type == 'var'){
    return(IC.var)
  }
}


for(i in 1:5){
  print(paste(i, "Mean", "Var", sep=" "))
  print(ICmeanvar(df[cl == i,2], alpha, n_intervals, type = 'mean'))
  print(ICmeanvar(df[cl == i,2], alpha, n_intervals, type = 'var'))
}



