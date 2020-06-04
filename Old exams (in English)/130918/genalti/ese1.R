load("mcshapiro.test.Rdata")
df <- read.table("IAMG.txt")
head(df)

mcshapiro.test(df)
# siamo di fronte ad una normale multivariata

n <- dim(df)[1]
p <- dim(df)[2] 

D.mean   <- sapply(df,mean)
D.cov    <- cov(df)
D.invcov <- solve(D.cov)

alpha   <- .05
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)

D.mean # center
# Registered       Talk    No.show 
# 249.44     119.04      23.12 

eigen(D.cov)$vectors # directions
#            [,1]       [,2]        [,3]
# [1,] 0.98711610  0.1554660 -0.03784352
# [2,] 0.09129046 -0.7414562 -0.66476221
# [3,] 0.13140723 -0.6527427  0.74609588

sqrt(cfr.fisher)*sqrt(eigen(D.cov/n)$values) # lengths of semiaxes
# 9.914474 4.218786 2.708017

for(i in 1:3){
  IC = c(D.mean[i] - sqrt(cfr.fisher*D.cov[i,i]/n),D.mean[i],D.mean[i] + sqrt(cfr.fisher*D.cov[i,i]/n))
  names(IC) <-c('inf', 'center','sup')
  print(IC)
}

diff <- 0.1*df$Registered - df$No.show
t.test(diff)



