rm(list = ls())
load('mcshapiro.test.RData')
df <- read.table('uranium.txt')
head(df)
# contrast matrix that looks at consecutive increments
C <- rbind( c(-1,1,0,0,0,0,0,0,0,0),
            c(0,-1,1,0,0,0,0,0,0,0),
            c(0,0,-1,1,0,0,0,0,0,0),
            c(0,0,0,-1,1,0,0,0,0,0),
            c(0,0,0,0,-1,1,0,0,0,0),
            c(0,0,0,0,0,-1,1,0,0,0),
            c(0,0,0,0,0,0,-1,1,0,0),
            c(0,0,0,0,0,0,0,-1,1,0),
            c(0,0,0,0,0,0,0,0,-1,1))

tempi <- df
n <- dim(tempi)[1]
q <- dim(tempi)[2]

M <- sapply(tempi,mean)
S <- cov(tempi)


mcshapiro.test(tempi)

# Test: H0: C%*%mu=0 vs H1: C%*%mu!=0
delta.0 <- c(0, 0, 0,0,0,0,0,0,0)

Md <- C %*% M 
Sd <- C %*% S %*% t(C)
Sdinv <- solve(Sd)

T2 <- n * t( Md - delta.0 ) %*% Sdinv %*% ( Md - delta.0 )

P <- 1-pf(T2*(n-(q-1))/((q-1)*(n-1)),(q-1),n-(q-1))
P  # Reject H0

### question b)
# b) Using appropriate confidence intervals, describe the temporal 
#    dynamics the mean times.
alpha <- .1
k <- (q-1)  # we provide a conf int for the mean at time t=0


ICmedie <- rbind(cbind(Md - sqrt(diag(Sd)/n) * qt(1 - alpha/(2*k), n-1),
                       Md,
                       Md + sqrt(diag(Sd)/n) * qt(1 - alpha/(2*k), n-1)))

ICmedie

t.test(tempi[,8]-tempi[,9], mu = 1, conf.level = 0.9)# confermiamo
# One Sample t-test
# 
# data:  tempi[, 8] - tempi[, 9]
# t = 1.3699, df = 31, p-value = 0.1806
# alternative hypothesis: true mean is not equal to 1
# 90 percent confidence interval:
#   0.9718873 1.2646127
# sample estimates:
#   mean of x 
# 1.11825 




