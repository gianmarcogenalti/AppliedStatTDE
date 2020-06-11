df <- read.table("IAMG.txt")
mcshapiro.test(df)
#it is gaussian

n <- dim(df)[1]
p <- dim(df) [2]

m <- sapply(df, mean) #center
S <- cov(df)

eigen(S)$vectors
# direction of the axes


alpha <- 0.05
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)

# Length of the semi-axes of the ellipse:
r <- sqrt(cfr.fisher)
r*sqrt(eigen(S/n)$values) 

# b

c1 <- m[1]
l1 <- m[1]-sqrt(cfr.fisher*S[1,1]/n)
r1 <- m[1]+sqrt(cfr.fisher*S[1,1]/n)
c2 <- m[2]
l2 <- m[2]-sqrt(cfr.fisher*S[2,2]/n)
r2 <- m[2]+sqrt(cfr.fisher*S[2,2]/n)
c3 <- m[3]
l3 <- m[3]-sqrt(cfr.fisher*S[3,3]/n)
r3 <- m[3]+sqrt(cfr.fisher*S[3,3]/n)

T2 <- cbind(inf = c(l1,l2,l3), center = c(c1,c2,c3), sup = c(r1,r2,r3))
T2

#c

t.test(0.1*df$Registered - df$No.show, alternative = "two.sided", mu = 0)
