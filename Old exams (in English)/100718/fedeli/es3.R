
df_pre <- read.table('Presales.txt')
df_post <- read.table('Sales.txt')

diff <- df_post - df_pre

#mcshapiro.test(df_pre)
#mcshapiro.test(df_post)

alpha <- 0.01

df <- merge(df_pre,df_post, by = 0 )
df <- df[-1]

mcshapiro.test(df)

Con <- rbind(c(0.8,0,-1,0),c(0,0.8,0,-1))
delta <- c(0,0) # quindi non serve

n <- length(df[,1])
q <- length(df)
Ss <- cov(df)
means <- colMeans(df)

test.stat <- n*t(Con%*%means)%*%solve(Con%*%Ss%*%t(Con))%*%(Con%*%means)

cfr.fisher <- ((q-1)*(n-1)/(n-(q-1)))*qf(1-alpha,(q-1),n-(q-1)) 

test.stat < cfr.fisher
test.stat
cfr.fisher

P <- 1-pf(test.stat*(n-(q-1))/((q-1)*(n-1)),(q-1),n-(q-1))
# bizzarro sia diverso da fato
P

# non rifiuto, i prezzi sono davvero scontati del 20%

## punto b

means_d <- colMeans(diff)
Cov_d <- cov(diff)

## punto c
alpha <- 0.01
p <- 2

plot(diff, asp=1, pch=1, main='Dataset of the Differences',xlim = c(-20,20), ylim=c(-50,10))
#e <- ellipse(center=means_d, shape=Cov_d, radius=sqrt((n-1)*p/(n-p)*qf(1-alpha,p,n-p)),lty=2,col='blue',lwd=2)
e <- ellipse(center=means_d, shape=Cov_d, radius=sqrt(qchisq(1-alpha,p)),lty=2,col='red',lwd=2)
# come trovo assi ecc? in teoria ho tutto -> means, cov etc