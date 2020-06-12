rm(list = ls())
load('mcshapiro.test.RData')
df <- read.table('MM.txt')
head(df)
mcshapiro.test(df)#0.1412

n <- dim(df)[1]
x.cov <- cov(df)
k <- 6
alpha <- .1
IC.m <- c()
for(i in 1:3){
  IC.m<- rbind(IC.m,t.test(df[,i], conf.level = 1 - alpha/(2*k))$conf.int)
}
ICvar <- cbind(inf=diag(x.cov)*(n-1) / qchisq(1 - alpha/(2*k), n-1),
               center=diag(x.cov),
               sup=diag(x.cov)*(n-1) / qchisq(alpha/(2*k), n-1))

t.test(df[,1]-df[,3], conf.level = 0.9) #p-value = 6.18e-12 sono diversi
t.test(df[,2], conf.level = 0.95, mu = 30, method = 'greater')# p-value = 0.6555 è vero, non smentiamo


