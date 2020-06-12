rm(list = ls())
load('mcshapiro.test.RData')
df <- read.table('gemelli.txt')
head(df)
levels(df$tipo)# "GB" "GM" "NG"

boxplot(df[,1]~df[,3])
boxplot(df[,2]~df[,3])

i.b <- df[,3] == 'GB'
i.m <- df[,3] == 'GM'
i.n <- df[,3] == 'NG'

mcshapiro.test(df[i.b,1:2]) # 0.5332
mcshapiro.test(df[i.m,1:2]) # 0.9012
mcshapiro.test(df[i.n,1:2]) # 0.8116

bartlett.test(eg + peso ~ tipo, data = df) # 0.1519 
# tutte le ipotesi della manova sono soddisfatte

mod <- manova(as.matrix(df[,1:2]) ~ df[,3])
summary.manova(mod,test="Wilks")
# forte evidenza che il fattore influenzi eg e peso
alpha = .1
k <- 6
n1 <- sum(i.b)
n2 <- sum(i.m)
n3 <- sum(i.n)
n <- n1+n2+n3
cfr.t <- qt(1-alpha/(2*k), n-1)
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)
# Conf int for the means
IC.T2.X1 <- c(t1.mean[1]-t2.mean[1]-sqrt(cfr.t*Sp[1,1]*(1/n1+1/n2)), t1.mean[1]-t2.mean[1]+sqrt(cfr.t*Sp[1,1]*(1/n1+1/n2)) )
IC.T2.X2 <- c(t1.mean[2]-t2.mean[2]-sqrt(cfr.t*Sp[2,2]*(1/n1+1/n2)), t1.mean[2]-t2.mean[2]+sqrt(cfr.t*Sp[2,2]*(1/n1+1/n2)) )
IC.T2 <- rbind(IC.T2.X1, IC.T2.X2)
dimnames(IC.T2)[[2]] <- c('inf','sup')                        
IC.T2
