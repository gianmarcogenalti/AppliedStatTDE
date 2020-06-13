df <- read.table('olives.txt')

mcshapiro.test(df[,1:3]) 
# normale

attach(df)

mcshapiro.test(df[which(Restaurant == levels(df[,4])[1]),1:3])
mcshapiro.test(df[which(Restaurant == levels(df[,4])[2]),1:3])
# tutto a posto comandante

var(df[which(Restaurant == levels(df[,4])[1]),1:3])
var(df[which(Restaurant == levels(df[,4])[2]),1:3])
# simili, ci va bene

m <- manova(as.matrix(df[,c(1,2,3)]) ~ Restaurant)
summary.manova(m, test = "Wilks")

# b




# we compute the sample mean, covariance matrices and the matrix
# Spooled
t1 <- df[which(Restaurant == levels(df[,4])[1]),1:3]
t2 <- df[which(Restaurant == levels(df[,4])[2]),1:3]

n1 <- dim(t1)[1]
n2 <- dim(t2)[1] 
p  <- dim(t1)[2] 

t1.mean <- sapply(t1,mean)
t2.mean <- sapply(t2,mean)
t1.cov  <-  cov(t1)
t2.cov  <-  cov(t2)
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)

cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)

# Simultaneous T2 intervals
total <- c(t1.mean[1]-t2.mean[1]-sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)), t1.mean[1]-t2.mean[1]+sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)) )
filling <- c(t1.mean[2]-t2.mean[2]-sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)), t1.mean[2]-t2.mean[2]+sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)) )
breading <- c(t1.mean[3]-t2.mean[3]-sqrt(cfr.fisher*Sp[3,3]*(1/n1+1/n2)), t1.mean[3]-t2.mean[3]+sqrt(cfr.fisher*Sp[3,3]*(1/n1+1/n2)) )
IC.T2 <- rbind(total,filling,breading)
dimnames(IC.T2)[[2]] <- c('inf','sup')                        
IC.T2

