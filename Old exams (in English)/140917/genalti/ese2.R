load("mcshapiro.test.Rdata")
df <- read.table("olives.txt")
head(df)
mcshapiro.test(df[,1:3]) # normale multivariato
attach(df)
shapiro.test(Tot[which(Restaurant == 'Dalla Luigina')])
shapiro.test(Tot[which(Restaurant != 'Dalla Luigina')])
var.test(Tot[which(Restaurant == 'Dalla Luigina')],Tot[which(Restaurant != 'Dalla Luigina')])
shapiro.test(Fill[which(Restaurant == 'Dalla Luigina')])
shapiro.test(Fill[which(Restaurant != 'Dalla Luigina')])
var.test(Fill[which(Restaurant == 'Dalla Luigina')],Fill[which(Restaurant != 'Dalla Luigina')])
shapiro.test(Bread[which(Restaurant == 'Dalla Luigina')])
shapiro.test(Bread[which(Restaurant != 'Dalla Luigina')])
var.test(Bread[which(Restaurant == 'Dalla Luigina')],Bread[which(Restaurant != 'Dalla Luigina')])
# tutte le variabili sono normali per ogni gruppo e con stesse varianze tra un ristorante e l'altro

man <- manova(cbind(Tot, Fill, Bread) ~ Restaurant)
summary.manova(man, test = "Wilks")
#             Df   Wilks approx F num Df den Df    Pr(>F)    
# Restaurant  1 0.12196   172.79      3     72 < 2.2e-16 ***
# Residuals  74                                             
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

detach(df)

p <- dim(df)[2]

n1 <- dim(df[which(df$Restaurant == 'Dalla Luigina'),1:3])[1]
n2 <- dim(df[which(df$Restaurant != 'Dalla Luigina'),1:3])[1]

t1.mean <- sapply(df[which(df$Restaurant == 'Dalla Luigina'),1:3],mean)
t2.mean <- sapply(df[which(df$Restaurant != 'Dalla Luigina'),1:3],mean)
t1.cov  <-  cov(df[which(df$Restaurant == 'Dalla Luigina'),1:3])
t2.cov  <-  cov(df[which(df$Restaurant != 'Dalla Luigina'),1:3])
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)
# we compare the matrices
list(S1=t1.cov, S2=t2.cov, Spooled=Sp)

# Test H0: mu1 == mu2  vs  H1: mu1 != mu2
# i.e.,
# Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)

alpha   <- .01
delta.0 <- c(0,0,0)
Spinv   <- solve(Sp)

T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)

cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
T2 < cfr.fisher

P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P  
# another evidence that the two restaurants have different olives

# Simultaneous T2 intervals
IC.T2.X1 <- c(t1.mean[1]-t2.mean[1]-sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)), t1.mean[1]-t2.mean[1]+sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)) )
IC.T2.X2 <- c(t1.mean[2]-t2.mean[2]-sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)), t1.mean[2]-t2.mean[2]+sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)) )
IC.T2.X3 <- c(t1.mean[3]-t2.mean[3]-sqrt(cfr.fisher*Sp[3,3]*(1/n1+1/n2)), t1.mean[3]-t2.mean[3]+sqrt(cfr.fisher*Sp[3,3]*(1/n1+1/n2)) )
IC.T2 <- rbind(IC.T2.X1, IC.T2.X2, IC.T2.X3)
dimnames(IC.T2)[[2]] <- c('inf','sup')                        
IC.T2
#               inf        sup
# IC.T2.X1 -10.363123 -7.1724325
# IC.T2.X2  -6.939987 -4.3422911
# IC.T2.X3  -1.024349 -0.2529841

# le medie delle varie dimensioni hanno una differenza sensibile, soprattutto nel totale





