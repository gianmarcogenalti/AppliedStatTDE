library(MASS)
load("mcshapiro.test.RData")
df <- read.table("horsecolic.txt")
head(df)
ipain <- which(df$Pain == 'Yes')
mcshapiro.test(df[ipain,1:4])
mcshapiro.test(df[-ipain,1:4])
# ok abbiamo dei dati normali multivariati
Ps <- c( shapiro.test(df[ipain,1])$p,
         shapiro.test(df[-ipain,1])$p,
         shapiro.test(df[ipain,2])$p,
         shapiro.test(df[-ipain,2])$p,
         shapiro.test(df[ipain,3])$p,
         shapiro.test(df[-ipain,3])$p,
         shapiro.test(df[ipain,4])$p,
         shapiro.test(df[-ipain,4])$p
         )
#Ps: 0.43570192 0.44362794 0.53201909 0.82669401 0.10293848 0.66937386 0.85577401 0.06890831 tutto considerabile normale

fit <- manova(as.matrix(df[,1:4]) ~ df$Pain)
summ <- summary.manova(fit, test = "Wilks")

W <- summ$SS$Residuals

k <- 4
alpha <- .01
means1 <- colMeans(df[ipain,1:4])
means2 <- colMeans(df[-ipain,1:4])
g <- 2
N <- dim(df)[1]
n1 <- length(ipain)
n2 <- N - n1

bonf.conf.X1 <- c(inf = means1[1] - means2[1] - qt(1 - alpha/(2*k), N - g)*sqrt((W[1,1]/(N-g))*(1/n1 + 1/n2)),
                  sup = means1[1] - means2[1] + qt(1 - alpha/(2*k), N - g)*sqrt((W[1,1]/(N-g))*(1/n1 + 1/n2)))
bonf.conf.X2 <- c(inf = means1[2] - means2[2] - qt(1 - alpha/(2*k), N - g)*sqrt((W[2,2]/(N-g))*(1/n1 + 1/n2)),
                  sup = means1[2] - means2[2] + qt(1 - alpha/(2*k), N - g)*sqrt((W[2,2]/(N-g))*(1/n1 + 1/n2)))
bonf.conf.X3 <- c(inf = means1[3] - means2[3] - qt(1 - alpha/(2*k), N - g)*sqrt((W[3,3]/(N-g))*(1/n1 + 1/n2)),
                  sup = means1[3] - means2[3] + qt(1 - alpha/(2*k), N - g)*sqrt((W[3,3]/(N-g))*(1/n1 + 1/n2)))
bonf.conf.X4 <- c(inf = means1[4] - means2[4] - qt(1 - alpha/(2*k), N - g)*sqrt((W[4,4]/(N-g))*(1/n1 + 1/n2)),
                  sup = means1[4] - means2[4] + qt(1 - alpha/(2*k), N - g)*sqrt((W[4,4]/(N-g))*(1/n1 + 1/n2)))

ICBF <- c(bonf.conf.X1, bonf.conf.X2, bonf.conf.X3, bonf.conf.X4)
#  inf.Rectal.temperature  sup.Rectal.temperature   
#  -0.05794649             0.23608499                     
#  inf.Pulse               sup.Pulse
#  31.49770325             41.76552030 
#  inf.Respiratory.rate    sup.Respiratory.rate 
#  16.06935433             23.56140459            
#  inf.Packed.cell.volume  sup.Packed.cell.volume 
#  -0.46107488             4.13003873 

# Rectal.temperature e Packed.cell.volume intersecano lo 0, ho quindi motivo di pensare che non si distinguano
# tra i due gruppi

var.test(df[,1]~ df[,5])
summary(aov(df[,1] ~ df[,5]))
# p-value = 0.0657 Rectal.temperature rispetta le ipotesi della anova e ha un pvalue sufficiente per dirci
# con confidenza del 95% che non è differente tra le due famiglie
var.test(df[,4]~ df[,5])
summary(aov(df[,4] ~ df[,5]))
# p-value = 0.0154 anche Packed.cell.volume non può considerarsi con confidenza del 99% differente tra le due
# famiglie 
summary(aov(df[,2] ~ df[,5]))
summary(aov(df[,3] ~ df[,5]))
# gli altri due valori invece hanno p-value pressoché nullo

pul <- df$Pulse
rr <- df$Respiratory.rate
pain <- df$Pain
plot(pul, rr, col = factor(pain), pch = 16)
M1 <- colMeans(df[ipain,c(2,3)])
M2 <- colMeans(df[-ipain,c(2,3)])
points(M1,M2, col = 'blue', pch= 11)


class <- lda(df[,c(2,3)],df[,5])
class$prior
#    No       Yes 
#0.7366667 0.2633333 
fitted <- predict(class)
tab <- table(pain,fitted$class)
tab
#pain    No  Yes
#No     214   7
#Yes     11  68
par(mfrow = c(1,2))
plot(pul, rr, col = factor(pain), pch = 16)
plot(pul, rr, col = factor(fitted$class), pch = 16)
plot(class, dimen = 2, type = "b")

x11()
plot(pul, rr, col = factor(fitted$class), pch = 16)
x  <- seq(min(df[,2]), max(df[,2]), length=300)
y  <- seq(min(df[,3]), max(df[,3]), length=300)
xy <- expand.grid(Pulse=x, Respiratory.rate=y)

z  <- predict(class, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]  

contour(x, y, matrix(z1, 300), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 300), levels=0, drawlabels=F, add=T)
dev.off()

APER <- (tab[1,2] + tab[2,1])/length(pain)
#APER = 0.06







