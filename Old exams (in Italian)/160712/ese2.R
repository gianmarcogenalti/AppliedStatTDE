rm(list = ls())
df1 <- read.table('puffin-M.txt')
df2 <- read.table('puffin-F.txt')
head(df1)

pc.M <- princomp(df1)

load.tour <- pc.M$loadings
summary(pc.M)
#                          Comp.1    Comp.2    Comp.3
# Standard deviation     1.4264855 0.9789096 0.8780063
# Proportion of Variance 0.5406084 0.2545853 0.2048063
# Cumulative Proportion  0.5406084 0.7951937 1.0000000

plot(cumsum(pc.M$sd^2)/sum(pc.M$sd^2), type='b', xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h = 0.8, lty = 2, col = 'red')
# graphical representation of the loadings of the principal components
x11()
par(mfcol = c(3,1))
for(i in 1:3) barplot(load.tour[,i], ylim = c(-1, 1), main=paste("PC",i))
dev.off()
pc.F <- princomp(df2)

load.tour2 <- pc.F$loadings
summary(pc.F)
#                          Comp.1    Comp.2    Comp.3
# Standard deviation     1.6163288 1.1452895 0.9114018
# Proportion of Variance 0.5494418 0.2758626 0.1746956
# Cumulative Proportion  0.5494418 0.8253044 1.0000000
# graphical representation of the loadings of the principal components

plot(cumsum(pc.F$sd^2)/sum(pc.F$sd^2), type='b', xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h = 0.8, lty = 2, col = 'red')
x11()
par(mfcol = c(3,1))
for(i in 1:3) barplot(load.tour2[,i], ylim = c(-1, 1), main=paste("PC",i))
dev.off()

x11()
par(mfcol = c(3,2))
for(i in 1:3) barplot(load.tour[,i], ylim = c(-1, 1), main=paste("PC",i))
for(i in 1:3) barplot(load.tour2[,i], ylim = c(-1, 1), main=paste("PC",i))
dev.off()



