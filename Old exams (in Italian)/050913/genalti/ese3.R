rm(list = ls())
df <- read.table('hotel.txt')
head(df)

boxplot(df)

P <- princomp(df, scores = T)
summary(P)

load.tour <- P$loadings

x11()
par(mfcol = c(2,2))
for(i in 1:4) barplot(load.tour[,i], ylim = c(-1, 1), main=paste("PC",i))
# la prima componente principale è una media degli incassi durante tutte le quattro stagioni dell'anno,
# la seconda mette in confronto l'incasso invernale con quello medio di tutte le altre stagioni,
# la terza esprime il contrasto tra l'incasso autunnale e quello primaverile, 
# infine la quarta esprime il contrasto tra autunno ed estate.

# noi scegliamo le prime due per il nostro modello

tourists <- df
pc.tourists <- P
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.tourists, las=2, main='Principal components', ylim=c(0,4.5e7))
barplot(sapply(tourists,sd)^2, las=2, main='Original Variables', ylim=c(0,4.5e7), ylab='Variances')
plot(cumsum(pc.tourists$sd^2)/sum(pc.tourists$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')

box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(tourists),labels=1:ncol(tourists),las=2)

dev.off()

plot(P$scores[,1], P$scores[,2])
