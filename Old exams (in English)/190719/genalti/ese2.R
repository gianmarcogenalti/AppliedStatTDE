rm(list = ls())
df <-read.table('buoys.txt')
head(df)

plot(df[,1],df[,2], pch = 16) # 3 clusters ci sembrano appropriati
dev.off()

nclust <- 3
#df <- scale(df)

df.e<- dist(df, method='euclidean') # distance matrix

## single linkage

df.es <- hclust(df.e, method='ward') # clustering gerarchico con ward linkage

#coph.es <- cophenetic(df.es)
#coph.es

#x11()
#par(mfrow= c(1,2))
#image(as.matrix(df.e), main='Euclidean', asp=1 )
#image(as.matrix(coph.es), main='Ward', asp=1 )

x11() # dendrogramma
plot(df.es, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(df.es, k=nclust) 
dev.off()
cluster.es <- cutree(df.es, k=nclust) 
cluster.es 

dim1 <- length(cluster.es[which(cluster.es == 1)]) # cluster di dimensione 
dim2 <- length(cluster.es[which(cluster.es == 2)]) # cluster di dimensione 
#dim3 <- length(cluster.es[which(cluster.es == 3)]) # cluster di dimensione 
dim1
dim2
dim3 
# clustering osceno

centroids <- apply (df, 2, function (x) tapply (x, cluster.es, mean))
centroids

plot(df[,1], df[,2], col = cluster.es, pch = 16)
points(centroids, col = 'blue', pch = 11)
#dev.off()

head(df)
pvals = c()
for(i in 1:nclust){
  pvals <- c(pvals,shapiro.test(df[which(cluster.es == i), 3])$p)
}
# pvals = 0.267776624, 0.033638471, 0.001070083 l'ipotesi di normalità viene a mancare per gli ultimi due clusters

bartlett.test(df$DO, cluster.es)
# pvalue = 0.08021

# sicuro la scelta del tipo di clustering non è felice

summary(aov(df$DO ~ cluster.es))

# c'è differenza significativa tra i clusters







