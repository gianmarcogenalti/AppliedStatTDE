rm(list = ls())
df <- read.table('gene.txt')
dim(df)

df.e<- dist(df, method='manhattan') # distance matrix

## single linkage

df.es <- hclust(df.e, method='ward.D2') # clustering gerarchico con single linkage

x11() # dendrogramma
plot(df.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(df.es, k=3) 

help(cutree)
cluster.es <- cutree(df.es, k=3) 
cluster.es 

dim1 <- length(cluster.es[which(cluster.es == 1)]) # cluster di dimensione 1
dim2 <- length(cluster.es[which(cluster.es == 2)]) # cluster di dimensione 159 
dim3 <- length(cluster.es[which(cluster.es == 3)])
dim1
dim2
dim3
# cluster osceno, solo il 70 viene classificato in un cluster diverso dagli altri
# per sport vediamo i centroidi dei cluster

centroids <- apply (df, 2, function (x) tapply (x, cluster.es, mean))
centroids

df$clu <- factor(cluster.es)

pv = c()
for(i in 1:1200){
  pv = c(pv, summary(aov(df[,i]~ df[,1201]))[[1]][["Pr(>F)"]])
}
ps <- na.omit(pv)
ps[ps < 0.01]

pa <- p.adjust(ps, method = 'fdr')
pa[pa < 0.01]
(1:1200)[pa < 0.01]# 200 300 403 405 500

  
  