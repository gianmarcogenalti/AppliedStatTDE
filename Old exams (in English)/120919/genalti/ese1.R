df <- read.table("sequoia.txt")
head(df)

plot(df$height, df$diameter)
# si vedono 5 clusters

df.e<- dist(df, method='euclidean')

df.es <- hclust(df.e, method='ward.D')

coph.es <- cophenetic(df.es)

x11() # dendrogramma
plot(df.es, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(df.es, k=5)

cluster.es <- cutree(df.es, k=5) 
cluster.es 

df$clu <- cluster.es

plot(df$height, df$diameter, col = cluster.es)

cldim <- rep(NA,5)

for(i in 1:5){
  cldim[i] <- length(cluster.es[which(cluster.es== i)])
}

cldim
# dimensioni dei clusters

centroids <- apply (df, 2, function (x) tapply (x, cluster.es, mean))
centroids
#centroidi

points(centroids, col = 'orange', pch = 16)

# ci sembra suggerire che ci siano stati quattro grandi incendi, ognuno dei quali ha attaccato una parte del parco,
# il cluster celeste in alto a destra, di sequoie molto alte e grandi, parrebbe essere quello di quelle intatte
p <- rep(NA, 5)
for(i in 1:5){
  p[i] <- shapiro.test(df[which(df$clu == i), 2])$p
}
p
# ok sono tutti normali -> IC di confidenza per medie e varianze di variabili normali

n_intervals <- 10
alpha <- .1

ICmeanvar <- function(x, alpha, n_int, type = 'mean'){
  n <- length(x)
  M <- mean(x)
  S <- var(x)
  IC.mean <- c(M - qt(1-alpha/(2*n_int), n-1)* sqrt(S/n), M + qt(1-alpha/(2*n_int), n-1)* sqrt(S/n))
  names(IC.mean) <- c('Inf', 'Sup')
  
  IC.var <- c(S* (n-1) /qchisq(alpha/(2*n_int), n-1), S* (n-1) /qchisq(1 - alpha/(2*n_int), n-1))
  names(IC.var) <- c('Inf', 'Sup')
  
  if(type == 'mean'){
    return(IC.mean)
  }
  if(type == 'var'){
    return(IC.var)
  }
}


for(i in 1:5){
  print(paste0('Mean of Cluster ',paste(ICmeanvar(df[which(df$clu == i),2], alpha, n_intervals, type = 'mean')[1],
                                         ICmeanvar(df[which(df$clu == i),2], alpha, n_intervals, type = 'mean')[2])))
  print(paste0('Var of Cluster ', paste(ICmeanvar(df[which(df$clu == i),2], alpha, n_intervals, type = 'var')[1],
                                         ICmeanvar(df[which(df$clu == i),2], alpha, n_intervals, type = 'var')[2])))
}

 # "Mean of Cluster 1    7.17075311121114 7.59510054732544"
 # "Var of Cluster  1    0.826840672194497 0.365627586177067"
 # "Mean of Cluster 2    5.90812307433339 6.31534631342172"
 # "Var of Cluster  2    0.879895015785456 0.417848041852536"
 # "Mean of Cluster 3    6.73439941844886 7.16871746466802"
 # "Var of Cluster  3    0.823680578034728 0.354578028215813"
 # "Mean of Cluster 4    7.71385634312306 8.43654365687694"
 # "Var of Cluster  4    1.01296656585437 0.219814554390001"
 # "Mean of Cluster 5    11.6055312084684 12.1928021248649"
 # "Var of Cluster  5    0.45324283379977 0.0440968093944876"




