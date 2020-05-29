df <- read.table('geisha.txt')
head(df)
# mischiamo i dati
misc <- sample(n)
df <- df[misc,]
n = dim(df)[1]

df.e<- dist(df, method='euclidean') # distance matrix
x11()
plot(df$duration, df$time, pch = 16)

## single linkage

df.es <- hclust(df.e, method='single') # clustering gerarchico con single linkage

help(hclust) # vediamo gli attributi
df.es$merge
df.es$height
df.es$order
df.es$labels

coph.es <- cophenetic(df.es)
coph.es

x11()
par(mfrow= c(1,2))
image(as.matrix(df.e), main='Euclidean', asp=1 )
image(as.matrix(coph.es), main='Single', asp=1 )

x11() # dendrogramma
plot(df.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(df.es, k=2) # ok fa schifo sto clustering

help(cutree)
cluster.es <- cutree(df.es, k=2) 
cluster.es 

dim1 <- length(cluster.es[which(cluster.es == 2)]) # cluster di dimensione 1
dim2 <- length(cluster.es[which(cluster.es == 1)]) # cluster di dimensione 159 
dim1
dim2
# cluster osceno, solo il 53 viene classificato in un cluster diverso dagli altri
# per sport vediamo i centroidi dei cluster

centroids <- apply (df, 2, function (x) tapply (x, cluster.es, mean))
centroids

x11()
plot(df$duration, df$time, pch = 16)
points(centroids, col = 'red', pch = 11)

# questo linkage è assolutamente inaccettabile, ripetiamo tutto modificando il method alla riga 14


df.es <- hclust(df.e, method='complete') # clustering gerarchico con single linkage

help(hclust) # vediamo gli attributi
df.es$merge
df.es$height
df.es$order
df.es$labels

coph.es <- cophenetic(df.es)
coph.es

x11()
par(mfrow= c(1,2))
image(as.matrix(df.e), main='Euclidean', asp=1 )
image(as.matrix(coph.es), main='Single', asp=1 )

x11() # dendrogramma
plot(df.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(df.es, k=2) # ora si inizia a ragionare

help(cutree)
cluster.es <- cutree(df.es, k=2) 
cluster.es 

dim1 <- length(cluster.es[which(cluster.es == 2)]) # cluster di dimensione 85
dim2 <- length(cluster.es[which(cluster.es == 1)]) # cluster di dimensione 75 
dim1
dim2

# centroidi

centroids <- apply (df, 2, function (x) tapply (x, cluster.es, mean))
centroids

x11()
plot(df$duration, df$time, pch = 16)
points(centroids, col = 'red', pch = 11)

# questo clustering con complete linkage ci sembra molto soddisfacente

# consideriamo il cluster 1 (dim. 75) come quelli che hanno vinto al gioco
win <- rep(NA, n)
win[which(cluster.es == 1)] <- 1 # hanno vinto
win[which(cluster.es == 2)] <- 0 # hanno perso

df$winner <- win

#D <- data.frame(diff_dur = df[which(df$winner == 1),]$duration - df[which(df$winner == 0),]$duration,
                #diff_time = df[which(df$winner == 1),]$time - df[which(df$winner == 0),]$time,
                #winner_dur = df[which(df$winner == 1),]$duration,
                #winner_time = df[which(df$winner == 1),]$time)

dur       <- df$dur
time      <- df$time
win   <- factor(df$winner)

g <- length(levels(win))
n <- length(dur)/g

Mdur           <- mean(dur)
Mtime          <- mean(time)
Mwin_dur       <- tapply(dur, win, mean)
Mwin_time      <- tapply(time,win, mean)

a <-4
a*(a-1)/2 # numero di comparazioni

SStype <- sum(n*b*(Mtype - M)^2)                
SScity  <- sum(n*g*(Mcity  - M)^2)            
SSres   <- sum((kvalue - M)^2) - (SStype+SScity)
### Interval at 95% for the differences (reduced additive model)
### [b=2, thus one interval only]
IC <- c(diff(Mtype) - qt(0.975, (n*g-1)*b) * sqrt(SSres/((n*g-1)*b) *(1/(n*g) + 1/(n*g))), 
        diff(Mtype) + qt(0.975, (n*g-1)*b) * sqrt(SSres/((n*g-1)*b) *(1/(n*g) + 1/(n*g))))
names(IC) <- c('Inf', 'Sup')
IC    # IC for mu(ready-to-use)-mu(hand-made)






