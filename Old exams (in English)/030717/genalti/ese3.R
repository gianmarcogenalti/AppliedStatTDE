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
image(as.matrix(coph.es), main='Complete', asp=1 )

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

n1 <- dim(df[which(df$winner == 1),])[1]
n2 <- dim(df[which(df$winner == 0),])[1]

plot(df$duration, df$time, pch = 16, col = factor(df$win))

var.test(duration ~ winner, data = df)
# we assume equal variance for duration
var.test(time ~ winner, data = df)
# we assume different variance for time

alpha = .05
k = 4 # to apply bonferroni correction

# IC for duration
Md1 <- mean(df[which(df$winner == 1),1])
Md2 <- mean(df[which(df$winner == 0),1])
Sd1 <- var(df[which(df$winner == 1),1])
Sd2 <- var(df[which(df$winner == 0),1])

Spd <- sqrt(((n1-1)*Sd1 + (n2-1)*Sd2)/(n1+n2-2))

IC.d <- c(Md1 - Md2 - qt(1-alpha/(2*k), n1+n2-2) * Spd * sqrt(1/n1+1/n2), Md1 - Md2 + qt(1-alpha/(2*k), n1+n2-2) * Spd * sqrt(1/n1+1/n2))
names(IC.d) <- c('Inf', 'Sup')
IC.d   

# IC for time
Mt1 <- mean(df[which(df$winner == 1),2])
Mt2 <- mean(df[which(df$winner == 0),2])
St1 <- var(df[which(df$winner == 1),2])
St2 <- var(df[which(df$winner == 0),2])

v <- ((St1/n1+St2/n2)^2)/((St1^2)/((n1^2)*(n1-1)) + (St2^2)/((n2^2)*(n2-1)))

IC.t <- c(Mt1 - Mt2 - qt(1-alpha/(2*k), v) * sqrt((St1/n1+St2/n2)^2), Mt1 - Mt2 + qt(1-alpha/(2*k), v) * sqrt((St1/n1+St2/n2)^2))
names(IC.t) <- c('Inf', 'Sup')
IC.t

# IC for mean duration in winners
shapiro.test(df[which(df$winner == 1),1])

IC.wd <- c(Md1 - qt(1-alpha/(2*k), n1-1)* sqrt(Sd1/n1),Md1 + qt(1-alpha/(2*k), n1-1)* sqrt(Sd1/n1))
names(IC.wd) <- c('Inf', 'Sup')
IC.wd

# IC for mean time in winners
shapiro.test(df[which(df$winner == 1),2])

IC.wt <- c(Mt1 - qt(1-alpha/(2*k), n1-1)* sqrt(St1/n1),Mt1 + qt(1-alpha/(2*k), n1-1)* sqrt(St1/n1))
names(IC.wt) <- c('Inf', 'Sup')
IC.wt

IC.d
IC.t
IC.wd 
IC.wt

# c'è evidenza per dire che i vincitori inizino prima il tour, tra le 16:40 e le 16:50, inoltre mediamente il tour
# dei vincitori dura di più dei perdenti, di circa 40-50 minuti. Il consiglio che do è di iniziare il tour tra e 16:40 e le 16:50
# e di continuare per almeno un'ora e mezza la ricerca!




