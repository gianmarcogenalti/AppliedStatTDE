df <- read.table("running.txt")
head(df)
smpar <- df$S.Mar.Paraggi
parpor <- df$Paraggi.Portofino

plot(smpar, parpor, pch = 16)

df.e<- dist(df, method='euclidean') # distance matrix

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
# cluster osceno, solo il 70 viene classificato in un cluster diverso dagli altri
# per sport vediamo i centroidi dei cluster

centroids <- apply (df, 2, function (x) tapply (x, cluster.es, mean))
centroids

x11()
plot(smpar, parpor, pch = 16)
points(centroids, col = 'red', pch = 11)


# questo linkage è assolutamente inaccettabile, ripetiamo tutto modificando il method alla riga 12


df.es <- hclust(df.e, method='complete') # clustering gerarchico

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

dim2 <- length(cluster.es[which(cluster.es == 2)]) e
dim1 <- length(cluster.es[which(cluster.es == 1)]) 
dim2
dim1

# centroidi

centroids <- apply (df, 2, function (x) tapply (x, cluster.es, mean))
centroids

x11()
plot(smpar, parpor, pch = 16)
points(centroids, col = 'red', pch = 11)

# questo clustering con complete linkage ci sembra molto soddisfacente
# immagino che quelli con tempi più brevi avranno corso mentre gli altri camminato
n <- dim(df)[1]
run <- rep(NA, n)
run <- ifelse(cluster.es == 1, 1, 0)

x11()
plot(smpar, parpor, col = factor(run), pch = 16)

n1 <- dim1
smpar1 <- smpar[which(run == 1)]
parpor1 <- parpor[which(run == 1)]

shapiro.test(smpar1) # pval= 0.06681 posso accettare, al pelo -> tratto come normale
shapiro.test(parpor1) # pval = 0.1011 posso accettare -> tratto come normale

alpha = .5
k = 4

M1 <- mean(smpar1)
M2 <- mean(parpor1)

S1 <- var(smpar1)
S2 <- var(parpor1)

# IC for mean time in smaria - paraggi
IC.sp <- c(M1 - qt(1-alpha/(2*k), n1-1)* sqrt(S1/n1),M1 + qt(1-alpha/(2*k), n1-1)* sqrt(S1/n1))
names(IC.sp) <- c('Inf', 'Sup')
IC.sp


# IC for mean time in paraggi - portofino
IC.pp <- c(M2 - qt(1-alpha/(2*k), n1-1)* sqrt(S2/n1),M2 + qt(1-alpha/(2*k), n1-1)* sqrt(S2/n1))
names(IC.pp) <- c('Inf', 'Sup')
IC.pp

# IC for variance of time in smaria - paraggi
IC.vsp <- c(S1* (n1-1) /qchisq(alpha/(2*k), n1-1), S1* (n1-1) /qchisq(1 - alpha/(2*k), n1-1))
names(IC.vsp) <- c('Inf', 'Sup')
IC.vsp

# IC for variance of time in paraggi - portofino
IC.vsp <- c(S2* (n1-1) /qchisq(alpha/(2*k), n1-1), S2* (n1-1) /qchisq(1 - alpha/(2*k), n1-1))
names(IC.vsp) <- c('Inf', 'Sup')
IC.vsp




