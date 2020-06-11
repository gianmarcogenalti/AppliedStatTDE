###### esercizio fatto a caso#############

df <- read.table("tourists.txt")
head(df)
dim(df)

tourists.label <- df[,1:2]
tourists <- df[,-(1:2)]

n <- dim(tourists)[1]
p <- dim(tourists)[2]

# Boxplot
x11()
par(mar=rep(8,4))
boxplot(tourists, las=2, col='gold')

# We observe that the variability of the number of nights in 3,4 stars hotels and residences
# is higher than that of the others. This may influence the PCA

# We perform the PCA on original data
pc.tourists <- princomp(tourists, scores=T)
pc.tourists
summary(pc.tourists)

# To obtain the rows of the summary:
# standard deviation of the components
pc.tourists$sd
# proportion of variance explained by each PC
pc.tourists$sd^2/sum(pc.tourists$sd^2)
# cumulative proportion of explained variance
cumsum(pc.tourists$sd^2)/sum(pc.tourists$sd^2)


load.tour <- pc.tourists$loadings
load.tour

load.tour[,1:8]

# graphical representation of the loadings of the first six principal components
x11()
par(mfcol = c(4,2))
for(i in 1:8) barplot(load.tour[,i], ylim = c(-1, 1), main=paste("PC",i))

x11()
par(mfrow = c(3,1))
for(i in 1:3) barplot(load.tour[,i], ylim = c(-1, 1))

# Interpretation of the loadings:
# First PCs: weighted average of the number of nights in 3,4 stars hotel and residences
# Second PCs: contrast between the number of nights in 3 and 4 stars hotel
# Third PC: residences

# The loadings reflect the previous observation: the first 3 PCs are 
# driven by the variables displaying the highest variability



# Explained variance
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.tourists, las=2, main='Principal components', ylim=c(0,4.5e7))
barplot(sapply(tourists,sd)^2, las=2, main='Original Variables', ylim=c(0,4.5e7), ylab='Variances')
plot(cumsum(pc.tourists$sd^2)/sum(pc.tourists$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(tourists),labels=1:ncol(tourists),las=2)

# The first PC explains more than 98% of the total variability. 
# This is due to the masking effect of those 3 variables over the others

tourists <- scale(df[,-(1:2)])

# Boxplot
x11()
par(mar=rep(8,4))
boxplot(tourists, las=2, col='gold')

# We observe that the variability is uniform

# We perform the PCA on scaed data
pc.tourists <- princomp(tourists, scores=T)
pc.tourists
summary(pc.tourists)

# To obtain the rows of the summary:
# standard deviation of the components
pc.tourists$sd
# proportion of variance explained by each PC
pc.tourists$sd^2/sum(pc.tourists$sd^2)
# cumulative proportion of explained variance
cumsum(pc.tourists$sd^2)/sum(pc.tourists$sd^2)


load.tour <- pc.tourists$loadings
load.tour

load.tour[,1:8]

# graphical representation of the loadings of the first six principal components
x11()
par(mfcol = c(4,2))
for(i in 1:8) barplot(load.tour[,i], ylim = c(-1, 1), main=paste("PC",i))

x11()
par(mfrow = c(3,1))
for(i in 1:3) barplot(load.tour[,i], ylim = c(-1, 1))

# Interpretation of the loadings:
# First PCs: weighted average of all the variables
# Second PCs: 
# Third PC: B&B

# The loadings reflect the previous observation: the first 3 PCs are 
# driven by the variables displaying the highest variability



# Explained variance
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.tourists, las=2, main='Principal components')
barplot(sapply(tourists,sd)^2, las=2, main='Original Variables', ylab='Variances')
plot(cumsum(pc.tourists$sd^2)/sum(pc.tourists$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(tourists),labels=1:ncol(tourists),las=2)

# The first PC explains more than 89% of the total variability. 
# This is due to the masking effect of those 3 variables over the others
