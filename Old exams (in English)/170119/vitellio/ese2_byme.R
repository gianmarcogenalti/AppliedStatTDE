# ese 2

diam=read.table("diamonds.txt", header=T)
head(diam)

#FIRST STANDARDIZE
diam.sd <- scale(diam)
diam.sd <- data.frame(diam.sd)

plot(diam)
plot(diam.sd) #perchè c'erano unità di misura diverse: prima 10 - 2.5, ora 3 - 3


#based on Euclidean distance and Ward linkage
#Provide a guess on the possible number of diamonds types and on their percentages in the sample
satellite=diam.sd
D.s <- dist(satellite, method='euclidean')
HCa <- hclust(D.s, method = 'ward.D')

plot(HCa, hang=-0.1, sub='', xlab='', labels=F)

# we know that there are 3 clusters, so we cut the dendrograms 
# accordingly
sata <- cutree(HCa, k=3)

plot(satellite , col=sata+1, asp=1, pch=16)

table(sata)

coph.a <- cophenetic(HCa)

coph.sat <- cor(D.s, coph.a)
coph.sat


sata=as.vector(sata)
### question c)
p  <- 2 #ci faccio one way manova
g = 3
n1 <- table(sata)[1]
n2 <- table(sata)[2]
n3 = table(sata) [3]
ng=c(n1, n2, n3)

# prepare data
var.risp <- diam.sd
dim(var.risp)[2]

N <- sum(ng)

#verifico ipotesi:
#gaussianita -> malissimo
mcshapiro.test(nuovo[which(nuovo$sata==1),1:2])
mcshapiro.test(nuovo[which(nuovo$sata==2),1:2])
mcshapiro.test(nuovo[which(nuovo$sata==3),1:2])
#omogeneità varianze
S1 <-  cov(nuovo[which(nuovo$sata==1),1:2])
S2 <-  cov(nuovo[which(nuovo$sata==2),1:2])
S3 <-  cov(nuovo[which(nuovo$sata==3),1:2])


### One-way MANOVA:
### Model: X.ij = mu + tau.i + eps.ij; eps.ij~N_p(0,Sigma), [p=2]
###       X.ij, mu, tau.i in R^2, i=1,2,3 


# Fit the model:
fit <- manova(as.matrix(var.risp) ~ sata )
summary.manova(fit,test="Wilks")

# who's the responsible?
summary.aov(fit,test="Wilks")

# all the variable...
# let's see if there is difference in the levels of the treatment -> question c)

### question c)
alpha <- 0.05
k <- 6
k

qT <- qt(1-alpha/(2*k), N-g)

# I need the diagonal of W
fit$res   # residuals of the estimated model
W <- diag(t(fit$res) %*% fit$res)/(N-g)   
W
# mean within the groups
nuovo=cbind(satellite,sata)
m1 <- colMeans(nuovo[which(nuovo$sata==1),1:2])
m2 <- colMeans(nuovo[which(nuovo$sata==2),1:2])
m3 <- colMeans(nuovo[which(nuovo$sata==3),1:2])
m1
m2
m3


Bf12 <- cbind(m1-m2 - qt(1 -alpha/(2*k), N-g) * sqrt((1/ng[1]+1/ng[2])*W), m1-m2, m1-m2 + qt(1 -alpha/(2*k), N-g) * sqrt((1/ng[1]+1/ng[2])*W))
Bf23 <- cbind(m2-m3 - qt(1 -alpha/(2*k), N-g) * sqrt((1/ng[2]+1/ng[3])*W), m2-m3, m2-m3 + qt(1 -alpha/(2*k), N-g) * sqrt((1/ng[2]+1/ng[3])*W))
Bf31 <- cbind(m3-m1 - qt(1 -alpha/(2*k), N-g) * sqrt((1/ng[3]+1/ng[1])*W), m3-m1, m3-m1 + qt(1 -alpha/(2*k), N-g) * sqrt((1/ng[3]+1/ng[1])*W))

IC <- list(gemme1-gemme2=Bf12, gemme2-gemme3=Bf23, gemme3-gemme1=Bf31)
IC
#Using Bonferroni's intervals (with global confidence level of 95%) detect the differences among the different
#types of gems.
