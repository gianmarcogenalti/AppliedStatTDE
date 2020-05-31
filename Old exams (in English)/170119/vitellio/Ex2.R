rm(list = ls())
data <- read.table("diamonds.txt")
head(data)
datastd <- data.frame(scale(data))

plot(datastd)

diam.e <- dist(datastd, method='euclidean')
diam.es <- hclust(diam.e, method='ward.D')

x11()
plot(diam.es, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')

cluster.es <- cutree(diam.es, k=3) # euclidean-single
coph.es <- cophenetic(diam.es)
es <- cor(diam.e, coph.es)

table(cluster.es)/dim(data)[1]

alpha <- 0.05
k <- 6
plot(datastd, col = cluster.es+1)

i1 <- which(cluster.es == "1")
i2 <- which(cluster.es == "2")
i3 <- which(cluster.es == "3")

load("mcshapiro.test.RData")
mcshapiro.test(datastd[i1,])
mcshapiro.test(datastd[i2,])
mcshapiro.test(datastd[i3,])

n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)

mean1 <- sapply(datastd[i1,],mean)
mean1
mean2 <- sapply(datastd[i2,],mean)
mean2
mean3 <- sapply(datastd[i3,],mean)
mean3

S1 <- cov(datastd[i1,])
S2 <- cov(datastd[i2,])
S3 <- cov(datastd[i3,])

fit <- manova(as.matrix(datastd) ~ cluster.es)
# summary.manova(fit,test="Wilks")   # Exact tests for p<=2 or g<=3 already implemented in R

W  <- (n1-1)*S1 + (n2-1)*S2 + (n3-1)*S3
W1 <- summary.manova(fit)$SS$Residuals

### question c)
alpha <- 0.05
p <- 2
g <- 3
n <- n1+n2+n3
IC1 <- cbind(mean1 - mean2 - qt(1-alpha/(p*g*(g-1)), n-g)*sqrt(diag(W)/(n-g) *(1/n1 + 1/n2)),
            mean1 - mean2,
            mean1 - mean2 + qt(1-alpha/(p*g*(g-1)), n-g)*sqrt(diag(W)/(n-g) *(1/n1 + 1/n2)))

IC2 <- cbind(mean1 - mean3 - qt(1-alpha/(p*g*(g-1)), n-g)*sqrt(diag(W)/(n-g) *(1/n1 + 1/n3)),
            mean1 - mean3,
            mean1 - mean3 + qt(1-alpha/(p*g*(g-1)), n-g)*sqrt(diag(W)/(n-g) *(1/n1 + 1/n3)))

IC3 <- cbind(mean3 - mean2 - qt(1-alpha/(p*g*(g-1)), n-g)*sqrt(diag(W)/(n-g) *(1/n3 + 1/n2)),
            mean3 - mean2,
            mean3 - mean2 + qt(1-alpha/(p*g*(g-1)), n-g)*sqrt(diag(W)/(n-g) *(1/n3 + 1/n2)))
IC1
IC2
IC3

