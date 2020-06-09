geisha <- read.table("geisha.txt")

dist_eucl <- dist(geisha, method = "euclidean")
hclust_single <- hclust(dist_eucl, method = "single")

plot(hclust_single, labels = F, sub = "")
tree_single <- cutree(hclust_single, k = 2)

plot(geisha, col = tree_single)

hclust_f <- hclust(dist_eucl, method = "mcquitty")

tree_f <- cutree(hclust_f, k = 2)

plot(geisha, col = tree_f )

i1 <- which(tree_f == 1)
i2 <- which(tree_f == 2)

length(i1)
length(i2)

mean_s <- colMeans(geisha[i1,])
mean_f <- colMeans(geisha[i2,])

n <- dim(geisha)[1]
alpha <- 0.1
k <- 4
g <- 2

fit <- manova(as.matrix(geisha) ~ tree_f)
s <- summary(fit, test = "Wilks")

W <- s$SS$Residuals

Sd1 <- cov(geisha[i1,])

bonf.conf.1 <- cbind(inf = mean_s - mean_f - qt(1 - alpha/(2*k), n - g)*sqrt(diag(W)/(n-g)*(1/n1 + 1/n2)), sup = mean_s - mean_f + qt(1 - alpha/(2*k), n - g)*sqrt(diag(W)/(n-g)*(1/n1 + 1/n2)))
bonf.conf.1
bonf.conf.2 <- cbind(inf = mean_s - qt(1 - alpha/(2*k), n1-1)*sqrt(diag(Sd1)/n1), sup = mean_s + qt(1 - alpha/(2*k), n1-1)*sqrt(diag(Sd1)/n1))
bonf.conf.2

# conviene andare un po' prima e fare un giro lungo


