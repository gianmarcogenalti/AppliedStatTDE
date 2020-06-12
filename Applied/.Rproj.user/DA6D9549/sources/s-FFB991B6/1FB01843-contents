df <- read.table("Running.txt")

dist.eucl <- dist(df, method = "euclidean")
hclust.single <- hclust(dist.eucl, method = "single")

plot(hclust.single, labels = F, sub = "")
tree.single <- cutree(hclust.single, k = 2)

plot(df, col = tree.single)

i1 <- which(tree.single == 1)
i2 <- which(tree.single == 2)
length(i1)
length(i2)
colMeans(df[i1,])
colMeans(df[i2,])

# shitty clustering

hclust.f <- hclust(dist.eucl, method = "complete")

plot(hclust.f, labels = F, sub = "")
tree.f <- cutree(hclust.f, k = 2)

plot(df, col = tree.f)

i1 <- which(tree.f == 1)
i2 <- which(tree.f == 2)
length(i1)
length(i2)
colMeans(df[i1,])
colMeans(df[i2,])


####

n <- dim(df)[1]
alpha <- 0.05
k <- 4

mean1 <- mean(df[,1])
var1 <- var(df[,1])
mean2 <- mean(df[,2])
var2 <- var(df[,2])

IC1_2 <- c(var1 * (n-1) / qchisq(1-alpha/(2*k), n-1),var1 * (n-1) / qchisq(alpha/(2*k), n-1))
IC1_2
IC2_2 <- c(var2 * (n-1) / qchisq(1-alpha/(2*k), n-1),var2 * (n-1) / qchisq(alpha/(2*k), n-1))
IC2_2

IC1_1 <- c(mean1 - qt(1-alpha/(2*k), n-1) * sqrt(var1)/n ,mean1 + qt(1-alpha/(2*k), n-1) * sqrt(var1)/n)
IC1_1
IC1_1 <- c(mean2 - qt(1-alpha/(2*k), n-1) * sqrt(var2)/n ,mean2 + qt(1-alpha/(2*k), n-1) * sqrt(var2)/n)
IC1_1
