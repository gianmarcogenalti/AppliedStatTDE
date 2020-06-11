df <- read.table('buoys.txt')

eu <- dist(df[,1:2], method='euclidean')
ward <- hclust(eu, method='ward.D2')
plot(ward, labels = F, sub = "")
tree <- cutree(ward, k = 3)

plot(df[,1:2], col = tree)

i1 <- which(tree == 1)
i2 <- which(tree == 2)
i3 <- which(tree == 3)
length(i1)
length(i2)
length(i3)
colMeans(df[i1,])
colMeans(df[i2,])
colMeans(df[i3,])

# b

shapiro.test(df[i1,3])
shapiro.test(df[i2,3])
shapiro.test(df[i3,3])
# supernormali

bartlett.test(df$DO, tree) # yuppppp

anova <- aov(DO ~ tree, data = df)
summary(anova)
# difficult to say, hypotheses are verified, there is no
# evidence at 5%, but we do have evidence at 10%
