rm(list = ls())
load('mcshapiro.test.RData')
df <- read.table('genes.txt')
head(df)

plot(df[,1],df[,2],pch = 16) # si vedono tre cluster
k <- 3

result.k <- kmeans(df, centers=k)
labels <- result.k$cluster

df$lab <- labels
plot(df[,1],df[,2],pch = 16, col = df[,3]) # ci sembra una ottima clusterizzazione
n1 <- sum(df[,3] == 1)
n2 <- sum(df[,3] == 2)
n3 <- sum(df[,3] == 3)

# verifichiamo le ipotesi
mcshapiro.test(df[df[,3] == 1,1:2])
mcshapiro.test(df[df[,3] == 2,1:2])
mcshapiro.test(df[df[,3] == 3,1:2])

shapiro.test(df[df[,3] == 1,1]) #0.4772
shapiro.test(df[df[,3] == 1,2]) #0.7038
shapiro.test(df[df[,3] == 2,1]) #0.4739
shapiro.test(df[df[,3] == 2,2]) #0.09144
shapiro.test(df[df[,3] == 3,1]) #0.009124
shapiro.test(df[df[,3] == 3,2]) #0.06621

boxplot(cbind(X19_A, X19_C) ~ factor(lab), data = df)
mod <- manova(cbind(X19_A, X19_C) ~ factor(lab), data = df)
summary.manova(mod, test = 'Wilks')
#                Df    Wilks approx F num Df den Df    Pr(>F)    
#  factor(lab)    2 0.017739   6495.1      4   3992 < 2.2e-16 ***
#  Residuals   1997

boxplot(X19_A ~ factor(lab), data = df)
mod <- aov(X19_A ~ factor(lab), data = df)
summary(mod)
#                Df  Sum Sq Mean Sq F value Pr(>F)    
#  factor(lab)    2 8830698 4415349   43495 <2e-16 ***
#  Residuals   1997  202722     102      

boxplot(X19_C ~ factor(lab), data = df)
mod <- aov(X19_C ~ factor(lab), data = df)
summary(mod)
#                Df Sum Sq Mean Sq F value Pr(>F)
#  factor(lab)    2     60   29.96   0.312  0.732
#  Residuals   1997 192040   96.16  


