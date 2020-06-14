library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)
rm(list = ls())
df <- read.table('radioville.txt')
head(df)
D <- ifelse(df$D == 'U', 0,1)
df$D <- D
head(df)
coordinates(df) <- c('Long','Lat')

v <- variogram(Bq ~ D, data = df)
plot(v)
v.fit1 <- fit.variogram(v, vgm(1, "Sph", 0.4))
plot(v, v.fit1, pch = 3)
v.fit1

g.tr <- gstat(formula = Bq ~ D, data = df, model = v.fit1)
predict(g.tr, df[1,], BLUE = TRUE)
predict(g.tr, df[6,], BLUE = TRUE)


plot(v)
v.fit1 <- fit.variogram(v,vgm(1, "Sph", 1, 0.4))
plot(v, v.fit1, pch = 3)
v.fit1

g.tr <- gstat(formula = Bq ~ D, data = df, model = v.fit1)
predict(g.tr, df[1,], BLUE = TRUE)
predict(g.tr, df[6,], BLUE = TRUE)
