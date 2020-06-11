rm(list = ls())
df <- read.table('montserrat.txt')
head(df)
s0 = c(402476, 4605558) # top of the mountain

## Define the sample coordinates
coordinates(df) <- c('x','y')

# bubble plot(obj,zcol,...)
# key.space=location of the key
bubble(df,'speed',do.log=TRUE,key.space='bottom')
dev.off()

# scatterplot of speed with respect to distance from the top of the mountain
xyplot(df$speed ~ df$distance)
# Negative correlation: lower distance from the top of the mountain => higher speed
dev.off()

svgm1 <- variogram(speed ~ 1, data = df)
svgm2 <- variogram(speed ~ distance, data = df)


plot(svgm1, main = 'Variogram 1',pch=19)
plot(svgm2, main = 'Variogram 2',pch=19)
dev.off()

v.fit1 <- fit.variogram(svgm2, vgm(8, "Sph", 10))
plot(svgm2, v.fit1, pch = 3)
#  model    psill    range
#   Sph   8.052424 28.32929


g.tr <- gstat(formula = speed ~ distance, data = df, model = v.fit1)
newdat <- data.frame(x = s0[1], y = s0[2], distance = 0)
coordinates(newdat) <- c('x','y')
guess <- predict(g.tr, newdat, BLUE = TRUE)

# windspeed prediction = 49.15833
# variance of prediction = 1.808778



