df <- read.table('watertemp.txt')
library(fda)
time <- seq(1,365)

par(mfrow = c(1,2))

basis <- create.fourier.basis(rangeval=c(1,365),nbasis=45)
data.fd <- Data2fd(t(df[,-366]),time,basis)
plot.fd(data.fd, main="Fourier")


basis2 <- create.bspline.basis(rangeval=c(1,365),nbasis=45)
data.fd2 <- Data2fd(y = t(df[,-366]) ,argvals = time,basisobj = basis2)
plot.fd(data.fd2, main="B-splines")

data.fd$coefs[1:3,1]
data.fd$coefs[1:3,2]

# b

fpca <- pca.fd(data.fd,nharm=5,centerfns=TRUE)

plot(fpca$values[1:35],xlab='j',ylab='Eigenvalues')
#screeplot
plot(cumsum(fpca$values)[1:35]/sum(fpca$values),xlab='j',ylab='CPV',ylim=c(0.8,1))

#variance proportion
fpca$varprop

par(mfrow=c(1,3))
plot(fpca$harmonics[1,],col=1,ylab='FPC1',ylim=c(-0.1,0.08))
abline(h=0,lty=2)
plot(fpca$harmonics[2,],col=2,ylab='FPC2',ylim=c(-0.1,0.08))
abline(h=0,lty=2)
plot(fpca$harmonics[3,],col=2,ylab='FPC2',ylim=c(-0.1,0.08))

# c

plot(fpca$scores[,1],fpca$scores[,2],col = df[,366])
legend('topright',c('Deep', 'Medium', 'Surface'), col = c('black','red','green'), pch = 16)

# acque profonde -> più fredde -> comp  + negativa sulla prima fpca
# prima fpca -> temperatura alta (+ alta d'estate)
# seconda fpca -> temp + alta d'inverno e + bassa d'estate
# infatti -> sulla seconda asse non c'è variazione per profondità

# d

# if we needed to do dim red for regression -> first 2