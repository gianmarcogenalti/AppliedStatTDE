mu <- c(30,50)
S <- cbind(c(1,1),c(1,2))
#fisher <- ((q-1)*(n-1)/(n-(q-1)))*qf(1-alpha,(q-1),n-(q-1))

eig <- eigen(S)$vectors
eig_v <- eigen(S)$values

#r1 <- qf(0.99,2,10000000)
r <- sqrt(qchisq(0.99,2))

T2 <- cbind(inf = mu - r*sqrt(diag(S)),
            center = mu, 
            sup = mu + r*sqrt(diag(S)))

plot(mu[1],mu[2],asp=1, pch=5, main='Cryptozoology',xlim=c(20,40),ylim=c(40,60))
ellipse(center=mu, shape = S, radius = r, lwd=2, add = F, asp = 1) 


segments(T2[1,1],T2[2,1],T2[1,3],T2[2,1], col = 'red')

segments(T2[1,3],T2[2,1],T2[1,3],T2[2,3], col = 'red')

segments(T2[1,1],T2[2,3],T2[1,3],T2[2,3], col = 'red')

segments(T2[1,1],T2[2,1],T2[1,1],T2[2,3], col = 'red')

phi <- angle(eig[1,],c(0,1))
a <- sqrt(eig_v[1])*r
b <- sqrt(eig_v[2])*r
# 
# t <- seq(0, 2*pi, 0.01) 
# x <- mu[1] + a*cos(t)*cos(phi) - b*sin(t)*sin(phi)
# y <- mu[2] + a*cos(t)*cos(phi) + b*sin(t)*cos(phi)
# lines(x,y,pch=5, col='green')

######
point <- c(31,52)
distance <- rep(0,1000)
for (i in 1:1000){
  x <- rmvnorm(1,mu,S)
  distance[i] <- sum((x-point)^2)
}
mean(distance)

  
