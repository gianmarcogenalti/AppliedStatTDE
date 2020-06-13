wideroe <- read.table("wideroe.txt")
norwegian <- read.table("norwegian.txt")

df <- rbind(wideroe, norwegian)
df$C <- rep(c(0,1), each = 85)

fit <- lm(durata ~ C*tratte, data = df)
summary(fit)
vif(fit)

library(car)

linearHypothesis(fit, rbind(c(0,1,0,0), c(0,0,0,1)))

# io lascerei tutto -> al massimo tolgo C, ma è significativo

# d
x = 508
while(coefficients(fit)[1]+coefficients(fit)[3]*x<coefficients(fit)[1]+coefficients(fit)[2]+coefficients(fit)[3]*x+coefficients(fit)[4]*x){
  x = x+0.001
}

# se x>508.19 -> norwegian

# e

# norwegian

a <- c(0, -1, 0, -950)
n <- 170

alpha <- 0.05

conf.int <- c(inf = a%*%coefficients(fit) - sqrt(t(a)%*%vcov(fit)%*%a)*qt(1 - alpha/2, n - 4), sup = a%*%coefficients(fit) + sqrt(t(a)%*%vcov(fit)%*%a)*qt(1 - alpha/2, n - 4))
conf.int
