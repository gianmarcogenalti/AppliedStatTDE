df <- read.table('energy.txt')

df$we <- ifelse(df[,1]=='sabato' | df[,1] == 'domenica',1,0 )

i = 2
j = 1

d <- data.frame(cons=numeric(28*24),
                we = numeric(28*24), 
                hour =numeric(28*24))

count = 1

for (i in 2:(length(df[1,])-1)){
  
  for (j in 1:length(df[,1])){

    print(i)
    print(df[j,i])
    d$cons[count] <- df[j,i]
    d$we[count] <- df[j,26]
    d$hour[count] <- i-1
    count = count + 1
    
  }
}
d$t <- 1-cos(d$hour*2*pi/24)

d$we <- as.factor(d$we)

fit <- lm(cons ~ we + we:t, data= d )
summary(fit)

fit1 <- lm(cons ~  we:t, data= d )
summary(fit1)

Z0.new <- data.frame(we= levels(d$we)[1], t = 12)

# bonferroni?
k = 3

Conf <- predict(fit1, Z0.new, interval='confidence', level = 1 - 0.1/k)  
Conf 

Z1.new <- data.frame(we= levels(d$we)[1], t = 18)

Conf1 <- predict(fit1, Z1.new, interval='confidence',alpha = 1 - 0.1/k)  
Conf1 

var1 <- var(fit1$residuals)
n <- length(fit1$residuals)
alpha = 0.1
CI_var <- c((n-1)*var1/qchisq(1-alpha/(2*k),n-1),(n-1)*var1/qchisq(alpha/(2*k),n-1))
CI_var

# d ed e non capisco la consegna