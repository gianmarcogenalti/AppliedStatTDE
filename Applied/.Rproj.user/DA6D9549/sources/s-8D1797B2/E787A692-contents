df <- read.table('sanrocco.txt')

attach(df)

shapiro.test(df[,1])

ven  <- levels(vento)
tem  <- levels(temp)

shapiro.test(durata[vento == ven[1]])
shapiro.test(durata[vento == ven[2]])
shapiro.test(durata[temp == tem[1]])
shapiro.test(durata[temp == tem[2]])

var(durata[vento == ven[1]])
var(durata[vento == ven[2]])
var(durata[temp == tem[1]])
var(durata[temp == tem[2]])


shapiro.test(durata[temp == tem[1] & vento == ven[2]])
shapiro.test(durata[temp == tem[1] & vento == ven[1]])
shapiro.test(durata[temp == tem[2] & vento == ven[1]])
shapiro.test(durata[temp == tem[2] & vento == ven[2]])

var(durata[temp == tem[1] & vento == ven[2]])
var(durata[temp == tem[1] & vento == ven[1]])
var(durata[temp == tem[2] & vento == ven[1]])
var(durata[temp == tem[2] & vento == ven[2]])

bartlett.test(list(durata[temp == tem[1] & vento == ven[2]],
                   durata[temp == tem[1] & vento == ven[1]],
                   durata[temp == tem[2] & vento == ven[1]],
                   durata[temp == tem[2] & vento == ven[2]]))

# abbastanza rispettate

anova <- aov(durata ~ temp + vento, data = df)
summary(anova)

g <- 2
b <- 2
N <- length(df[,1])


W <- sum(anova$residuals^2)  # SS_res
variance <- W/(N-g-b+1)     # SS_res/gdl(res)
variance

m <- mean(df[,1])

tau_1  <- mean(durata[temp == tem[1]]) - m  # tau.1
#tau_2  <- mean(durata[temp == tem[2]]) - m  # tau.2

beta_1 <- mean(durata[vento==ven[1]]) - m  # beta.1

#means 

mean_11 <- m + tau_1 + beta_1
#et cetera


# <- aov(durata ~ vento, data = df)
#summary(anova)
