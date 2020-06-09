rm(list = ls())
df <- read.table('sanrocco.txt')
head(df)

g <- 2
b <- 2
p <- 1
N <- dim(df)[1]
detach(df)
attach(df)

# Verify the assumptions
# 1) normality (univariate) in each group
Ps <- c(shapiro.test(durata[ temp=='torrido' & vento=='ventilato' ])$p,
        shapiro.test(durata[ temp=='torrido' & vento=='fermo' ])$p,
        shapiro.test(durata[ temp=='caldo' & vento=='ventilato' ])$p,
        shapiro.test(durata[ temp=='caldo' & vento=='fermo' ])$p)
# pvalues = 0.3030448, 0.6067242, 0.6721671, 0.1251575

# 2) homogeneity of variances
bartlett.test(list(durata[ temp=='torrido' & vento=='ventilato' ],
                   durata[ temp=='torrido' & vento=='fermo' ],
                   durata[ temp=='caldo' & vento=='ventilato'],
                   durata[ temp=='caldo' & vento=='fermo' ]))
# pvalue = 0.251 
# ok le ipotesi

# Fit the model:
fit <- aov(durata ~ temp + vento)
summary(fit)

# vento è molto significativa ma temp no

names(fit)

# Estimate variances
W <- sum(fit$residuals^2)  # SS_res
var <- W/(N-g-b+1)     # SS_res/gdl(res)

# Estimate the great mean mu:
m <- mean(df[,1])

# Estimate tau.i, beta.j:

tauAC  <- mean(df[df$temp=='torrido',1]) - m  # tau.1
tauCA  <- mean(df[df$temp=='caldo',1]) - m  # tau.2

betaFest <- mean(df[df$vento=='ventilato',1]) - m  # beta.1
betaFer  <- mean(df[df$vento=='fermo',1]) - m  # beta.2

# Point-wise estimates of mean duration of travels
# (model without interaction!)
mAC_Fest <- m + tauAC + betaFest
mAC_Fer  <- m + tauAC + betaFer
mCA_Fest <- m + tauCA + betaFest
mCA_Fer  <- m + tauCA + betaFer

# Fit the model:
fit <- aov(durata ~ vento)
summary(fit)

# vento è molto significativa ma temp no

names(fit)

betaFest <- mean(df[df$vento=='ventilato',1]) # beta.1
betaFer  <- mean(df[df$vento=='fermo',1]) # beta.2



