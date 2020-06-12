rm(list = ls())
df <- read.table('library.txt')
head(df)

# assumo la normalità dei dati e faccio 4 intervalli di bonferroni com conf. globale 90%

sm <- df[,1]+df[,2]+df[,3]+df[,4]
df$total <- sm

alpha <- .1
k <- 5

M <- colMeans(df)
V <- cov(df)
n <- dim(df)[1]

cfr.t <- qt(1 -alpha/(2*k), n - 1)

IC.mean = cbind(inf = M - sqrt(diag(V)/n) * cfr.t, sup = M + sqrt(diag(V)/n) * cfr.t )
#             inf       sup
# books     387807.26 411531.69
# salaries  135950.74 157199.26
# electrics  38445.64  63100.62
# other     206044.14 224642.38
# total     795023.00 829698.73

qCinf <- qchisq(1 - alpha / (2*k), n-1)
qCsup <- qchisq(alpha / (2*k), n-1)

IC.sd    <- sqrt(cbind(inf=diag(V) * (n-1) / qCinf, sup=diag(V) * (n-1) / qCsup))
#             inf      sup
# books     16759.54 34437.08
# salaries  15010.50 30843.20
# electrics 17416.90 35787.81
# other     13138.27 26996.19
# total     24495.82 50333.40

t.test(df[,1]-df[,5]*0.5, conf.level = 0.95) # p-value = 0.2349 I can accept the null hypothesis, so the diff. is 0









