rm(list = ls())
df <- read.table("wellness.txt")
head(df)
set.seed(321)

stat = c()
for (i in 1:5000) {
  er = c()
  for (j in 1:14) {
    er = c(er, df[j,sample(1:11)[1]])
  }
  stat = c(stat, mean(er))
  
}

hist(stat)

for (i in 1:11) {
  abline(v=mean(df[,i]))
  print(paste(i, sum(mean(df[,i]) > stat)/5000))
}