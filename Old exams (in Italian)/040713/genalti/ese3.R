rm(list = ls())
df <- read.table('dongting.txt')
head(df)
toty <- 97 +276
priors = c(276/toty, 97/toty)
mins <- c(min(df[which(df$period == 'ming'),1]), min(df[which(df$period == 'yuan'),1]))
maxs <- c(max(df[which(df$period == 'ming'),1]), max(df[which(df$period == 'yuan'),1]))

doubt <- df[which(df$length >= 12.02 & df$length <= 15.8),]
N <- dim(doubt)[1]
ming <- length(doubt[which(doubt$period == 'ming'),1])
yuan <- length(doubt[which(doubt$period == 'yuan'),1])
evid <- c(ming/N, yuan/N)
evid
