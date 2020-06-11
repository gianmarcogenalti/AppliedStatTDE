###--------------------###
### LAB 9 (28/04/2020) ###
###--------------------###

### TOPIC:
### Support Vector Machines

library(e1071)
help(svm)

#################################################################################################
### Linear case

# Generate the data
set.seed (123)
x <- matrix (rnorm (20*2) , ncol =2)
y <- c(rep (-1,10) , rep (1 ,10) )
x[y==1,] <- x[y==1,] + 1

# The classes are not separable
x11()
plot(x, col =ifelse(y==1, 'light blue', 'salmon'), 
     pch=19, xlab='x1', ylab='x2', asp=1)

# Fit the Support Vector Classifier (kernel = "linear")
# given a cost C
dat <- data.frame(x=x, y=as.factor (y))
svmfit <- svm(y~., data=dat , kernel ='linear', cost =10, scale =FALSE )
summary(svmfit)

x11()
par(mfrow=c(1,2))
plot(svmfit , dat, col =c('salmon', 'light blue'), pch=19, asp=1)

# support vectors are indicated with crosses
# they are:
svmfit$index

###
n.g <- 100

xgrid <- expand.grid(x.1=seq(from=range(dat$x.1)[1],to=range(dat$x.1)[2],length=n.g),
                  x.2=seq(from=range(dat$x.2)[1],to=range(dat$x.2)[2],length=n.g))
ygrid <- predict(svmfit,xgrid)
x11()
plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.2)
points(x,col=c("red","blue")[as.numeric(dat$y)],pch=19)
points(x[svmfit$index,],pch=5,cex=2)

x11()
plot(x,col=c("red","blue")[as.numeric(dat$y)],pch=19)
contour(seq(from=range(dat$x.1)[1],to=range(dat$x.1)[2],length=n.g),
        seq(from=range(dat$x.2)[1],to=range(dat$x.2)[2],length=n.g),
        matrix(as.numeric(ygrid),n.g,n.g),level=1.5,add=TRUE,
        drawlabels=F)

###

# If we try to change the cost parameter we get more support points
# (higher bias, lower variance)
svmfit <- svm(y~., data=dat , kernel ='linear', cost =0.1, scale =FALSE )
plot(svmfit , dat, col =c('salmon', 'light blue'), pch=19, asp=1)

# To set the parameter C we can use the function tune(),
# which is based on cross-validation (10-fold)
set.seed (1)
tune.out <- tune(svm,y~.,data=dat ,kernel = 'linear',
              ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))
summary(tune.out)

# Extract the best model from the result of tune
bestmod <- tune.out$best.model
summary(bestmod)

plot(bestmod , dat, col =c('salmon', 'light blue'), pch=19, asp=1)

# Prediction for a new observation (command predict())
xtest <- matrix(rnorm (20*2) , ncol =2)
ytest <- sample(c(-1,1) , 20, rep=TRUE)
xtest[ytest ==1 ,] <- xtest[ytest ==1,] + 1
testdat <- data.frame(x=xtest , y=as.factor (ytest))

plot(xtest, col =ifelse(ytest==1, 'light blue', 'salmon'), 
     pch=19, xlab='x1', ylab='x2', asp=1)

ypred <- predict(bestmod,testdat)
table(true.label=testdat$y, assigned.label =ypred )

# If the classes are separable, setting a high value for the cost function
# leads to the maximal margin classifier (i.e., it returns the classification
# provided by the best separating hyperplane)

species.name <- factor(iris$Species, labels=c('setosa','versicolor','virginica'))
set.seed(1)
iris2 <- iris[,1:2] + cbind(rnorm(150, sd=0.025))    # jittering

# setosa VS versicolor+virginica
y <- rep(0,150)
y[which(species.name=='setosa')] <- 1

x11()
plot(iris2[,1], iris2[,2], xlab='Sepal.Length', ylab='Sepal.Width', pch=20, col=as.character(y+1))

dat <- data.frame(x=iris2[,c(2,1)], y=as.factor (y))
svmfit <- svm(y~., data=dat , kernel ='linear', cost =100, scale =FALSE )
summary(svmfit)

x11()
par(mfrow=c(1,2))
plot(svmfit , dat, col =c('salmon', 'light blue'), pch=19)


dat <- data.frame(x=iris2[,c(2,1)], y=as.factor (y))
svmfit <- svm(y~., data=dat , kernel ='linear', cost =1, scale =FALSE )
summary(svmfit)

x11()
par(mfrow=c(1,2))
plot(svmfit , dat, col =c('salmon', 'light blue'), pch=19)


#################################################################################################
### Non-linear case

# Generate the data
set.seed (1)
x <- matrix (rnorm (200*2) , ncol =2)
x[1:100 ,] <- x[1:100 ,]+2
x[101:150 ,] <- x[101:150 ,] -2
y <- c(rep (1 ,150) ,rep (2 ,50) )
dat <- data.frame(x=x,y=as.factor (y))

# The classes are not separable
x11()
plot(x, col =ifelse(y==1, 'salmon', 'light blue'), pch=19, xlab='x1', ylab='x2', asp=1)

# Randomly split in train and test
train <- sample (200 ,100)

# Fit a Support Vector Machine (kernel = "radial") given a cost C
svmfit <- svm(y~., data=dat [train ,], kernel ='radial', gamma =1, cost =1)
summary(svmfit)

# Plot the SVM
x11()
plot(svmfit , dat, col =c('salmon', 'light blue'), pch=19, asp=1)

# Misclassification error on the training set
table(true=dat[train ,"y"], pred=predict (svmfit ,
                                          newdata =dat[train ,]))
(3+4)/100

# Misclassification error on the test set
table(true=dat[-train ,"y"], pred=predict (svmfit ,
                                           newdata =dat[-train ,]))
(3+10)/100

# Increasing the cost decreases the errors on the training set,
# at the expense of a more irregular boundary
svmfit <- svm(y~., data=dat [train ,], kernel ='radial',gamma =1,cost=1e5)

x11()
plot(svmfit , dat, col =c('salmon', 'light blue'), pch=19, asp=1)

# Misclassification error on the training set
table(true=dat[train ,"y"], pred=predict (svmfit ,
                                           newdata =dat[train ,]))
0

# Misclassification error on the test set
table(true=dat[-train ,"y"], pred=predict (svmfit ,
                                           newdata =dat[-train ,]))
(5+20)/100

# Set parameters via CV:
set.seed (1)
tune.out <- tune(svm , y~., data=dat[train ,], kernel ='radial',
              ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),
                           gamma=c(0.5,1,2,3,4) ))
summary(tune.out)

# Misclassification error with best model on the training set
table(true=dat[train ,"y"], pred=predict (tune.out$best.model ,
                                           newdata =dat[train ,]))
(2+4)/100

# Misclassification error with best model on the test set
table(true=dat[-train ,"y"], pred=predict (tune.out$best.model ,
                                           newdata =dat[-train ,]))
(2+10)/100



#################################################################################################
### Application to Gene Expression Data (multiclass classification)
library (ISLR)

help(Khan)

is(Khan)
names(Khan)

dim(Khan$xtrain)
dim(Khan$xtest)
length (Khan$ytrain)
length (Khan$ytest)

table(Khan$ytrain)
table(Khan$ytest)

dat <- data.frame(x=Khan$xtrain , y=as.factor(Khan$ytrain))
out <- svm(y~., data=dat , kernel ="linear",cost =10)
summary (out)

table(out$fitted , dat$y)

dat.te <- data.frame(x=Khan$xtest , y=as.factor (Khan$ytest ))
pred.te <- predict (out , newdata =dat.te)
table(pred.te , dat.te$y)
