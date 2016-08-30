#Juliann Booth
#Data Mining HW 4
#due: 15 September 2015
#----------------------------------------------------------------------
#Problem 1
#------------------------------------------------------------------
set.seed(5364)

blackx <- runif(1800, 0, 20)
redx1 <- rnorm(400,10,2)
redx2 <- rnorm(400,5,2)
redx3 <- rnorm(400,15,2)
x <- c(blackx, redx1, redx2, redx3)

blacky <- runif(1800,0,20)
redy1 <- rnorm(400, 5, 2)
redy2 <- rnorm(400, 15, 2)
redy3 <- rnorm(400, 15, 2)
y <- c(blacky, redy1, redy2, redy3)

class <- c(rep(0,1800), rep(1, 1200))
class <- as.factor(class)
exdata <- data.frame(x,y,class, row.names = NULL)

#------------------------------------------------------------------------
#Problem 2
#a---------------------------------------------------------------------
plot(exdata$x, exdata$y, col = c('black','red')[exdata$class])
#b--------------------------------------------------------------------
train1 <- sample(nrow(exdata), round(0.3*nrow(exdata),0))
train <- exdata[train1,]
test <- exdata[-train1,]
#c-------------------------------------------------------------------------
library(rpart)
library(rattle)
library(RColorBrewer)
library(rpart.plot)
extree <- rpart(train$class~., data= train)

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}

#training error
confmatrix(train$class, predict(extree, newdata = train, 
                               type = "class"))$error

#testing error
confmatrix(test$class, predict(extree, newdata = test, 
                               type = "class"))$error
#plot tree
plot(extree)

#d------------------------------------------------------------------------

 maxdepth1 <- c(rep(1,3))
 maxdepth2 <- c(rep(1,3))
 maxdepth3 <- c(rep(1,3))
 maxdepth4 <- c(rep(1,3))
 maxdepth5 <- c(rep(1,3))
 maxdepth6 <- c(rep(1,3))
 store <- data.frame(maxdepth1,maxdepth2,maxdepth3,maxdepth4,
                     maxdepth5,maxdepth6, row.names = 
                       c("trainerror", "testerror","nnodes"))

 for (n in 1:6){
  extree <- rpart(train$class~., data= train, maxdepth = n)
  trainerror <- confmatrix(train$class, predict(extree, newdata = train, 
                                                type = "class"))$error
  testerror <- confmatrix(test$class, predict(extree, newdata = test, 
                                              type = "class"))$error
  nnodes <- dim(extree$frame)[1]
  store[1,n] <-  trainerror
  store[2,n] <- testerror
  store[3,n] <- nnodes
 }
store
#e-----------------------------------------------------------------------------
c <- c(seq(-2,-3,-0.1))
cp <- c(rep(1,length(c)))
for (i in 1:length(c)){
  cp[i] <- 10^(c[i])
}
#Give names for matrix
rownames <- c("trainerror", "testerror","nnodes")
colnames <- c("cp = 10^-2", "10^-2.1", "10^-2.2", "10^-2.3", 
              "10^-2.4", "10^-2.5", "10^-2.6", 
              "10^-2.7", "10^-2.8","10^-2.9","10^-3.0")
names <- list(rownames, colnames)

store2 <- matrix(nrow = 3, ncol = length(cp), dimnames = names)

for (i in 1:length(cp)){
  extree <- rpart(train$class~., data = train, minsplit = 1, cp = cp[i])
  trainerror <- round(confmatrix(train$class, 
                                 predict(extree, newdata = train,
                                         type = "class"))$error, 4)
  testerror <- round(confmatrix(test$class, 
                                predict(extree, newdata = test, 
                                        type = "class"))$error, 4)
  nnodes <- dim(extree$frame)[1]
  store2[1,i] <-  trainerror
  store2[2,i] <- testerror
  store2[3,i] <- nnodes
}
store2

#f------------------------------------------------------------------
plot(store2[3,], store2[1,], type ="l", xlab = "Number of Nodes",
     ylab = "Error Rate", ylim = c(0,0.40))
lines(store2[3,],store2[2,], col = "blue")
legend(275,0.2, c("Testing", "Training"),lty=c(1,1), 
       lwd=c(2.5,2.5), col = c("blue","black"))

#g-----------------------------------------------------------------
extree2 <- rpart(train$class~., data = train, minsplit = 1, cp = 10^(-3))
plot(extree2)
#h-----------------------------------------------------------------


#Exact binomial test.
extree <- rpart(train$class~., data= train)
confmatrix(test$class,predict(extree,newdata=test,type='class'))
sum(confmatrix(test$class,predict(extree,newdata=test,type='class'))$matrix)
#So we know our dimension is 2100
#Compute the exact binomial confidence interval
binom.test(1397,2100) #1397 is number of correctly classified objects (922+475), 
#2100 are total
#p-value is < 2.2e-16


#Building tree 2
extree2 <- rpart(train$class~., data = train, control=rpart.control(minsplit=1,cp=10^(-3)))

plot(extree2)
confmatrix(train$class,predict(extree2,newdata=train,type='class'))
confmatrix(test$class,predict(extree2,newdata=test,type='class'))

#How to precisely see if two trees differ:
#Building accuracy vectors
accvector1=(test$class==predict(extree,newdata=test,type='class'))
table(accvector1)
table(accvector1)/2100

accvector2=(test$class==predict(extree2,newdata=test,type='class'))
table(accvector2)
table(accvector2)/2100

#McNemar Table
mcnemartable=table(accvector1,accvector2)
mcnemartable



#Built-in Function
mcnemar.test(mcnemartable)

#Exact McNemar Test
library(exact2x2)
mcnemar.exact(mcnemartable)
#-----------------------------------------------------------------
#Problem 3
#-----------------------------------------------------------------
zcritical = function(alpha, numtails){
  if (numtails == 1){
    zalpha <- qnorm(1-alpha, 0, 1)
    zalpha}
  else{
    zalphahalf <- qnorm(1-(alpha/2),0,1)
    zalphahalf}
}
#-----------------------------------------------------------------
#Problem 4
#-----------------------------------------------------------------
accuracyconfint = function(accuracy, n, alpha){
  z <- zcritical(alpha, 2)
  upper <- accuracy + z*(sqrt(accuracy*(1-accuracy)/n))
  lower <- accuracy - z*(sqrt(accuracy*(1-accuracy)/n))
  confint <- c(lower, upper)
  print(confint)
}

accuracyconfint(0.76889, nrow(train), 0.05) #(0.74135, 0.79643) training
accuracyconfint(0.71667, nrow(train), 0.05) #(0.68723, 0.74612) testing
#----------------------------------------------------------------
#Problem 5
#---------------------------------------------------------------
rsample <- sample(nrow(kyphosis), round(0.7*nrow(kyphosis),0))
trainK <- kyphosis[rsample,]
testK <- kyphosis[-rsample,]
#a--------------------------------------------------------------
ktree <- rpart(trainK$Kyphosis~.,data = trainK)
plot(ktree)

confmatrix(trainK$Kyphosis, predict(ktree, newdata = trainK,
                                    type = "class"))
confmatrix(testK$Kyphosis, predict(ktree, newdata = testK,
                                   type = "class"))

#b------------------------------------------------------------
binom.test(round(0.8246*nrow(trainK),0),nrow(trainK))
#CI for training: (0.7009, 0.91253)

binom.test(round(0.8333*nrow(testK),0),nrow(testK))
#CI for testing is (0.626,0.953)