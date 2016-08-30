#Juliann Booth
#Data Mining HW 6
#September 29, 2015

newwdbc <- read.csv("~/Data Mining/newwdbc.csv")
#-----------------------------------------------------------------------
#Problem 1. (a) ------------------------------------------------------------
#-----------------------------------------------------------------------
#Get rid of the classification column
x <- newwdbc[,-1]
xbar=apply(x,2,mean)
xbarMatrix=cbind(rep(1,nrow(newwdbc)))%*%xbar
s=apply(x,2,sd)
sMatrix=cbind(rep(1,nrow(newwdbc)))%*%s
#the standardized data
z=(x-xbarMatrix)/sMatrix

#verify that the means are 0 and sd's are 1:
apply(z,2,mean) #approx. 0
apply(z,2,sd) #equal 1

#(b) Split the data into 70% training and 30% testing-----------------
set.seed(13)
trainsamp <- sample(nrow(z),round(0.7*nrow(z),0))
train <- z[trainsamp,]
test <- z[-trainsamp,]

#(c) Calculate test error for predicting breast cancer diagnosis-------
#using knn with k = 3
#Find a 95% confidence interval for the error rate

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}
library(class)

#The classifications:
diagnosis <- newwdbc$V2
#use the index trainsamp, not the dataset train
preddiagnosis <- knn(train, test, cl=diagnosis[trainsamp],k=3)
length(preddiagnosis) #171
confmatrix(diagnosis[-trainsamp],preddiagnosis)

#95% confidence interval
#from HW4:
zcritical = function(alpha, numtails){
  if (numtails == 1){
    zalpha <- qnorm(1-alpha, 0, 1)
    zalpha}
  else{
    zalphahalf <- qnorm(1-(alpha/2),0,1)
    zalphahalf}}

accuracyconfint = function(accuracy, n, alpha){
  z <- zcritical(alpha, 2)
  upper <- accuracy + z*(sqrt(accuracy*(1-accuracy)/n))
  lower <- accuracy - z*(sqrt(accuracy*(1-accuracy)/n))
  confint <- c(lower, upper)
  print(confint)
}

accuracyconfint(0.03508772,nrow(test) ,0.05) #(0.007509, 0.06266)

#(d)----------------------------------------------------------------------
#Use rpart
library(rpart)
library(rattle)
library(RColorBrewer)
library(rpart.plot)
original_train <- newwdbc[trainsamp,]
original_test <- newwdbc[-trainsamp,]
tree <- rpart(original_train$V2~.,data = original_train)
 
plot(tree)
fancyRpartPlot(tree)

confmatrix(original_test$V2, predict(tree, newdata = original_test, type = "class"))
#error = 0.07602

#test if there is a significant difference between rpart and knn
#seeing when the parameters of the confmatrices are equal
accvectortree <- (original_test$V2 == predict(tree, newdata = original_test, type = "class"))
accvectorknn <- (diagnosis[-trainsamp] == preddiagnosis)

mcnemartable <- table(accvectortree, accvectorknn)
library(exact2x2)
mcnemar.exact(mcnemartable)
#----------------------------------------------------------------------------
#Problem 2------------------------------------------------------------------------
#--------------------------------------------------------------------------
#(a)Use knn.cv to estimate the error rate when k=3-------------------------------
knncv3 <- knn.cv(train=z, cl=newwdbc$V2, k=3)
confmatrix(newwdbc$V2, knncv3) #0.035149

#(b) find k0 that minimizes error rate--------------------------------
set.seed(13)
accvectorknncv <- 1
for(k in 1:10){
  predy=knn.cv(train=z,cl=newwdbc$V2,k=k)
  accvectorknncv[k]=confmatrix(newwdbc$V2,predy)$error
}
k0 <- which.min(accvectorknncv) #k=4

#(c) use 10-fold cross validation with k=k0--------------------------
library(cvTools)
createfolds=function(n,K){
  reps=ceiling(n/K)
  folds=sample(rep(1:K,reps))
  return(folds[1:n])
}

set.seed(13)
folds=createfolds(nrow(z),10)
table(folds)

accvector10fold=1

for(k in 1:10){
  temptest=z[folds==k,]
  temptrain=z[folds!=k,]
  preddiagnosis <- knn(temptrain, temptest, cl=diagnosis[folds!=k], k = k0) 
  accvector10fold[k]=confmatrix(diagnosis[folds==k],preddiagnosis)$error
}

mean(accvector10fold) 

#(d) use bootstrap with b = 100------------------------------------------------


#Repeat the following 100 times: randomly select n records with replacement 
#(n is the number of records in the data). Use the n records as a training set. Use all
#other records as a test set. Compute error rate. Afterwards, average all error rates.

bootstrapacc <- 1
b <- 100


for (i in 1:b){
  bootsamp <- sample(nrow(z), replace = TRUE)
  boottrain <- z[bootsamp,]
  boottest <- z[-bootsamp,]
  preddiagnosis <- knn(boottrain, boottest, cl=diagnosis[bootsamp], k = k0)
  bootstrapacc[i] <- confmatrix(diagnosis[-bootsamp],preddiagnosis)$error
}

mean(bootstrapacc) #0.044779

#---------------------------------------------------------------------
#Problem 3 ---------(Bonus)-----------------------------------------
#------------------------------------------------------------------

#Assumptions: 1. k is odd and there are no ties
#             2. D is already standardized! (because knn does as well)

#Distance function -------------------------------------------------
euclidean=function(x1,x2){
  return(sqrt((x1-x2)%*%t(x1-x2)))
}
#added a transpose on the second because we are going by row and we want to end up with
#a 1x1 answer.
distancematrix = function(Ztrain, Ztest){
  D <- matrix(nrow = nrow(Ztrain), ncol = nrow(Ztest))
  for (i in 1:nrow(Ztrain)){
    x <- as.matrix(Ztrain[i,])
    for (j in 1:nrow(Ztest)){
     y <- as.matrix(Ztest[j,])
     D[i,j] <- euclidean(x,y)
   }
  }
  return(D)
}

D <- distancematrix(train, test)
#dimension of D is indeed 398 x 171 :)
#knn function ----------------------------------------------------------
#now try to find the k smallest values in D
#ZtrainClasses is the list of class values for the Ztrain (diagnosis[trainsamp])
predClass = function(Distmatrix, ZtrainClasses, k){
  predClass <- 1
  for (i in 1:ncol(Distmatrix)){
    vector <- D[,i]
    nearestneighbors <- 1
    classlabels <- 1
    for (j in 1:k){
      nearestneighbors[j] <- which.min(vector)
      classlabels[j] <- ZtrainClasses[nearestneighbors[j]]
      vector <- vector[-nearestneighbors[j]]
    }
    a <- table(classlabels)
    #account for if they are all the same predictions (the else being dim == 1):
    #note: R changed B and M to 1 and 2 respectively
    if(dim(a)==2){
      if (a[names(a)==1] > a[names(a)==2]){
        predClass[i] <- "B"
      }
      else{predClass[i] <- "M"}
    }
    else{
      if(names(a)==1){predClass[i] <- "B"}
      else{predClass[i] <- "M"}
    }
  }
  predClass <- as.factor(predClass)
  return(predClass)
}

preds <- predClass(D, diagnosis[trainsamp],5)
#check that length of preds is 171:
length(preds)

#check accuracy: ----------------------------------------------------------
confmatrix(diagnosis[-trainsamp], preds)

#compare to knn: -----------------------------------------------------
preddiagnosis <- knn(train, test, cl=diagnosis[trainsamp],k=3)
length(preddiagnosis) #171
confmatrix(diagnosis[-trainsamp],preddiagnosis) 
