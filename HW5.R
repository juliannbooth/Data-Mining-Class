#Data Mining HW 5
#Juliann Booth
#due September 22, 2015

#Problem 1---------------------------------------
splitdata = function(data, trainfrac){
  trainsample <- sample(nrow(data),round(trainfrac*nrow(data),0))
  traindata <- data[trainsample,]
  testdata <- data[-trainsample,]
  return(list(traindata = traindata, testdata = testdata))
}

splitlist <- splitdata(iris,0.7)
traindata <- splitlist$traindata
testdata <- splitlist$testdata

#Problem 3------------------------------------------
#a
wdbc <- read.csv("~/Data Mining/wdbc.data", header=FALSE)
newwdbc <- wdbc[,-1]
#save the file for future homeworks
write.csv(newwdbc, file = "newwdbc.csv", row.names = FALSE)
#b
splitwdbc<-splitdata(newwdbc, 0.7)
train <- splitwdbc$traindata
test <- splitwdbc$testdata
#c
dim(newwdbc)[1] == (dim(train) + dim(test))[1]
colSums(newwdbc[,2:3]) == colSums(train[,2:3])+colSums(test[,2:3])

#Problem 4-----------------------------------------
#a
is.factor(train[,1])
is.factor(test[,1])

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

tree1 <- rpart(V2~.,data = train)
plot(tree1)
fancyRpartPlot(tree1)

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}

#Confusion matrix, accuracy, and error rate for training data. Should see higher accuracy on training
confmatrix(train$V2,
           predict(tree1,newdata=train,type="class"))


#Confusion matrix, accuracy, and error rate for test data.
confmatrix(test$V2,
           predict(tree1,newdata=test,type="class"))

#b----------------------------------------------------
library(party)
tree2 <- ctree(V2~., data = train)
plot(tree2)
plot(tree2,type='simple')



#ctree confusion matrix.
#Note that we don't need type = "class".
predClass=predict(tree2,newdata=train)
confusionmatrix=table(train$V2,predClass)
confusionmatrix
accuracy <- sum(diag(confusionmatrix))/sum(confusionmatrix)
error <- 1-accuracy

predClass1=predict(tree2,newdata=test)
confusionmatrix1=table(test$V2,predClass1)
confusionmatrix1
accuracy <- sum(diag(confusionmatrix1))/sum(confusionmatrix1)
error <- 1-accuracy

#d-------------------------------------------------

accvectortree1 <- (test$V2 == predict(tree1, newdata = test, type = 'class'))
table(accvectortree1)

#don't do type = "class" because ctree automatically using class
accvectortree2 <- (test$V2==predict(tree2, newdata=test))
table(accvectortree2)

mcnemartable=table(accvectortree1,accvectortree2)
library(exact2x2)
mcnemar.exact(mcnemartable) #p-value = 1


#Problem 5--------------------------------------------------
#a. 10-fold cross validation -------------------------------
library(cvTools)
createfolds=function(n,K){
  reps=ceiling(n/K)
  folds=sample(rep(1:K,reps))
  return(folds[1:n])
}

set.seed(5364)
folds=createfolds(nrow(newwdbc),10)
table(folds)

accvector=1:10

for(k in 1:10){
  temptest=newwdbc[folds==k,]
  temptrain=newwdbc[folds!=k,]
  
  temptree=rpart(V2~.,data=temptrain)
  accvector[k]=confmatrix(temptest$V2,
                          predict(tree1,newdata=temptest,type="class"))$accuracy
}

mean(accvector) #0.9542607

#b. 20-fold cross validation-------------------------------

folds<- createfolds(nrow(newwdbc), 20)
table(folds)
accvector20 <- 1
for(k in 1:20){
  temptest=newwdbc[folds==k,]
  temptrain=newwdbc[folds!=k,]
  
  temptree=rpart(V2~.,data=temptrain)
  accvector20[k]=confmatrix(temptest$V2,
                          predict(tree1,newdata=temptest,type="class"))$accuracy
}
mean(accvector20) #0.9541119

#c. Leave-one-out cross validation-------------------------
#equivalent to k-fold cv with k = n where n is number of records in original data
folds<- createfolds(nrow(newwdbc), nrow(newwdbc))
table(folds)
accvectorall <- 1
for(k in 1:nrow(newwdbc)){
  temptest=newwdbc[folds==k,]
  temptrain=newwdbc[folds!=k,]
  
  temptree=rpart(V2~.,data=temptrain)
  accvectorall[k]=confmatrix(temptest$V2,
                            predict(tree1,newdata=temptest,type="class"))$accuracy
}
mean(accvectorall) #0.9543058

#d. Delete-d cross-validation with d = 20 and m = 100--------------------------
#repeat the following 100 times: randomly select 20 records. Use those 20 records
#as a test set. Use all other records as a training set. Compute accuracy. After the
#100 times, average all accuracies.

deletedacc <- 1
m <- 100
d <- 20
for (i in 1:m){
  randomsamp <- sample(nrow(newwdbc),d)
  randomtest <- newwdbc[randomsamp,]
  randomtrain <- newwdbc[-randomsamp,]
  temptree <- rpart(V2~., data = randomtrain)
  deletedacc[i] <- confmatrix(randomtest$V2, predict(tree1, newdata=randomtest, type="class"))$accuracy
}

mean(deletedacc) #0.9545

#e The bootstrap with b=100-----------------------------------------------
#Repeat the following 100 times: randomly select n records with replacement 
#(n is the number of records in the data). Use the n records as a training set. Use all
#other records as a test set. Compute accuracy. Afterwards, average all accuracies.

bootstrapacc <- 1
b <- 100

index=sample(nrow(newwdbc),replace=TRUE)
index[sum(index>1)]
sum(index==562)

for (i in 1:b){
  bootsamp <- sample(nrow(newwdbc), replace = TRUE)
  boottrain <- newwdbc[bootsamp,]
  boottest <- newwdbc[-bootsamp,]
  bootstrapacc[i] <- confmatrix(boottest$V2, predict(tree1, newdata=boottest, type="class"))$accuracy
}
mean(bootstrapacc) #0.9532662
