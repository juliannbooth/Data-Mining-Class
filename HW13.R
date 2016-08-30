#Juliann Booth
#Data Mining
#HW 13
#due: November 3, 2015

newwdbc <- read.csv("~/Data Mining/newwdbc.csv")

#Problem 1---------------------------------------

trainsamp <- sample(nrow(newwdbc), round(0.7*nrow(newwdbc),0))
train <- newwdbc[trainsamp,]
test <- newwdbc[-trainsamp,]

#(a)--------------------------------------------
library(nnet)
model1=nnet(V2~.,data=train,size=1,linout=FALSE)  
#linout=FALSE for categorical output, size is number of neurons
plot(model1)

predtest=predict(model1,newdata=test, type="class")
confmatrix(test$V2,predtest)

V2hat=predict(model1,newdata=test, type = "raw")

library(pROC)
plot(roc(response=test$V2,predictor=V2hat[,1]))

#(b)------------------------------------------
library(cvTools)
createfolds=function(n,K){
  reps=ceiling(n/K)
  folds=sample(rep(1:K,reps))
  return(folds[1:n])
}

set.seed(13)
folds=createfolds(nrow(newwdbc),10)
table(folds)

sizeacc=1
for (j in 1:13){
  accvector10fold=1
  for(k in 1:10){
    temptest=newwdbc[folds==k,]
    temptrain=newwdbc[folds!=k,]
    model <- nnet(V2~., data=temptrain, size = j, linout = FALSE) 
    preddiagnosis=predict(model,newdata=temptest, type="class")
    accvector10fold[k]=confmatrix(newwdbc$V2[folds==k],preddiagnosis)$accuracy
  }
  sizeacc[j] = mean(accvector10fold)
}

which.max(sizeacc)
plot(sizeacc, xlab = "size", ylab = "accuracy", 
     main = "Accuracy Using 10-fold CV")


#(c)------------------------------------------------
model2=nnet(V2~.,data=train,size=which.max(sizeacc),linout=FALSE)  
#linout=FALSE for categorical output

predtest1=predict(model2,newdata=test, type="class")
confmatrix(test$V2,predtest1)

V2hat1=predict(model2,newdata=test, type = "raw")

plot(roc(response=test$V2,predictor=V2hat1[,1]), main="ROC curve, size = 8")

#------------------------------------------------
#Problem 2--------------------------------------------------
#(a)------------------------------------


set.seed(5364)
x=runif(100,0,2*pi)
y=sin(x)
traindata=data.frame(x,y)

model3=nnet(y~.,data=traindata,size=1,linout=TRUE)
plot(model3)

#(b)------------------------------------------------

folds=createfolds(nrow(traindata),10)
table(folds)

sizeacc=1
for (j in 1:13){
  accvector10fold=1
  for(k in 1:10){
    temptest=traindata[folds==k,]
    temptrain=traindata[folds!=k,]
    model <- nnet(y~., data=temptrain, size = j, linout = TRUE) 
    predy=predict(model,newdata=temptest)
    accvector10fold[k]=confmatrix(traindata$y[folds==k],predy)$accuracy
  }
  sizeacc[j] = mean(accvector10fold)
}

which.max(sizeacc)
plot(sizeacc, xlab = "size", ylab = "accuracy", 
     main = "Accuracy of y=sin(x) Using 10-fold CV")


#(c)--------------------------------------------------
model4=nnet(y~.,data=traindata,size=which.max(sizeacc),linout=TRUE)  
#linout=TRUE for quantitative output


predsin=predict(model4,newdata=traindata)
confmatrix(traindata$y,predsin)

plot(y~x, main = "Actual (black) versus Predicted (red)", ylab = "sin(x)")
points(predsin~x, col = 'red')

cor(predsin, y)




