#Juliann Booth
#Data Mining HW 10
#due: 20 October 2015

germancredit <- read.csv("C:/Users/jbooth/Downloads/germancredit.csv")

#Useful functions:
confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}

is.factor(germancredit$Default)

germancredit$Default <- as.factor(germancredit$Default)

trainsamp <- sample(nrow(germancredit), round(0.7*nrow(germancredit),0))
train <- germancredit[trainsamp,]
test <- germancredit[-trainsamp,]

#(a) Decision Tree - predict default---------------------------------------------
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

tree <- rpart(Default~., data = train)
plot(tree)
fancyRpartPlot(tree)

predictions <- predict(tree, newdata = test)[,2] #need probabilities and just the probability of default, aka Default == 1
#area under the ROC curve
library(pROC)

tree_roc=roc(response=test$Default, predictor=predictions)

tree_roc
plot(tree_roc,ylab="True Positive Rate",xlab="False Positive Rate", main = "ROC curve for Decision Tree")

#(b) Weighted K-nearest Neighbors-------------------------------------------------

library(kknn)


fit.default <- train.kknn(Default~., data = train)

fit.default$best.parameters #optimal, k = 11

kknnmodel<-(kknn(Default~.,train,test,k=11,kernel="optimal"))
kknnphat <- kknnmodel$prob[,2]
plot(roc(test$Default, kknnphat), main = "ROC curve for Weighted k-nearest Neighbors")

#(c) Naive Bayes-------------------------------------------------------------------
library(mlbench)
library(e1071)


naivebayesmodel = naiveBayes(Default ~ ., data = train)

#phat for naiveBayes
phat=predict(naivebayesmodel,newdata=test,type="raw")[,2]



#ROC for Flight Data naiveBayes model
flightroc=roc(test$Default,phat)
flightroc
plot(flightroc, main = "ROC curve for Naive Bayes")

#------------------------------------------------------------------------------
#Problem 2-------------------------------------------------------------------
#-----------------------------------------------------------------------------

p0 = 0.2 #5000/25000

#Cost of decision tree------------------------------------------
newpreddecision <- predictions
newpreddecision[predictions>=p0]<- 1
newpreddecision[predictions<p0] <- 0

decisionmatrix <- confmatrix(test$Default, newpreddecision)$matrix
decisionmatrix

TdecP=decisionmatrix[1,1]
TNdec=decisionmatrix[2,2]
FPdec=decisionmatrix[2,1]
FNdec=decisionmatrix[1,2]

costdec = 20000*FPdec + 5000*FNdec
costdec

#Cost of weighted knn-------------------------------

newpredkknn <- kknnphat
newpredkknn[kknnphat >= p0] <- 1
newpredkknn[kknnphat < p0] <- 0
kknnmatrix <- confmatrix(test$Default, newpredkknn)$matrix

Tdk=kknnmatrix[1,1]
TNk=kknnmatrix[2,2]
FPk=kknnmatrix[2,1]
FNk=kknnmatrix[1,2]

costdec = 20000*FPk + 5000*FNk
costdec


#Cost of Naive Bayes---------------------------------
newprednb <- phat
newprednb[phat >=p0] <- 1
newprednb[phat < p0] <- 0

nbmatrix <- confmatrix(test$Default, newprednb)$matrix

TPnb=nbmatrix[1,1]
TNnb=nbmatrix[2,2]
FPnb=nbmatrix[2,1]
FNnb=nbmatrix[1,2]

costnb = 20000*FPnb + 5000*FNnb
costnb

#Using p0 = 0.5-----------------------------------

p0 = 0.5

#Cost of decision tree------------------------------------------
newpreddecision <- predictions
newpreddecision[predictions>=p0]<- 1
newpreddecision[predictions<p0] <- 0

decisionmatrix <- confmatrix(test$Default, newpreddecision)$matrix
decisionmatrix

TdecP=decisionmatrix[1,1]
TNdec=decisionmatrix[2,2]
FPdec=decisionmatrix[2,1]
FNdec=decisionmatrix[1,2]

costdec = 20000*FPdec + 5000*FNdec
costdec

#Cost of weighted knn-------------------------------

newpredkknn <- kknnphat
newpredkknn[kknnphat >= p0] <- 1
newpredkknn[kknnphat < p0] <- 0
kknnmatrix <- confmatrix(test$Default, newpredkknn)$matrix

Tdk=kknnmatrix[1,1]
TNk=kknnmatrix[2,2]
FPk=kknnmatrix[2,1]
FNk=kknnmatrix[1,2]

costk = 20000*FPk + 5000*FNk
costk


#Cost of Naive Bayes---------------------------------
newprednb <- phat
newprednb[phat >=p0] <- 1
newprednb[phat < p0] <- 0

nbmatrix <- confmatrix(test$Default, newprednb)$matrix

TPnb=nbmatrix[1,1]
TNnb=nbmatrix[2,2]
FPnb=nbmatrix[2,1]
FNnb=nbmatrix[1,2]

costnb = 20000*FPnb + 5000*FNnb
costnb

