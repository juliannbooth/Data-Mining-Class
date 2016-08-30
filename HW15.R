#Juliann Booth
#Data Mining HW 15
#November 11, 2015

#Problem 1
newwdbc <- read.csv("~/Data Mining/newwdbc.csv")
trainsamp <- trainsample(newwdbc, 0.7)
train <- newwdbc[trainsamp,]
test <- newwdbc[-trainsamp,]

#bagging
library(adabag)

baggingmodel=bagging(V2~.,data=train)
bagpred=predict(baggingmodel,newdata=test,type="class")
bagpred$confusion
1-bagpred$error #0.9532
names(bagpred)



#boosting

boostingmodel=boosting(V2~.,data=train)
boostpred=predict(boostingmodel,newdata=test,type="class")
boostpred$confusion
1-boostpred$error #0.971



#random forests
library(randomForest)
forestmodel=randomForest(V2~.,data=train)
forestpred=predict(forestmodel,newdata=test)
confmatrix(test$V2,forestpred) #accuracy = 0.959
