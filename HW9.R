#Juliann Booth
#Data Mining HW 9
#due: October 13, 2015

germancredit <- read.csv("C:/Users/jbooth/Downloads/germancredit.csv")

#Useful functions:
confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}

#Problem 3
is.factor(germancredit[,1])
germancredit[,1] <- as.factor(germancredit[,1])

trainsamp <- sample(nrow(germancredit), round(0.7*nrow(germancredit),0))
train <- germancredit[trainsamp,]
test <- germancredit[-trainsamp,]

#(a)----------------------------------------------------------------------
library(e1071)
model1=naiveBayes(Default ~ ., data = train)
predDef=predict(model1,newdata=test)

model1matrix <- confmatrix(germancredit$Default[-trainsamp],predDef)$matrix
model1acc <- confmatrix(germancredit$Default[-trainsamp],predDef)$accuracy

TP=model1matrix[1,1] #True Positive
TN=model1matrix[2,2] 
FP=model1matrix[2,1] #False Positive
FN=model1matrix[1,2]
#sensitivity
sens1 <- TP/(TP+FN)

#precision
prec <- TP/(TP+FP)

#F1
F1 <- 2*TP/(2*TP+FP+FN)

#(b)------------------------------------------------------------------
#Find p0 that optimizes the F1 measure
phat <- predict(model1, newdata = test, type = "raw")[,1] #default == 0

trainphat <- predict(model1, newdata = train, type = "raw")[,1]

trainprec=1:100
trainrecall=1:100
trainF1=1:100

range(trainphat)

p0vect=10^(-5+(1:100)/100*5)

for(i in 1:100){
  p0 <- p0vect[i]
  trainpredDef <- (trainphat>=p0)*1
  
  TP <- sum((trainpredDef==1)&(germancredit$Default[trainsamp]=="0"))
  FP <- sum((trainpredDef==1)&(germancredit$Default[trainsamp]!="0"))
  FN <- sum((trainpredDef!=1)&(germancredit$Default[trainsamp]=="0"))
  
  trainprec[i] <- TP/(TP+FP)
  trainrecall[i] <- TP/(TP+FN)
  trainF1[i] <- 2*TP/(2*TP+FP+FN)
  
}

plot(p0vect,trainrecall,type='l',xlab="Probability Threshold",ylab="") #the higher the recall, the lower the sensitivity
lines(p0vect,trainprec,col="blue") #precision gets higher and higher and higher (blue)
lines(p0vect,trainF1,col='red') #F1 is a compromise between the two
#Our plot was very bad. We need to be on a logarithmic scale:

plot(log(p0vect,base=10),trainrecall,type='l',xlab="log10(Probability Threshold)",ylab="")
lines(log(p0vect,base=10),trainprec,col="blue")
lines(log(p0vect,base=10),trainF1,col='red')

which.max(trainF1) #95th position of the vector is the max
trainF1[which.max(trainF1)] #0.844 is the F1
trainprec[which.max(trainF1)] #0.84 is the precision
trainrecall[which.max(trainF1)] #0.8484 is the recall

optimalp0 <- p0vect[which.max(trainF1)] #0.5623

#(c)------------------------------------------
testphat=predict(model1,newdata=test,type="raw")[,1]
newpredDef <- testphat
newpredDef[testphat>=optimalp0]="0" 
newpredDef[testphat<optimalp0]="1"

cmatrix=confmatrix(germancredit$Default[-trainsamp],newpredDef)$matrix

TP=cmatrix[1,1]
TN=cmatrix[2,2]
FP=cmatrix[2,1]
FN=cmatrix[1,2]

#accuracy
acc=(TP+TN)/(TP+TN+FP+FN)
acc #0.757

#sensitivity
TPR=TP/(TP+FN) #0.829

#specificity
TNR=TN/(TN+FP) #0.6

#precision
p=TP/(TP+FP) #0.817

#F1
r=TP/(TP+FN)
F1=2*r*p/(r+p) #0.823

