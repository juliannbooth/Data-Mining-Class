#Juliann Booth
#Data Mining HW 8
#due October 8, 2015

#------------------------------------------
#Problem 1----------------------------------
#------------------------------------------
library(mlbench)
data(HouseVotes84)
#(a)-------------------------------------
tab <- table(HouseVotes84$Class)
tabprob <- tab/sum(tab) #0.6138
#(b)------------------------------------
#Given that a representative is a Republican, what is the probability that he or she voted
#for water project cost sharing? (V3)

library(e1071)
#another way to find the answer for (a) from class:
model = naiveBayes(Class ~ ., data = HouseVotes84)
model
model$apriori #gives count of republican and democrat
table(HouseVotes84$Class) #another way to find the count
model$apriori/(sum(model$apriori)) #This gives the PERCENTAGE...the PROBABILITY of Y...the PRIOR DISTRIBUTION


#Conditional probabilities
(model$tables)$V2 #just the conditional distribution for V2

table(HouseVotes84$Class,HouseVotes84$V2) #contingency table 
#Thus P(Yes on V3 | Republican) = 0.5067


#(c)-----------------------------------------------
#adoption of budget resolution = Yes for V3
#against physician fee freeze = No for V4
#for duty free exports = Yes for V15
#find prob is a democrat
#Find P(Democrat | Yes V3, No V4, Yes V15) -> Postierior Probability

y <- HouseVotes84[1,]
y$V3 <- "y"
y$V4 <- "n"
y$V15 <- "y"

predict(model,newdata=y[,c(4,5,16)],type="raw") #0.6138

#another way:
y[,-c(4,5,16)] <- NA
predict(model,newdata=y,type="raw") #gives same answer

#---------------------------------------------------
#Problem 2-------------------------------------------
#----------------------------------------------------
newwdbc <- read.csv("~/Data Mining/newwdbc.csv")
quantwdbc <- newwdbc[,-1]
                    
#Shapiro-Wilk Test
pvals = 1
for (i in 1:ncol(quantwdbc)){
  x <- shapiro.test(quantwdbc[,i])$p.value
  pvals[i] <- x
}

#histogram

for (i in 1:ncol(quantwdbc)){
  hist(quantwdbc[,i], main = paste("Histogram of column ",i+1))
}

#qq-plots
for (i in 1:ncol(quantwdbc)){
  qqnorm(quantwdbc[,i], main = paste("QQ-plot of column ",i+1))
}

#---------------------------------------------------
#Problem 3 ----------------------------------------
#--------------------------------------------------
trainsamp <- sample(nrow(newwdbc), round(0.7*nrow(newwdbc),0))
train <- newwdbc[trainsamp,]
test <- newwdbc[-trainsamp,]

#(a) treat quants as normally distributed variables-------------------
model=naiveBayes(V2~.,data=train)
predDiagnosis=predict(model,newdata=test)

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}
confmatrix(newwdbc$V2[-trainsamp],predDiagnosis) #accuracy = 0.924

#(b) categorical variables with 4 levels----------------------------

discdata<-newwdbc

#manually discretizing it into 4 bins - don't use the V2 column
for(j in 2:ncol(newwdbc)){
  discdata[,j]=as.factor(cut(newwdbc[,j],4))
  
}


model=naiveBayes(V2~.,data=discdata[trainsamp,])


predDiagnosis=predict(model,newdata=discdata[-trainsamp,])
confmatrix(newwdbc$V2[-trainsamp],predDiagnosis)

#----------------------------------------------------
#Problem 4-----------------------------------------
#---------------------------------------------------

#Use 10-fold cross validation
#(a) normally distributed variables
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

model=naiveBayes(V2~.,data=train)
predDiagnosis=predict(model,newdata=test)
confmatrix(newwdbc$V2[-trainsamp],predDiagnosis)

for(k in 1:10){
  temptest=newwdbc[folds==k,]
  temptrain=newwdbc[folds!=k,]
  
  tempBayes=naiveBayes(V2~., data = temptrain)
  accvector[k]=confmatrix(temptest$V2,
                          predict(model,newdata=temptest))$accuracy
}

mean(accvector) #0.942

#(b) Categorical variables with L levels, L = 2,3,...,10

#I manually changed L
discdata<-newwdbc
for(j in 2:ncol(newwdbc)){
  discdata[,j]=as.factor(cut(newwdbc[,j],10))
}
model=naiveBayes(V2~.,data=discdata[trainsamp,])

accvector1 = 1

for(k in 1:10){
  temptest=discdata[folds==k,]
  temptrain=discdata[folds!=k,]
  
  tempBayes=naiveBayes(V2~., data = temptrain)
  accvector1[k]=confmatrix(temptest$V2,
                          predict(model,newdata=temptest))$accuracy
}

mean(accvector1)

#L = 2 has accuracy of 0.8998
#L= 3 has accuracy of 0.949
#L = 4 has accuracy of 0.9437
#L = 5 has accuracy of 0.9472
#L = 6 has accuracy of 0.9542
#L = 7 has accuracy of 0.9472
#L = 8 has accuracy of 0.9507
#L = 9 has accuracy of 0.9455
#L = 10 has accuracy 0f 0.9419

#---------------------------------------------------
#Problem 5----------------------------------------
#--------------------------------------------------
model=naiveBayes(V2~.,data=train, laplace = 10)
predDiagnosis=predict(model,newdata=test)

confmatrix(newwdbc$V2[-trainsamp],predDiagnosis)

#0.9239 Laplace = 1
discdata <- newwdbc
for(j in 2:ncol(newwdbc)){
  discdata[,j]=as.factor(cut(newwdbc[,j],6))
  
}


model=naiveBayes(V2~.,data=discdata[trainsamp,], laplace = 5)


predDiagnosis=predict(model,newdata=discdata[-trainsamp,])
confmatrix(newwdbc$V2[-trainsamp],predDiagnosis)
