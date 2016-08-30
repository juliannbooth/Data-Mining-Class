#Juliann Booth
#Data Mining HW 12
#due: October 27, 2015

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}


#Problem 1--------------------------------------------
#(a)---------------------------------------------------

irisnew <- iris[,-5]
Species <- c(rep("setosa",50), rep("nonsetosa",100))
irisnew <- cbind(irisnew, Species)

#(b) Linear SVM with cost = 1000 and plot the SVM---------
library(e1071)


train=sample(nrow(irisnew),round(nrow(irisnew)*.7,0))

#Fit SVM Model to Training Data
model=svm(Species~.,data=irisnew[train,],kernel="linear",cost=1000) 

#Make predictions for test data
predSpecies = predict(model,newdata=irisnew[-train,])
confmatrix(predSpecies,irisnew$Species[-train])


#Plot model
#need formula because we have more than 2 dimensions
plot(x= model,data=irisnew[train,], formula=Petal.Length~Petal.Width)


model
model$rho     #The negative intercept, i.e., rho = -b
model$SV      #Support vectors (All data is scaled, include SV's). automatically standardized
model$index   #Positions in data set of SV's (gives the row numbers in our original data set that our SVs are)
model$coefs   #coef = lambda*y

#Calculating w and b
w=t(model$coefs)%*%(model$SV) #transpose of lambdai*yis and matrix 
#multiplying it by the support vectors
b=-model$rho #we take the negative because rho= -b


#-----------------------------------------------------------------------
#Problem 2-------------------------------------------------------------
#-----------------------------------------------------------------------

newwdbc <- read.csv("~/Data Mining/newwdbc.csv")
train <- sample(nrow(newwdbc), round(0.7*nrow(newwdbc),0))

#(a) fit an SVM-------------------------------
model2 = svm(V2~., data = newwdbc[train,])

#(b)----------------------------------------
model2

#(c)------------------------------------------
#training
predDiag = predict(model2,newdata=newwdbc[train,])
confmatrix(predDiag,newwdbc$V2[train]) #0.9899

#testing
predDiag = predict(model2,newdata=newwdbc[-train,])
confmatrix(predDiag,newwdbc$V2[-train]) #0.965


#(d)------------------------------------------
tunediag=tune.svm(V2~.,data=newwdbc[train,],
                  gamma = 10^(-6:1), 
                  cost = 10^(1:10))


tunediag$best.parameters
#gamma = 1e-04, cost = 10000 :: gamma = 10^(-6:1), cost = 10^(1:10)

#(e)------------------------------------------
diagmodel=svm(V2~.,data=newwdbc[train,],
               gamma=0.0001,
               cost=10000)
diagmodel

#(f)------------------------------------------
confmatrix(newwdbc$V2[train],
           predict(diagmodel,newdata=newwdbc[train,])) #0.9997

confmatrix(newwdbc$V2[-train],
           predict(diagmodel,newdata=newwdbc[-train,])) #0.9474


#------------------------------------------------------------
#Problem 3--------------------------------------------------
#------------------------------------------------------------

#(a)-------------------------------------
redx1 <- rnorm(50,0,1.5)
redx2 <- rnorm(50,6,1.5)
blackx1 <- rnorm(50,0,1.5)
blackx2 <- rnorm(50,6,1.5)
x <- c(redx1, redx2, blackx1, blackx2)

redy1 <- rnorm(50,6,1.5)
redy2 <- rnorm(50,0,1.5)
blacky1 <- rnorm(50,0, 1.5)
blacky2 <- rnorm(50,6,1.5)
y <- c(redy1, redy2, blacky1, blacky2)

class <- c(rep("red",100), rep("black", 100))
class <- as.factor(class)
exdata <- data.frame(x,y, class)

symbols <- c(rep(24,100), rep(21, 100)) #pch = 21 is circle, 
#pch = 24 is triangle
plot(exdata$x, exdata$y, col = c('black','red')[exdata$class],
     pch = symbols, xlab = "x", ylab = "y")

#(b)-----------------------------------------
train <- sample(nrow(exdata), round(0.7*nrow(exdata),0))

exsvm <- svm(class~., data = exdata[train,])
exsvm

plot(x= exsvm,data=exdata[train,])

#testing accuracy:
pred = predict(exsvm,newdata=exdata[-train,])
confmatrix(pred,exdata$class[-train]) #0.967


