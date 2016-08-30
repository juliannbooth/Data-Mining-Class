#Juliann Booth
#September 9, 2015
#HW 3

#Problem 1 -----------------------------------
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

help(kyphosis)

#Problem 2 ------------------------------------
#a

kyphosis$Kyphosis <- as.factor(kyphosis$Kyphosis)
tree <- rpart(Kyphosis~., data = kyphosis)
fancyRpartPlot(tree)
#b, c
#confmatrix function
confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}

#Confusion matrix, accuracy, and error rate 
confmatrix(kyphosis$Kyphosis,
           predict(tree,newdata=kyphosis,type="class"))

#Problem 3 -----------------------------------
attach(kyphosis)
plot(Age, Start, col = c('blue','red')[Kyphosis])
#blue is absent, red is present

#Problem 4 -----------------------------------
#a
library(party)
tree2=ctree(Kyphosis~.,data=kyphosis)
#Simple plot.
plot(tree2,type='simple')

#ctree confusion matrix.
#Note that we don't need type = "class". By default, ctree does class.
predKyphosis=predict(tree2,newdata=kyphosis)
confusionmatrix=table(Kyphosis,predKyphosis)
confusionmatrix
accuracy=sum(diag(confusionmatrix))/sum(confusionmatrix)
error = 1-accuracy
accuracy
error

#Problem 6 -----------------------------------
entropy <- function(vector){
  for (i in 1:length(vector)){
    if (vector[i]!= 0){
      vector[i] <- vector[i]*log2(vector[i])
    }
    else{vector[i]<-vector[i]}
  }
  -1*sum(vector)
}

table <- Kyphosis == 'absent'
sum(table) #gives the amount of times kyphosis is absent. 
#64 in this case.

vector <- c(64/81, 17/81)
entropy(vector) #0.741

#For ctree: Note that wectree = "weighted entropy of ctree"
node1 <- c(0.421,0.579)
entropy(node1) #0.9819166
node2 <- c(0.903,0.097)
entropy(node2) #0.459413
wectree <- ((19/81)*entropy(node1)) + ((62/81)*entropy(node2))
wectree #0.5819756

#For rpart: Note that werpart = "weighted entropy of rpart"
rnode1 <- c(0.86,0.14)
rnode2 <- c(.43,.57)
rnode3 <- c(0.42, 0.58)
werpart <- (0.17*entropy(rnode1)) + (0.09*entropy(rnode2)) 
+ (0.23*entropy(rnode3))
werpart #0.4137783

#Problem 7 -----------------------------------------------
#calculate largest petal length for a setosa
max(iris$Petal.Length[Species == 'setosa']) #1.9
min(iris$Petal.Length[Species == 'versicolor']) #3

