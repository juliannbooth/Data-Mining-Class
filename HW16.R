#Juliann Booth
#Data Mining HW 16
#November 19, 2015

#Problem 1
library(MASS)
data(fgl)

trainsamp <- trainsample(fgl, 0.7)
train <- fgl[trainsamp,]
test <- fgl[-trainsamp,]
#----------------------------------------------------------
#(a) Build an SVM and use the predict command---------------
#-----------------------------------------------------------
library(e1071)
set.seed(5301)

modelSVM=svm(type~.,data=train)
predTypeSVM=predict(modelSVM,newdata=test)
confmatrix(predTypeSVM,test$type)
#accuracy = 0.78125

#-----------------------------------------------------------
#(b) one-against-rest with SVM--------------------------------
#-------------------------------------------------------------
rand.which.max=function(x){
  index=((1:length(x))[x==(max(x))])
  return(sample(c(index,index),1))
}

fgl.levels=levels(fgl$type)
k = length(fgl.levels)
levels(fgl$type) = c(levels(fgl$type), "other")


model.list = vector(length=k, mode="list")
pred.list=vector(length=k,mode="list")
levels(train$type) = c(levels(train$type), "other")

for(i in 1:k){
  i.train=train
  i.train$type[(i.train$type!=fgl.levels[i])]="other"
  i.model=svm(type~.,data=i.train)
  i.pred=predict(i.model,
                 newdata=test,
                 type="class")
  model.list[[i]]=i.model
  pred.list[[i]]=i.pred 
}

model.list

vote.matrix=matrix(0,nrow=nrow(train),
                   ncol=k)
dim(vote.matrix) #150x6



for(i in 1:k){
  vote.matrix[,i]=vote.matrix[,i]+
    (pred.list[[i]]==fgl.levels[i])*1
  
  vote.matrix[,-i]=vote.matrix[,-i]+
    (pred.list[[i]]=="other")*1
}

OAR.pred=fgl.levels[apply(vote.matrix,
                           1,
                           rand.which.max)]

confmatrix(train$type, OAR.pred) #accuracy = .1266
#---------------------------------------------------------------------
#(c) One-against-one SVM----------------------------------------------
#---------------------------------------------------------------------
modelSVM
predTypeSVM

levels(fgl$type)
#WinF, WinNF, Veh, Con, Tabl, Head, other
#But we don't want other anymore


model.list=vector(length=15,mode="list")
pred.list=vector(length=15,mode="list")


k=0
for (i in 1:(length(levels(fgl$type))-2)){
  j = i+1
  while (j < length(levels(fgl$type))){
    new.train = traindata[(traindata$type == levels(fgl$type)[i])|(traindata$type == levels(fgl$type)[j]),]
    new.model = svm(type~., data = new.train)
    new.pred = predict(new.model, newdata = fgl[-trainsamp,], type = "class")
    k=k+1
    model.list[[k]]=new.model
    pred.list[[k]]=new.pred
    j = j+1
  }
}



winf.votes = vector(length=15, mode = "list")
for (i in 1:15){
  winf.votes[[i]] = (pred.list[[i]]==levels(fgl$type)[1])*1}
total.winf.votes = winf.votes[[1]]+winf.votes[[2]]+winf.votes[[3]]+
  winf.votes[[4]]+winf.votes[[5]]+winf.votes[[6]]+winf.votes[[7]]+
  winf.votes[[8]]+winf.votes[[9]]+winf.votes[[10]] + 
  winf.votes[[11]] +winf.votes[[12]] +winf.votes[[13]] +
  winf.votes[[14]] +winf.votes[[15]]


winnf.votes = vector(length=15, mode = "list")
for (i in 1:15){
  winnf.votes[[i]] = (pred.list[[i]]==levels(fgl$type)[2])*1}
total.winnf.votes = winnf.votes[[1]]+winnf.votes[[2]]+winnf.votes[[3]]+
  winnf.votes[[4]]+winnf.votes[[5]]+winnf.votes[[6]]+winnf.votes[[7]]+
  winnf.votes[[8]]+winnf.votes[[9]]+winnf.votes[[10]] + 
  winnf.votes[[11]] +winnf.votes[[12]] +winnf.votes[[13]] +
  winnf.votes[[14]] +winnf.votes[[15]]

length(total.winnf.votes) #64

veh.votes = vector(length=15, mode = "list")
for (i in 1:15){
  veh.votes[[i]] = (pred.list[[i]]==levels(fgl$type)[3])*1}
total.veh.votes = veh.votes[[1]]+veh.votes[[2]]+veh.votes[[3]]+
  veh.votes[[4]]+veh.votes[[5]]+veh.votes[[6]]+veh.votes[[7]]+
  veh.votes[[8]]+veh.votes[[9]]+veh.votes[[10]] + 
  veh.votes[[11]] +veh.votes[[12]] +veh.votes[[13]] +
  veh.votes[[14]] +veh.votes[[15]]

con.votes = vector(length=15, mode = "list")
for (i in 1:15){
  con.votes[[i]] = (pred.list[[i]]==levels(fgl$type)[4])*1}
total.con.votes = con.votes[[1]]+con.votes[[2]]+con.votes[[3]]+
  con.votes[[4]]+con.votes[[5]]+con.votes[[6]]+con.votes[[7]]+
  con.votes[[8]]+con.votes[[9]]+con.votes[[10]] + 
  con.votes[[11]] +con.votes[[12]] +con.votes[[13]] +
  con.votes[[14]] +con.votes[[15]]

tabl.votes = vector(length=15, mode = "list")
for (i in 1:15){
  tabl.votes[[i]] = (pred.list[[i]]==levels(fgl$type)[5])*1}
total.tabl.votes = tabl.votes[[1]]+tabl.votes[[2]]+tabl.votes[[3]]+
  tabl.votes[[4]]+tabl.votes[[5]]+tabl.votes[[6]]+tabl.votes[[7]]+
  tabl.votes[[8]]+tabl.votes[[9]]+tabl.votes[[10]] + 
  tabl.votes[[11]] +tabl.votes[[12]] +tabl.votes[[13]] +
  tabl.votes[[14]] +tabl.votes[[15]]

head.votes = vector(length=15, mode = "list")
for (i in 1:15){
  head.votes[[i]] = (pred.list[[i]]==levels(fgl$type)[6])*1}
total.head.votes = head.votes[[1]]+head.votes[[2]]+head.votes[[3]]+
  head.votes[[4]]+head.votes[[5]]+head.votes[[6]]+head.votes[[7]]+
  head.votes[[8]]+head.votes[[9]]+head.votes[[10]] + 
  head.votes[[11]] +head.votes[[12]] +head.votes[[13]] +
  head.votes[[14]] +head.votes[[15]]

votes.best=cbind(total.winf.votes,total.winnf.votes,total.veh.votes,total.con.votes,total.tabl.votes,total.head.votes)
apply(votes.best,1,rand.which.max)
one.against.one.pred=c("WinF",
                       "WinNF",
                       "Veh", "Con", "Tabl", "Head")[apply(votes.best,
                                          1,
                                          rand.which.max)]
table(predTypeSVM, one.against.one.pred)
sum(confmatrix(one.against.one.pred,test$type)$matrix)
#------------------------------------------------------------




