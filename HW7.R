#Juliann Booth
#Data Mining HW 7
#due: October 1, 2015

#Problem 1---------------------------------------------

#triweight kernel: figure out what I is
#from notes: .  Indicator of y_i = r (represented by I(y_i = r))
#is 1 when true and 0 when false. Don't use the I because 
#Boolean values will be 0 or 1 when multiplied by numbers

#plot the triweight and cosine kernel


d <- seq(-1.2, 1.2, 0.1)
triweight <- 35/32*(1-d^2)^3*(abs(d) <= 1)
plot(d, triweight, type = "l", main = "Triweight Kernel")
xlim = c(-1.2,1.2)
ylim = c(0,1.2)

d <- seq(-1.2, 1.2, 0.1)
cosinekern <- (pi/4)*(cos((pi/2)*d))*(abs(d)<=1)
plot(d, cosinekern, type = "l", main = "Cosine Kernel")
xlim = c(-1.2,1.2)
ylim = c(0,1.2)

#Problem 2 ------------------------------------------------
#Use train.kknn to find the optimal kernel and k
newwdbc <- read.csv("~/Data Mining/newwdbc.csv")
library(kknn)

fit.wdbc = train.kknn(V2 ~ ., data=newwdbc, kmax = 15, 
                       kernel =c("rectangular","triangular", "epanechnikov",  "biweight",
                                 "triweight","cos","inv", "gaussian" ,"optimal"), 
                       distance = 2)
plot(fit.wdbc)
fit.wdbc$best.parameters

#Problem 3------------------------------------------------

#standardize! (209)
#take out categorical variable:
x <- newwdbc[,-1]
xbar=apply(x,2,mean)
xbarMatrix=cbind(rep(1,nrow(newwdbc)))%*%xbar
s=apply(x,2,sd)
sMatrix=cbind(rep(1,nrow(newwdbc)))%*%s
z=(x-xbarMatrix)/sMatrix

apply(z,2,mean)
apply(z,2,sd)

trainsamp <- sample(nrow(z), round(0.7*nrow(z),0))
train <- z[trainsamp,]
test <- z[-trainsamp,]

diagnosis <- newwdbc$V2
#diagnosis[trainsamp] are the actual diagnoses for the train set

predy=(kknn(diagnosis[trainsamp]~.,train, test,
             k=10,kernel="inv",distance=2))$fitted.values

confmatrix=function(y,predy){
  matrix=table(y,predy)
  accuracy=sum(diag(matrix))/sum(matrix)
  return(list(matrix=matrix,accuracy=accuracy,error=1-accuracy))
}

confkknn <- confmatrix(diagnosis[-trainsamp],predy) 
confkknn$accuracy #accuracy: 98.24%

#confidence interval for the accuracy:
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

accuracyconfint(confkknn$accuracy,nrow(test) ,0.05) #(0.96278, 1.002)

#Problem 4----------------------------------------------------------
predy2=(kknn(diagnosis[trainsamp]~.,train, test,
            k=4,kernel="rectangular",distance=2))$fitted.values
confkknnr <- confmatrix(diagnosis[-trainsamp],predy2) 
confkknnr$accuracy #accuracy: 97.076%


#Problem 5 -------------------------------------------------------
accvectorkknn <- (diagnosis[-trainsamp] == predy)
accvectorkknnr <- (diagnosis[-trainsamp] == predy2)
mcnemartable <- table(accvectorkknn, accvectorkknnr)
library(exact2x2)
mcnemar.exact(mcnemartable)









