#Juliann Booth
#Data Mining HW 2
#due Sepetember 8,2015

#Problem 6

#Gini
vector <- c(0.4,0.2,0.1,0.3)

gini <- function(vector){
  store <- rep(1,length(vector))
  for (i in 1:length(vector)){
    store[i] <- vector[i]^2
  }
  1-sum(store)
}

#Classification Error
classerror <- function(vector){
  1-max(vector)
}

#Entropy
entropy <- function(vector){
  for (i in 1:length(vector)){
    if (vector[i]!= 0){
      vector[i] <- vector[i]*log2(vector[i])
    }
    else{vector[i]<-vector[i]}
  }
  -1*sum(vector)
}

#Problem 7 (Example code)
x = seq(from =-5, to =5, by=0.01)
mysquare = function(a){
  return(a^2)
}
y=sapply(x,mysquare)
plot(x,y,type='l')

#Problem 8

#two-class gini
vector <- seq(from=0, to=1, by=0.01)
twoclassgini <- function(vector){
  store <- rep(1,length(vector))
  for (i in 1:length(vector)){
    p0 <- vector[i]
    p1 <- 1-p0
    store[i] <- 1-(p0^2 + p1^2)
  }
  plot(vector,store,type='l', xlab = 'p0', ylab = 'Gini Measure',
       main = 'Two Class Gini')
}


#two-class entropy

vector <- seq(from=0, to=1, by=0.01)
twoclassentropy <- function(vector){
  store <- rep(1,length(vector))
  for (i in 1:length(vector)){
    p0 <- vector[i]
    p1 <- 1-p0
    if (p1 == 1 | p1 == 0){
      store[i] <- 0
    }
    else{
      store[i] <- -(p0*log2(p0) + p1*log2(p1))
    }
  }
  plot(vector,store,type='l', xlab = 'p0', ylab = 'Entropy Measure',
       main = 'Two Class Entropy Measure')
}

#two-class classification error
twoclasserror <- function(vector){
  store <- rep(1,length(vector))
  for (i in 1:length(vector)){
    p0 <- vector[i]
    p1 <- 1-p0
    p <- max(p0,p1)
    store[i] <- 1-p
  }
  plot(vector,store,type='l', xlab = 'p0', ylab = 'Classification Error Measure',
       main = 'Two Class Classification Error')
}

#Problem 9
Hw2 <- read.csv("~/Data Mining/Hw2.csv")
#a
dim(Hw2)

attach(Hw2)
table(Class)
class <- c(480/1000,520/1000)
entropy(class)

#b
table(Gender)
table(Gender, Class)
Male <- c(290/509,219/509)
entropy(Male) #0.9859187

Female <- c(190/491, 301/491)
entropy(Female) #0.9628133

tree <- ctree(Class~Gender, data = Hw2)
plot(tree3)


#c
tree3 <- rpart(CarType~., data = Hw2)
fancyRpartPlot(tree3)

#d
tree3 <- rpart(ShirtSize~., data = Hw2)
fancyRpartPlot(tree3)
