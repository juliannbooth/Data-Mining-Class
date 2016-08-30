#Juliann Booth
#Data Mining HW 17
#due November 23, 2015

set.seed(5364)
x1=rnorm(100,5,1)
x2=rnorm(100,5,1)
x3=rnorm(100,15,1)
x4=rnorm(100,15,1)

y1=rnorm(100,10,1)
y2=rnorm(100,20,1)
y3=rnorm(100,10,1)
y4=rnorm(100,20,1)

mydata = data.frame(x=c(x1,x2,x3,x4),y=c(y1,y2,y3,y4))
plot(y~x, data = mydata, asp=1, main = "mydata")

#---------------------------------------------
#(a)--------------------------------------------
#---------------------------------------------

#Perform a k-means clustering with k=4 and 1000 repetitions

repeat.kmeans=function(data,centers,repetitions){
  best.kmeans=NULL
  best.ssw=Inf
  
  for(i in 1:repetitions){
    kmeans.temp=kmeans(x=data,centers=centers)
    if(kmeans.temp$tot.withinss<best.ssw){
      best.ssw=kmeans.temp$tot.withinss
      best.kmeans=kmeans.temp
    }
  }
  return(best.kmeans)
}

best=repeat.kmeans(mydata,4,1000)


#-----------------------------------------------
#(b)--------------------------------------------
#--------------------------------------------

#Plot the points and color them based on which clusters they are in
plot(y~x,data=mydata,col=best$cluster,asp=1, main = "mydata k-means clustering")

#---------------------------------------------
#(c)---------------------------------------
#---------------------------------------------

#Find the total, total within, and between sums of squares

best$totss #20851.05
best$tot.withinss #721.0723
best$betweenss #20129.98

#-------------------------------------------
#(d)---------------------------------------
#-------------------------------------------

#Find the centers of the clusters

best$centers
plot(y~x,data=mydata,col=best$cluster,asp=1, main = "mydata centers")
points(best$centers,col='black',pch=24,bg='black')

#---------------------------------------------
#(e)---------------------------------------
#--------------------------------------------

#We want at least one of our repetitions of k-means to have the property that every cluster
#contains exactly one initial centroid.How many repetitions would be necessary to ensure that
#this happens with at least 99% probability?

min.rep=function(K,epsilon){
  ceiling(log(epsilon)/log(1-factorial(K)/K^K)) #ceiling rounds up
}

min.rep(4,.01) #47

#--------------------------------------------
#(f)---------------------------------------
#--------------------------------------------

#can you find a choice of initial centers that does not result in the optimal clusters?
#What is the total within sum of squares for that clustering?

centers0=cbind(c(4,6,10,10),c(20,20,15,19))
kout=kmeans(mydata,centers=centers0)
plot(y~x,data=mydata,col=kout$cluster,asp=1, main = "bad centers")
points(centers0,col='black',pch=24,bg='black')
kout$tot.withinss #5604.302

#---------------------------------------
#Problem 2
#-------------------------------------------

newwdbc <- read.csv("C:/Users/User/Downloads/newwdbc.csv")
quantonly <- newwdbc[,-1]

wdbc_kmeans=repeat.kmeans(quantonly,2,1000)

table(wdbc_kmeans$cluster,newwdbc$V2)

accuracy = (130+356)/(130+356+82+1)
