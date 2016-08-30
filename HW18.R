#Juliann Booth
#Data Mining HW 18
#November 21, 2015

#Problem 1
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

#-----------------------------------------------------
#(a)-------------------------------------------------
#-----------------------------------------------------

#Find number of clusters that maximizes the silhouette coefficient. Plot it
#verses K

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

library(fields)
dmatrix=rdist(mydata)

library(cluster)

min.rep=function(K,epsilon){
  ceiling(log(epsilon)/log(1-factorial(K)/K^K)) #ceiling rounds up
}

mysil=function(x,dmatrix){
  return(mean(silhouette(x=x,dmatrix=dmatrix)[,3]))
}

plot.sil=function(data,max.K,max.iter,epsilon,dmatrix){
  sil.vect=1:max.K
  for(K in 2:max.K){
    iter=min(max.iter,min.rep(K,epsilon))
    kmeans.temp=repeat.kmeans(data,K,iter)
    sil.vect[K]=mysil(kmeans.temp$cluster,dmatrix)
  }
  sil.vect=sil.vect[2:max.K]
  plot(2:max.K,sil.vect,xlab="K",ylab="Silhouette Coefficient")
  return(max(sil.vect))
}

plot.sil(mydata,10,1000,.01,dmatrix) #0.8202



#-----------------------------------------------------
#(b)-------------------------------------------------
#----------------------------------------------------

#What is the max possible value for the silhouette coefficient?
plot.sil(mydata,10,1000,.01,dmatrix) #0.8202

#----------------------------------------------
#(c)-----------------------------------------
#---------------------------------------------

#Plot SSW verses K

plot.ssw=function(data,max.K,max.iter,epsilon){
  ssw.vect=1:max.K
  for(K in 1:max.K){
    iter=min(max.iter,min.rep(K,epsilon))
    kmeans.temp=repeat.kmeans(data,K,iter)
    ssw.vect[K]=kmeans.temp$tot.withinss
  }
  plot(1:max.K,ssw.vect,xlab="K",ylab="SSW")
}

plot.ssw(mydata, 10, 1000, 0.01)



#-------------------------------------------
#Problem 2----------------------------------
#---------------------------------------------

#(a)
newwdbc.x <- newwdbc[,-1]
dmatrix1=rdist(newwdbc.x)

plot.sil(newwdbc.x,10,1000,.01,dmatrix1)

#(b)
plot.sil(newwdbc.x,10,1000,.01,dmatrix1) #0.697

#(c)
plot.ssw(newwdbc.x, 10, 1000, 0.01) #question this.
