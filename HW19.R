#Juliann Booth
#Data Mining HW 19
#December 1, 2015

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
#Problem 1 --------------------------------------------------
newwdbc <- read.csv("~/Data Mining/newwdbc.csv")
newwdbc.x <- newwdbc[,-1]
dmatrix1=rdist(newwdbc.x)


best = repeat.kmeans(newwdbc.x,2,1000)
newwdbc.table = table(newwdbc$V2, best$cluster)

chisq.test(newwdbc.table, simulate.p.value = TRUE) #0.0004998

#------------------------------------------------------
#Problem 2--------------------------------------------
#------------------------------------------------------

#(a)

library(fields)
xbar=apply(newwdbc.x,2,mean)
xbarMatrix=cbind(rep(1,nrow(newwdbc.x)))%*%xbar
s=apply(newwdbc.x,2,sd)
sMatrix=cbind(rep(1,nrow(newwdbc.x)))%*%s

z=(newwdbc.x-xbarMatrix)/sMatrix
apply(z,2,mean)
apply(z,2,sd)


dmatrix=rdist(z)
sort.dmatrix=apply(dmatrix,2,sort)

k=5
kdist=sort.dmatrix[k,]

plot(sort(kdist),type="l",
     xlab="Points Sorted by k-dist",
     ylab="k-dist", main = "Sorted k-dist Values")
lines(1:length(kdist),rep(4.5,length(kdist)),col="red")



#(b)
library(fpc)
wdbc.dbscan=dbscan(z,eps=4.5,MinPts=5)


#(c)
wdbc.dbscan$cluster

#(d)
length(wdbc.dbscan$cluster[wdbc.dbscan$cluster==0]) #32
length(wdbc.dbscan$cluster) #569
percentage = (32/569)*100 #5.623

#(e)
newwdbc.table = table(newwdbc$V2, wdbc.dbscan$cluster)

chisq.test(newwdbc.table, simulate.p.value = TRUE) #0.007996

