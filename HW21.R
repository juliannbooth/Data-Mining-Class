#Juliann Booth
#Data Mining II
#HW 21
#due: 1/28/16


#UCI site is down, so Mary sent me the csv of the data that she
#had from last week

auto_data <- read.csv("C:/Users/000680344/Desktop/Data Mining II/auto_data.csv")

#remove categorical variables. Note, discrete variables are categorical variables! Columns 2,7,8,9 

nocat_auto <- auto_data[,-c(2,7,8,9)]



#examine data for anomalies:
#Distance to k-nearest neighbor, k=5

#use na.rm = TRUE to fix the NA values
xbar <- apply(nocat_auto,2,mean, na.rm=TRUE)
xbarMatrix <- cbind(rep(1,nrow(nocat_auto)))%*%xbar
s=apply(nocat_auto,2,sd,na.rm=TRUE)
sMatrix=cbind(rep(1,nrow(nocat_auto)))%*%s

z=(nocat_auto-xbarMatrix)/sMatrix
apply(z,2,mean, na.rm=TRUE) # 0
apply(z,2,sd, na.rm=TRUE) #1

#Distance Matrix
D = as.matrix(dist(z))
kdist = 1:nrow(z)

for(i in 1:nrow(z)){
  kdist[i]=(sort(D[i,]))[6]
}

#plot density of the outlier score:
install.packages("proto")
install.packages("gridExtra")
packageurl = "http://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_1.0.1.tar.gz"
install.packages(packageurl,repos=NULL,type="source")
library(ggplot2)
library(gridExtra)

#Plot with Color Determined by kdist (heat map with blue)
#par(mfrow=c(3,3),mai=c(.3,.7,.7,.36)) #setting parameters of screen to show 9 plots
#xlab = "mpg", ylab = "displacement"
plot1<-ggplot(data=z,aes(x=z[,1],y=z[,2],col=kdist,size=3))+geom_point()  + labs(x = "mpg", y = "displacement")

#Gradient Plot (Heatmap)
#ggplot(data=z,aes(x=z[,1],y=z[,2],col=kdist,size=3))+geom_point()+
 # scale_colour_gradientn(colours=c("black", "red")) + labs(x = "mpg", y = "displacement")#specify the extremes to be black and red
#to make it easier to see

#I prefer the blue
#xlab = "mpg", ylab = "horsepower"
plot2<-ggplot(data=z,aes(x=z[,1],y=z[,3],col=kdist,size=3))+geom_point()  + labs(x = "mpg", y = "horsepower")

#xlab = "mpg", ylab = "weight"
plot3<-ggplot(data=z,aes(x=z[,1],y=z[,4],col=kdist,size=3))+geom_point()  + labs(x = "mpg", y = "weight")

#xlab = "mpg", ylab = "acceleration"
plot4<-ggplot(data=z,aes(x=z[,1],y=z[,5],col=kdist,size=3))+geom_point()  + labs(x = "mpg", y = "acceleration")

#xlab = "displacement", ylab = "horsepower"
plot5<-ggplot(data=z,aes(x=z[,2],y=z[,3],col=kdist,size=3))+geom_point()  + labs(x = "displacement", y = "horespower")

#xlab = "displacement", ylab = "weight"
plot6<-ggplot(data=z,aes(x=z[,2],y=z[,4],col=kdist,size=3))+geom_point()  + labs(x = "displacement", y = "weight")

#xlab = "displacement", ylab = "acceleration"
plot7 <- ggplot(data=z,aes(x=z[,2],y=z[,5],col=kdist,size=3))+geom_point()  + labs(x = "displacement", y = "acceleration")

#xlab = "horsepower", ylab = "weight"
plot8<-ggplot(data=z,aes(x=z[,3],y=z[,4],col=kdist,size=3))+geom_point()  + labs(x = "horsepower", y = "weight")

#xlab = "horsepower", ylab = "acceleration"
plot9<-ggplot(data=z,aes(x=z[,3],y=z[,5],col=kdist,size=3))+geom_point()  + labs(x = "horsepower", y = "acceleration")

#xlab = weight", ylab = "acceleration"
plot10<-ggplot(data=z,aes(x=z[,4],y=z[,5],col=kdist,size=3))+geom_point()  + labs(x = "weight", y = "acceleration")

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10, ncol=2)


#Density curve for Outlier Scores (kdist)
plot(density(kdist))

(1:nrow(z))[outlier.scores >=2.0] #29 262 321 328 360 382

#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#Density

my.density=function(data,k){
  n=nrow(data)
  D=as.matrix(dist(data))
  density=1:n
  for(i in 1:n){
    knn.distances=(sort(D[i,]))[2:(k+1)]
    density[i]=(mean(knn.distances))^(-1)
  }
  return(density)
}

z.density=my.density(z,5)

dense1<-ggplot(data=z,aes(x=z[,1],y=z[,2],col=z.density,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("blue", "red"))+labs(x = "mpg", y = "displacement")

dense2<-ggplot(data=z,aes(x=z[,1],y=z[,3],col=z.density,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("blue", "red"))+labs(x = "mpg", y = "horsepower")

dense3<-ggplot(data=z,aes(x=z[,1],y=z[,4],col=z.density,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("blue", "red"))+labs(x = "mpg", y = "weight")

dense4<-ggplot(data=z,aes(x=z[,1],y=z[,5],col=z.density,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("blue", "red"))+labs(x = "mpg", y = "acceleration")

dense5<-ggplot(data=z,aes(x=z[,2],y=z[,3],col=z.density,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("blue", "red"))+labs(x = "displacement", y = "horsepower")

dense6<-ggplot(data=z,aes(x=z[,2],y=z[,4],col=z.density,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("blue", "red"))+labs(x = "displacement", y = "weight")

dense7<-ggplot(data=z,aes(x=z[,2],y=z[,5],col=z.density,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("blue", "red"))+labs(x = "displacement", y = "acceleration")

dense8<-ggplot(data=z,aes(x=z[,3],y=z[,4],col=z.density,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("blue", "red"))+labs(x = "horsepower", y = "weight")

dense9<-ggplot(data=z,aes(x=z[,3],y=z[,5],col=z.density,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("blue", "red"))+labs(x = "horsepower", y = "acceleration")

dense10<-ggplot(data=z,aes(x=z[,4],y=z[,5],col=z.density,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("blue", "red"))+labs(x = "weight", y = "acceleration")

grid.arrange(dense1, dense2, dense3, dense4, dense5, dense6, dense7, dense8, dense9, dense10, ncol=2)



plot(density(z.density))


#------------------------------------------------------------------
#------------------------------------------------------------------

#Local Outlier Factor method
library(DMwR)

#get rid of 6 rows with n/a since the gglop ignores them anyway
zomit <- na.omit(z)
outlier.scores=lofactor(zomit,k=5)

ggplot(data=zomit,aes(x=zomit[,1],y=zomit[,2],col=outlier.scores,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("blue","red"))

ofm1<-ggplot(data=zomit,aes(x=zomit[,1],y=zomit[,2],col=outlier.scores,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("black", "red"))+labs(x = "mpg", y = "displacement")

ofm2<-ggplot(data=zomit,aes(x=zomit[,1],y=zomit[,3],col=outlier.scores,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("black", "red"))+labs(x = "mpg", y = "horsepower")

ofm3<-ggplot(data=zomit,aes(x=zomit[,1],y=zomit[,4],col=outlier.scores,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("black", "red"))+labs(x = "mpg", y = "weight")

ofm4<-ggplot(data=zomit,aes(x=zomit[,1],y=zomit[,5],col=outlier.scores,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("black", "red"))+labs(x = "mpg", y = "acceleration")

ofm5<-ggplot(data=zomit,aes(x=zomit[,2],y=zomit[,3],col=outlier.scores,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("black", "red"))+labs(x = "displacement", y = "horsepower")

ofm6<-ggplot(data=zomit,aes(x=zomit[,2],y=zomit[,4],col=outlier.scores,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("black", "red"))+labs(x = "displacement", y = "weight")

ofm7<-ggplot(data=zomit,aes(x=zomit[,2],y=zomit[,5],col=outlier.scores,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("black", "red"))+labs(x = "displacement", y = "acceleration")

ofm8<-ggplot(data=zomit,aes(x=zomit[,3],y=zomit[,4],col=outlier.scores,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("black", "red"))+labs(x = "horsepower", y = "weight")

ofm9<-ggplot(data=zomit,aes(x=zomit[,3],y=zomit[,5],col=outlier.scores,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("black", "red"))+labs(x = "horsepower", y = "acceleration")

ofm10<-ggplot(data=zomit,aes(x=zomit[,4],y=zomit[,5],col=outlier.scores,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("black", "red"))+labs(x = "weight", y = "acceleration")

grid.arrange(ofm1, ofm2, ofm3, ofm4, ofm5, ofm6, ofm7, ofm8, ofm9, ofm10, ncol=2)

plot(density(outlier.scores))

(1:nrow(zomit))[outlier.scores >=3.0] #382
#----------------------------------
library(DMwR)


outlier.scores=lofactor(mydata3,k=5)

ggplot(data=mydata3,aes(x=x,y=y,col=outlier.scores,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("blue","red"))

plot(density(outlier.scores))
dim(mydata3)
(1:21)[outlier.scores >=30] #we got the 30 from the density plot






