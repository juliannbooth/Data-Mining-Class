#Juliann Booth
#Data Mining HW 14

#Problem 1
gradientfct=function(w){
  return(-t(X)%*%y+t(X)%*%X%*%w)
}
#gradient is partial wrt f(x,y) so 2x, 2y+1

errorfct=function(w){
 w[1]^2+w[2]^2+w[2]
}

#error function is w[1]^2 + w[2]^2+w[2] where w = c(x,y)


gradientdescent=function(gradientfct,errorfct,parameter0,eta0=0.1,maxit=100,abstol=10^-4){
  parameter=parameter0
  eta=eta0
  olderror=errorfct(parameter)
  for(i in 1:maxit){
    parameter=parameter-eta*gradientfct(parameter)
    if(sum(gradientfct(parameter)^2)<abstol){
      print(paste("Converged in",i,"iterations."))
      return(list(parameter=parameter,error=newerror,eta=eta))
    }
    newerror=errorfct(parameter)
    if(olderror>newerror){eta=1.1*eta}else{eta=0.5*eta}
    olderror=newerror
  }
  print(paste("Did not converge in ",maxit," iterations."))
  return(list(parameter=parameter,error=newerror,eta=eta))
  
}