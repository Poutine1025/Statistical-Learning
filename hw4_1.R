#生成随机数
set.seed(20)
X=runif(50,min = 0,max = 1)
X=data.matrix(sort(X))

#Global Linear
I=data.matrix(rep(1,times = 50))
X_GL=cbind(I,X)
H_GL=X_GL%*%solve(t(X_GL)%*%X_GL)%*%t(X_GL)
PV_GL=data.matrix(rep(0,times = 50))
for(i in 1:50){
  PV_GL[i]=H_GL[i,i]
}
plot(X,PV_GL,xlab='X',ylab='Pointwise Variances',xlim=c(0,1),ylim=c(0,0.6),pch=19,col='orange',type='o')

#Global Cubic Polynomial
X_GCP=cbind(I,X,X^2,X^3)
H_GCP=X_GCP%*%solve(t(X_GCP)%*%X_GCP)%*%t(X_GCP)
PV_GCP=data.matrix(rep(0,times = 50))
for(i in 1:50){
  PV_GCP[i]=H_GCP[i,i]
}
lines(X,PV_GCP,xlab='X',ylab='Pointwise Variances',xlim=c(0,1),ylim=c(0,0.6),pch=19,col='red',type='o')

#Cubic Spline - 2 knots
library(splines)
X_CS=bs(X,degree=3,knots=c(0.33,0.66),intercept=TRUE)
H_CS=X_CS%*%solve(t(X_CS)%*%X_CS)%*%t(X_CS)
PV_CS=data.matrix(rep(0,times = 50))
for(i in 1:50){
  PV_CS[i]=H_CS[i,i]
}
lines(X,PV_CS,xlab='X',ylab='Pointwise Variances',xlim=c(0,1),ylim=c(0,0.6),pch=19,col='green',type='o')

#Natural Cubic Spline - 6 knots
X_NCS=ns(X,df=3,knots=c(0.26,0.42,0.58,0.74),intercept=TRUE,Boundary.knots=c(0.1,0.9))
H_NCS=X_NCS%*%solve(t(X_NCS)%*%X_NCS)%*%t(X_NCS)
PV_NCS=data.matrix(rep(0,times = 50))
for(i in 1:50){
  PV_NCS[i]=H_NCS[i,i]
}
lines(X,PV_NCS,xlab='X',ylab='Pointwise Variances',xlim=c(0,1),ylim=c(0,0.6),pch=19,col='blue',type='o')

legend(0.2,0.6,cex=0.7,legend=c("Global Linear","Global Cubic Polynomial","Cubic Spline - 2 knots","Natural Cubic Spline - 6 knots"),lty=c(1,1,1,1),col=c("orange","red","green","blue"))
