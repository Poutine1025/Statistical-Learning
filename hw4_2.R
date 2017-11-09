#定义函数，生成数据
set.seed(21)
f<-function(x){
  return(sin(12*(x+0.2))/(x+0.2))
}
X=runif(100,min = 0,max = 1)
X=data.matrix(sort(X))
Y=f(X)+rnorm(length(X))

#安装并调用包
#install.packages('sfsmisc')
library(sfsmisc)

#对不同的自由度作smooth spline拟合图像,这里以自由度为15为例
df_lambda=15
S=smooth.spline(X,Y,df=df_lambda,cv=TRUE)
#用hatMat函数计算hat matrix
S_lambda=hatMat(X,pred.sm=function(X,Y) fitted(smooth.spline(X,Y,df=df_lambda)))
cov_f_hat=S_lambda%*%t(S_lambda)
se=rep(0,times=length(X))
#计算标准差
for(i in 1:length(X)){
  se[i]=sqrt(cov_f_hat[i,i])
}
x1=S$y-2*se
x2=S$y+2*se
#作图
plot(x=X,xlim=c(0,1),ylim=c(-4,4),xlab="X",ylab="y",type="n",main="df=15")
polygon(c(X,rev(X)),c(x1, rev(x2)),col="yellow",border=NA)
lines(X,S$y,col="green",lwd=2)
lines(X,f(X),col='purple',lwd=2)
points(X,Y)

#对不同自由度计算CV和EPE
df=seq(5,15,by=0.4)
CV=rep(0,times=length(df))
EPE=rep(0,times=length(df))
for(i in 1:length(df)){
  H=smooth.spline(X,Y,df=df[i],cv=TRUE)
  CV[i]=H$cv.crit
  #用随机产生的模拟数据计算EPE的值
  n=1000
  X_test=sort(runif(n,min=0,max=1))
  f_hat=predict(H,X_test)$y
  EPE[i]=1+mean((f(X_test)-f_hat)^2)
}
#作图
plot(df,CV,col="blue",pch=16,ylim=c(1,1.7),xlab="df_lambda",ylab="EPE and CV",main="Cross-Validation")
points(df,EPE,col="orange",pch=16)
legend("topright",cex=0.8,legend=c("CV","EPE"),lty=c(1,1),col=c("blue","orange"))
abline(v=df[which.min(CV)])
abline(v=df[which.min(EPE)])
