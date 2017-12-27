library(rpart)
library(adabag)
N=10000
X.test=matrix(rnorm(N*10,0,1),N,10)
Y.test=as.factor(ifelse(rowSums(X.test^2)>9.34,1,-1))
data.test=data.frame(Y.test,X.test)
names(data.test)[1]=c("Y")


n=2000
X.train=matrix(rnorm(n*10,0,1),n,10)
Y.train=as.factor(ifelse(rowSums(X.train^2)>9.34,1,-1))
data.train=data.frame(Y.train,X.train)
names(data.train)[1]=c("Y")


M=400
w=rep(1/n,n)
alpha=rep(0,M)
Y.pred=rep(0,N)
error=rep(0,M)

#handwrite
for(i in 1:M){
  #用rpart函数建立stump
  tree.data=rpart(Y~., data = data.train, weights = w,
                  control = rpart.control(minsplit = 2,minbucket=0,maxdepth=1))
  #预测
  Yhat=predict(tree.data,newdata = data.train,type = "class")
  indicator=ifelse(Yhat==Y.train,0,1)
  #计算误差
  err=sum(w*indicator)/sum(w)
  alpha[i]=log((1-err)/err)
  #更新权重
  w=w*exp(alpha[i]*indicator)
  test.pred=predict(tree.data,newdata = data.test,type = "class")
  #更新预测值
  Y.pred=Y.pred+alpha[i]*as.numeric(as.character(test.pred))
  Yi=sign(Y.pred)
  #记录误差
  error[i]=sum(ifelse(Yi==Y.test,0,1))/N
}
#作图
plot(error, type = "l",
     ylim = c(0,0.5), col = "orange",
     ylab = "Test Error", xlab = "Boosting Iterations")
error

#package
adaboost.data=boosting(Y~.,data=data.train)
yhat.data=predict.boosting(adaboost.data,newdata=data.test)
yhat.data$error
