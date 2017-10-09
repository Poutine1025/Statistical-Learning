#数据处理
data<-read.table('prostate.data')
Xtrain<-data[data$train==TRUE,]
Xtest<-data[data$train==FALSE,]
Xtrain=Xtrain[,-10]
Xtest=Xtest[,-10]

#计算测试误差的函数
TestError<-function(Y,Yhat){
  result=mean((Y-Yhat)^2)
  return(result)
}

#计算标准误差的函数
StdError<-function(Y,Yhat){
  result=0
  Ntest=dim(Y)[1]
  T=TestError(Y,Yhat)
  for(i in 1:Ntest) result=result+((Y[i,1]-Yhat[i,1])^2-T)^2
  result=result/(Ntest*(Ntest-1))
  result=sqrt(result)
  return(result)
}

#最小二乘拟合
LS=lm(lpsa~.,data = Xtrain)
Yhat.LS=data.matrix(predict.lm(LS,Xtest))
Y.LS=data.matrix(Xtest$lpsa)
TestError.LS=TestError(Y.LS,Yhat.LS)
StdError.LS=StdError(Y.LS,Yhat.LS)

#最佳子集选择的预测函数
pred.subsets<-function(object,newdata,id){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id)
  xvars=names(coefi)
  return(mat[,xvars]%*%coefi)
}

#最佳子集选择
#安装leaps包
install.packages('leaps')
library(leaps)
#总变量数
varnum=dim(Xtrain)[2]-1
#回归
BestSubset<-regsubsets(lpsa~.,data = Xtrain,nvmax = varnum,intercept = TRUE)
reg.summary=summary(BestSubset)
#记录各个模型的测试误差和标准误差
Error.BS=matrix(data = NA,nrow = varnum,ncol = 2)
for(i in 1:varnum){
  Y.BS=data.matrix(Xtest$lpsa)
  Yhat.BS=pred.subsets(BestSubset,Xtest,i)
  Error.BS[i,1]=TestError(Y.BS,Yhat.BS)
  Error.BS[i,2]=StdError(Y.BS,Yhat.BS)
}

#岭回归
X.ridge=model.matrix(lpsa~.,Xtrain)[,-1]
Y.ridge=Xtrain$lpsa
Xtest.ridge=model.matrix(lpsa~.,Xtest)[,-1]
Ytest.ridge=data.matrix(Xtest$lpsa)
install.packages('glmnet')
library(glmnet)
library(boot)
ridge.mod=glmnet(X.ridge,Y.ridge,alpha = 0)
set.seed(5)
ridge.cv=cv.glmnet(X.ridge,Y.ridge,alpha = 0,nfolds = 5)
bestlam=ridge.cv$lambda.1se
ridge.pred=predict(ridge.mod,s = bestlam,newx = Xtest.ridge)
TestError.ridge=TestError(Ytest.ridge,ridge.pred)
StdError.ridge=StdError(Ytest.ridge,ridge.pred)

#LASSO
X.lasso=model.matrix(lpsa~.,Xtrain)[,-1]
Y.lasso=Xtrain$lpsa
Xtest.lasso=model.matrix(lpsa~.,Xtest)[,-1]
Ytest.lasso=data.matrix(Xtest$lpsa)
lasso.mod=glmnet(X.lasso,Y.lasso,alpha = 1)
set.seed(5)
lasso.cv=cv.glmnet(X.lasso,Y.lasso,alpha=1)
bestlam.lasso=lasso.cv$lambda.1se
lasso.pred=predict(lasso.mod,s=bestlam.lasso,newx = Xtest.lasso)
TestError.lasso=TestError(Ytest.lasso,lasso.pred)
StdError.lasso=StdError(Ytest.lasso,lasso.pred)
