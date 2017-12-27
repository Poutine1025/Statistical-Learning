library(ElemStatLearn)
data(spam)
set.seed(as.numeric(Sys.time()))
train=sort(sample(4601,3065))
data.train=spam[train,]
data.test=spam[-train,]

#adaboost
library(adabag)
adaboost.spam=boosting(spam~.,data=data.train,mfinal=100)
yhat.adaboost=predict.boosting(adaboost.spam,newdata=data.test)
adaboost.error=yhat.adaboost$error
adaboost.error

#gbm
library(gbm)
gbm.train=data.train
gbm.test=data.test
gbm.train$spam=ifelse(gbm.train$spam=="spam",1,0)
gbm.test$spam=ifelse(gbm.test$spam=="spam",1,0)
gbm.spam=gbm(spam~.,
             data=gbm.train,shrinkage=0.01,
             distribution="bernoulli",
             cv.folds=5,
             n.trees=3000,
             verbose=FALSE)
best.iter=gbm.perf(gbm.spam,method="cv")
yhat.gbm=ifelse(predict.gbm(gbm.spam,newdata=gbm.test,n.trees=best.iter)>0,1,0)
gbm.error=mean(ifelse(yhat.gbm!=gbm.test$spam,1,0))
gbm.error

#CART
library(tree)
tc=tree.control(dim(data.train)[1],mincut=1,minsize=2,mindev=0)
tree.spam=tree(spam~.,data=data.train,control=tc)
cv.spam=cv.tree(tree.spam,FUN=prune.misclass,K=10)
prune.spam=prune.misclass(tree.spam,best=cv.spam$size[which.min(cv.spam$dev)])
yhat.cart=predict(prune.spam,data.test,type="class")
cart.error=mean(ifelse(yhat.cart!=data.test$spam,1,0))
cart.error

library(rpart)
rc=rpart.control(minsplit=1,minbucket=1,xval=10,cp=0)
rpart.spam=rpart(spam~.,data=data.train,method="class",control=rc)
xpred.spam=as.data.frame(xpred.rpart(rpart.spam,xval=10))
err=rep(0,dim(xpred.spam)[2])
for(i in 1:length(err)){
  err[i]=mean(ifelse(rpart.spam$y!=xpred.spam[,i],1,0))
}
prune.rpart.spam=prune.rpart(rpart.spam,cp=as.numeric(names(xpred.spam))[which.min(err)])
yhat.rpart=predict(prune.rpart.spam,newdata=data.test,type="class")
rpart.error=mean(ifelse(yhat.rpart!=data.test$spam,1,0))
rpart.error

pred.raw=predict(rpart.spam,newdata=data.test,type="class")
raw.error=mean(ifelse(pred.raw!=data.test$spam,1,0))
raw.error

#MARS
library(earth)
mars.spam=earth(spam~.,data.train,degree=10)
yhat.mars=predict(mars.spam,newdata=data.test,type="class")
mars.error=mean(ifelse(yhat.mars!=data.test$spam,1,0))
mars.error

#additive logistic regression
library(gam)
gam.train=data.train
gam.test=data.test
for(i in 1:57){
  gam.train[,i]=s(log(data.train[,i]+0.1),4)
  gam.test[,i]=s(log(data.test[,i]+0.1),4)
}
logit.spam=gam(spam~.,family=binomial,data=gam.train)
yhat.logit=ifelse(predict.gam(logit.spam,newdata=gam.test)>0,"spam","email")
logit.error=mean(ifelse(yhat.logit!=gam.test$spam,1,0))
logit.error