require(rpart)
housing<-read.table("housing.header.binary.txt", header=T, sep=",")
set.seed(1)
train<-sample(1:nrow(housing),0.75*nrow(housing))
housing.tree<-rpart(Medv~Rm+Lstat,data=housing[train,],method="class")
summary(housing.tree)

housing.pred<-predict(housing.tree,housing[-train, ],type="class")
table(housing[-train, ]$Medv,housing.pred)

housing.prob<-predict(housing.tree,housing[-train, ],type="prob")
housing.tree
housing.prob
housing.prob[,2]
housing.prob.label<-cbind(housing.prob[,2],housing[-train,]$Medv)
housing.prob.label

library(ROCR)
pred<-prediction(housing.prob.label[,1],housing.prob.label[,2])
pred
perf<-performance(pred,"tpr","fpr")
perf
plot(perf,col="red")
abline(0,1,col="lightgray")

auc<-performance(pred,"auc")
auc
auc@y.values[[1]]
