library(readxl)
RNGversion("3.5.1")
data<-read_xls("data/creditscoring.xls")
data$good_bad<-as.factor(data$good_bad)
n=dim(data)[1] 
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=data[id,] 
id1=setdiff(1:n, id) 
set.seed(12345) 
id2=sample(id1, floor(n*0.25)) 
valid=data[id2,] 
id3=setdiff(id1,id2) 
test=data[id3,]

library(tree)

tree_model=tree(good_bad~.,data=train,split = c("deviance"))

pred_train=predict(tree_model,train,type="class")
pred_test=predict(tree_model,test,type="class")

get_misclassfication_rate<-function(pred,actual){
  cft<-table(pred,actual)
  print(cft)
  rate<-(cft[1,2]+cft[2,1])/sum(cft)*100
  return(rate)
}

get_misclassfication_rate(pred_train,train$good_bad)
get_misclassfication_rate(pred_test,test$good_bad)

#summary(tree_model)
# Improvement

trainScore=rep(0,15)
testScore=rep(0,15)
# prunedTree=prune.tree(tree_model,best=4) 
# pred=predict(prunedTree, newdata=valid,type="class") 
# pred
# get_misclassfication_rate(pred,valid$good_bad)

for(i in 2:15){
  prunedTree=prune.tree(tree_model,best=i) 
  pred=predict(prunedTree, newdata=valid, type="tree") 
  #get_misclassfication_rate(pred,valid$good_bad)
  trainScore[i]=deviance(prunedTree) 
  testScore[i]=deviance(pred)
}

library(ggplot2)
ggplot()+geom_point(aes(x=c(2:15),y=trainScore[2:15]),col="red")+
  geom_point(aes(x=c(2:15),y=testScore[2:15]),col="blue")+
  scale_x_discrete(limits=c(2:15))+xlab("leaves")+ylab("score")




# 4
prunedTree=prune.tree(tree_model,best=4)
pred_prunedTree=predict(prunedTree,test)
pred_prunedTree
get_misclassfication_rate(pred_prunedTree,test$good_bad)


library(e1071)
naive_bayes_model=naiveBayes(good_bad~.,train)
pred_bayes_train=predict(naive_bayes_model,train,type="raw")
get_misclassfication_rate(pred_bayes_train,train$good_bad)
pred_bayes_test=predict(naive_bayes_model,test,type="raw")

get_misclassfication_rate(pred_bayes_test,test$good_bad)



# Tree ROC Curve
TPR_tree<-c()
FPR_tree<-c()

mis_rate<-c()
for(pi in seq(0.05,0.95,0.05)){
  res<-ifelse(pred_prunedTree[,2]>pi,1,0)
  actual<-ifelse(test$good_bad=="good",1,0)
  #u=union(res,actual)
  
  factor(res,levels = c(0,1))
  cft<-table(factor(res,levels = c(0,1)),factor(actual))
  TP <- cft[2, 2]
  TN <- cft[1, 1]
  FP <- cft[2, 1]
  FN <- cft[1, 2]
  current_TPR<-TP/(TP+FN)
  current_FPR<-FP/(FP+TN)
  TPR<-c(TPR,current_TPR)
  FPR<-c(FPR,current_FPR)
}
ggplot()+geom_line(aes(x=FPR,y=TPR))+geom_point(aes(x=FPR,y=TPR))


# Naive Bayes
TPR<-c()
FPR<-c()

for(pi in seq(0.05,0.95,0.05)){
  res<-ifelse(pred_bayes_test[,2]>pi,1,0)
  actual<-ifelse(test$good_bad=="good",1,0)
  #u=union(res,actual)
  
  factor(res,levels = c(0,1))
  cft<-table(factor(res,levels = c(0,1)),factor(actual))
  TP <- cft[2, 2]
  TN <- cft[1, 1]
  FP <- cft[2, 1]
  FN <- cft[1, 2]
  current_TPR<-TP/(TP+FN)
  current_FPR<-FP/(FP+TN)
  TPR<-c(TPR,current_TPR)
  FPR<-c(FPR,current_FPR)
}
TPR
FPR
ggplot()+geom_line(aes(x=FPR,y=TPR))+geom_point(aes(x=FPR,y=TPR))
