RNGversion("3.5.1")
data<-read.csv("data/State.csv",header = TRUE,sep=";",dec = ",")

set.seed(12345)
data<-data[order(data$MET),]
data$MET
library(tree)
tree_mod<-tree(EX~MET,data=data,control = tree.control(minsize = 8,nobs = nrow(data)))

cv.res=cv.tree(tree_mod)
plot(cv.res$size, cv.res$dev, type="b", col="red")



library(boot)
data<-read.csv("data/State.csv",header = TRUE,sep=";",dec = ",")
data<-data[order(data$MET),]

f=function(data,ind){
  data1=data[ind,]
  tree_mod<-tree(EX~MET,data=data1,control = tree.control(minsize = 8,nobs = nrow(data)))
  prunedTree=prune.tree(tree_mod,best=3) 
  pred=predict(prunedTree,data)
  return(pred)
}

boot_res=boot(data,f,R=1000)
CE<-envelope(test,level = 0.95)


tree_mod<-tree(EX~MET,data=data,control = tree.control(minsize = 8,nobs = nrow(data)))
prunedTree=prune.tree(tree_mod,best=3) 
pred=predict(prunedTree,data)

CI_band=data.frame(upper=CE$point[1,],lower=CE$point[2,],EX=data$EX,MET=data$MET,pred=pred)
ggplot(data=CI_band)+geom_point(aes(x=MET,y=EX))+geom_line(aes(x=MET,y=pred),col="blue")+geom_ribbon(aes(x=MET,ymin=lower,ymax=upper),alpha=0.1)


library(boot)
data<-read.csv("data/State.csv",header = TRUE,sep=";",dec = ",")
data<-data[order(data$MET),]

tree_mod<-tree(EX~MET,data=data,control = tree.control(minsize = 8,nobs = nrow(data)))
prunedTree=prune.tree(tree_mod,best=3) 

rng<-function(data,mle){
  data1=data.frame(EX=data$EX,MET=data$MET)
  data1$EX=rnorm(nrow(data1$EX),predict(prunedTree,data1),sd())
}






