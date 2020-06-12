
#linear regression
mylin=function(X,Y, Xpred){
  X1=cbind(1,X)
  beta=solve(t(X1)%*%X1)%*%t(X1)%*%Y
  Xpred1=cbind(1,Xpred)
 #MISSING: check formulas for linear regression and compute beta
  Res=Xpred1%*%beta
  return(Res)
}

myCV=function(X,Y,Nfolds){
  n=length(Y)
  p=ncol(X)
  set.seed(12345)
  ind=sample(n,n)
  X1=X[ind,]
  Y1=Y[ind]
  sF=floor(n/Nfolds)
  MSE=numeric(2^p-1)
  Nfeat=numeric(2^p-1)
  Features=list()
  curr=0
  
  #we assume 5 features.
  
  for (f1 in 0:1)
    for (f2 in 0:1)
      for(f3 in 0:1)
        for(f4 in 0:1)
          for(f5 in 0:1){
            model= c(f1,f2,f3,f4,f5)
            if (sum(model)==0) next()
            SSE=0
            
            for (k in 1:Nfolds){
              #MISSING: compute which indices should belong to current fold
              current_feat=which(model==1)
			  #MISSING: implement cross-validation for model with features in "model" and iteration i.
              #train_X=X1[((k-1)*9+1):(k*9),]
              #train_X_Pred=train_X[,-current_feat]
              #train_Y=Y1[((k-1)*9+1):(k*9)]
              #return(mylin(train_X,train_Y,train_X_Pred))
              begin_pos=(k-1)*9+1
              if(k==Nfolds){
                end_pos=length(Y1)
              }else{
                end_pos=k*9
              }
              #test_X=X1[((k-1)*9+1):(k*9),-current_feat]
              test_X=X1[begin_pos:end_pos,current_feat]
              #train_X=X1[-((k-1)*9+1):-(k*9),-current_feat]
              train_X=X1[-begin_pos:-end_pos,current_feat]
              #train_Y=Y1[-((k-1)*9+1):-(k*9)]
              train_Y=Y1[-begin_pos:-end_pos]
              
              Ypred=mylin(train_X,train_Y,test_X)
                            
			  #MISSING: Get the predicted values for fold 'k', Ypred, and the original values for folf 'k', Yp.

              #Yp=Y1[((k-1)*9+1):(k*9)]
              Yp=Y1[begin_pos:end_pos]
              SSE=SSE+sum((Ypred-Yp)^2)
            }
            curr=curr+1
            MSE[curr]=SSE/n
            Nfeat[curr]=sum(model)
            Features[[curr]]=model
          }
  #MISSING: plot MSE against number of features
  library(ggplot2)
  #df<-data.frame(,y=MSE)
  df<-data.frame(number=c(),MSE=c())
  for(i in 1:length(Features)){
    tmp=data.frame(number=sum(Features[[i]]),MSE=MSE[i])
    df=rbind(df,tmp)
  }
  plot1<-ggplot(df,aes(x=number,y=MSE))+geom_point(shape=21)
  #return(plot1)
  i=which.min(MSE)
  return(list(CV=MSE[i], Features=Features[[i]]))
}

myCV(as.matrix(swiss[,2:6]), swiss[[1]], 5)
