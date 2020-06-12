library(kernlab)
data("spam")

mod1<-ksvm(type~.,spam,kernel="rbfdot",kpar=list(sigma=0.05),C=0.5)
mod2<-ksvm(type~.,spam,kernel="rbfdot",kpar=list(sigma=0.05),C=1)
mod3<-ksvm(type~.,spam,kernel="rbfdot",kpar=list(sigma=0.05),C=5)
x=spam[,-58]
y=spam[,58]

pred=predict(mod3,x)

mod1


cft<-table(pred,y)
mis_rate=1-((cft[1,1]+cft[2,2])/sum(cft))
mis_rate