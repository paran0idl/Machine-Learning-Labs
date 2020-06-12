library(openxlsx)
data<-read.xlsx("material/spambase.xlsx")
n <- dim(data)[1]
set.seed(12345)
id <- sample(1:n, floor(n*0.5))
train <- data[id,]
test <- data[-id,]

logit<-glm(Spam~.,data=train,family = "binomial")

pre<-predict(logit,test,type="response")
pre_class<-as.integer(pre>0.5)
#pre_class<-ifelse(pre>0.5, 1, 0)

cft<-table(Pred=pre_class,Actual=test$Spam)
cft
tp <- cft[2, 2]
tn <- cft[1, 1]
fp <- cft[2, 1]
fn <- cft[1, 2]
print(accuracy <- 1-((tp + tn)/(tp + tn + fp + fn)))




library(kknn)
r<-kknn(Spam~.,train=train,test=test,k=30)
fit<-fitted(r)
#fit
fit <- ifelse(fit>0.5, 1, 0)
#fit<-as.integer(fit>0.5)
#table(fit,test$Spam)
cft<-table(fit,test$Spam)
cft
tp <- cft[2, 2]
tn <- cft[1, 1]
fp <- cft[2, 1]
fn <- cft[1, 2]
print(accuracy <- 1-(tp + tn)/(tp + tn + fp + fn))

r<-kknn(Spam~.,train=train,test=test,k=1)
fit<-fitted(r)
#fit
fit <- ifelse(fit>0.5, 1, 0)
#fit<-as.integer(fit>0.5)
#table(fit,test$Spam)
cft<-table(fit,test$Spam)
cft
tp <- cft[2, 2]
tn <- cft[1, 1]
fp <- cft[2, 1]
fn <- cft[1, 2]
print(accuracy <- 1-(tp + tn)/(tp + tn + fp + fn))