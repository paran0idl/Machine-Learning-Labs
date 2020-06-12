library(openxlsx)
library(ggplot2)

data<-read.xlsx("material/tecator.xlsx")
# n=dim(data)[1]
# set.seed(12345)
# id=sample(1:n,floor(n*0.5))
# train=data[id,]
# test=data[-id,]

# data<-data.frame(Moisture=train$Moisture,Protein=train$Protein)
# plot(data$Moisture,data$Protein)

#2. creating models

my_model <- function(i) {
  data <- data[,103:104]
  form <- Moisture ~ poly(Protein, i)
  mod <- lm(form, data)
  return(mod)
}

#Probabilistic Model
#Model i:
#FINISH THIS

#The Mean Squared Error criterion is appropriate because the high degrees of polynomial fit will lead to  high variance (and low bias) in the estimations. MSE will measure how well fitted (or overfitted) the model is.

#dividing data
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))

#train fit and MSE
train <- data[id,]
y <- train$Moisture
MSE_score_train <- c()

for (i in 1:6) {
  
  y_pred <- predict(my_model(train, i), train)
  MSE_score_train[i] <- mean((y - y_pred)^2) 
  
}


#test fit and MSE
test <- data[-id,]
y <- test$Moisture
MSE_score_test <- c()

for (i in 1:6) {
  
  y_pred <- predict(my_model(train, i), test)
  MSE_score_test[i] <- mean((y - y_pred)^2) 
  
}

df <- data.frame(degree = c(1:6),MSE_score_train, MSE_score_test)
ggplot() +
  geom_point(data = df, aes(x = degree, y = MSE_score_test), color = "blue") +
  geom_point(data = df, aes(x = degree, y = MSE_score_train), color = "red") 
# my_model<-function(data,i){
#   form <- Moisture ~ poly(Protein, i)
#   model<-lm(form,data)
#   return(model)
# }
# train_MSE<-c()
# for(i in 1:6){
#   model<-my_model(data,i)
#   y_pred<-predict(model,train)
#   MSE<-mean((y_pred-train$Moisture)^2)
#   train_MSE<-c(train_MSE,MSE)
# }
# 
# test_MSE<-c()
# for(i in 1:6){
#   model<-my_model(data,i)
#   y_pred<-predict(model,test)
#   MSE<-mean((y_pred-test$Moisture)^2)
#   test_MSE<-c(test_MSE,MSE)
# }
# 
# df1<-data.frame(degree=c(1:6),MSE=train_MSE)
# df2<-data.frame(degree=c(1:6),MSE=test_MSE)
# 
# #ggplot()+geom_point(df1,aes(x=degree,y=MSE),color="blue")
# plot1<-ggplot(df1,aes(x=degree,y=MSE))+geom_point(color="blue")
# plot1<-plot1+geom_point(df2,mapping=aes(x=degree,y=MSE),color="red")
# plot1

library(MASS)
data<-read.xlsx("material/tecator.xlsx")
n_data<-data.frame(Fat=data$Fat)
n_data<-cbind(n_data,data[,2:101])

fit<-lm(Fat~.,data=n_data)
stepAIC(fit,direction = "both",trace = 0)
#step$anova
#summary(step)
coef(step)
length(coef(step))-1

library(glmnet)
covariates=n_data[,-1]
response=n_data[,1]


y=test[,1]
model0=glmnet(as.matrix(covariates), response, alpha=0,family="gaussian")
plot(model0, xvar="lambda", label=TRUE)

model1=glmnet(as.matrix(covariates), response, alpha=1,family="gaussian")
plot(model1, xvar="lambda", label=TRUE)


# for(i in 1:dim(model$beta)[2]){
#   as.matrix(model$beta[,i])
#   ypred=as.matrix(test[,-1])%*%as.matrix(model$beta[,i])
#   print(sum((ypred-mean(y))^2)/sum((y-mean(y))^2))
# }
# as.matrix(model$beta[,1])
# ypred=as.matrix(test[,-1])%*%as.matrix(model$beta[,1])
# sum((ypred-mean(y))^2)/sum((y-mean(y))^2)





# MSE_v<-c()
# for(i in c(mod_ridge_lambdas$lambda,0)){
#   mod=glmnet(as.matrix(covariates), response, alpha=0,family="gaussian",lambda = i)
#   pred=predict(mod,as.matrix(covaraites))
#   MSE=mean((pred-response)^2)
#   MSE_v<-c(MSE_v,MSE)
# }
# MSE_v
# d<-predict(mod_ridge,as.matrix(covaraites))
# plot(mod_ridge_lambdas, xvar="lambda", label=TRUE)

mod_lasso=cv.glmnet(as.matrix(covariates), response, alpha=1,family="gaussian")
plot(mod_lasso)
c<-coef(mod_lasso, s = "lambda.min")
length(which(c!=0))-1







