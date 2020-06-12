data<-read.csv("data/NIRSpectra.csv",header = TRUE,sep=";",dec = ",")
data$Viscosity=c()
res=prcomp(data)
print(res$x)



lambda=res$sdev^2 
#eigenvalues 

#proportion of variation 
lambda=lambda/sum(lambda)*100
library(ggplot2)
df<-data.frame(PC=1:5,va=lambda[1:5])
ggplot(data=df,aes(x=PC,y=va))+geom_bar(stat = "identity")
sum(lambda[1:2])
# 2 PC

ggplot(data=data,aes(x=res$x[,1],y=res$x[,2]))+geom_point()
plot(x=res$x[,1],y=res$x[,2])
text(label=rownames(res$x[,1]))
u=res$rotation
plot(u[,1],main="PCA Traceplot, PC1")
plot(u[,2],main="PCA Traceplot, PC2")

library(fastICA)
set.seed(12345)

IC<-fastICA(data,n.comp = 2)
IC$K
IC$W
new_W<-IC$K%*%IC$W
plot(new_W[,1],main="ICA Traceplot, PC1")
plot(new_W[,2],main="ICA Traceplot, PC2")

plot(IC$S[,1],IC$S[,2])

