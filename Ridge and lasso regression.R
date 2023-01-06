data=read.csv("DataCorr.csv",header=T)
d=data[,2:10]

#install.packages("caret")   #Need to install lava and stringi packages along with caret    
#install.packages("mlbench")
#install.packages("glmnet")
library(caret)
library(mlbench)
library(psych)
library(glmnet)
library(MASS)

attach(d)
y=d$PRECTOTCORR[-1]           #PRECTOTCORR
x1=d$TS[-1]                   #TS
x2=d$PS[-1]                   #PS
x3=d$T2MDEW[-1]               #T2MDEW
x4=d$T2MWET[-1]               #T2MWET
x5=d$QV2M[-1]                 #QV2M
x6=d$RH2M[-1]                 #RH2M
x7=d$T2M[-1]                  #T2M
x8=d$WS2M[-1]                  #WS2M
x=cbind(x1,x2,x3,x4,x5,x6,x7,x8)

ind=sample(2,nrow(d),replace=TRUE,prob=c(0.7,0.3))
train=d[ind==1,]
nrow(train)
test=d[ind==2,]
ytrain=train$PRECTOTCORR
custom=trainControl(method="repeatedcv",number=10,repeats=5,verboseIter=T)
lm=train(PRECTOTCORR[-1]~TS[-1]+PS[-1]+T2MDEW[-1]+T2MWET[-1]+QV2M[-1]+RH2M[-1]+T2M[-1]+WS2M[-1],train,method="lm",trControl=custom,na.rm=TRUE)
lm$results
lm
summary(lm)
ft=fitted(lm)
plot(lm$finalModel)


# Ridge regression
ridge=train(PRECTOTCORR[-1]~TS[-1]+PS[-1]+T2MDEW[-1]+T2MWET[-1]+QV2M[-1]+RH2M[-1]+T2M[-1]+WS2M[-1],train,method="glmnet",tuneGrid=expand.grid(alpha=0,lambda=seq(0.001,1,length=15)),trControl=custom)
plot(ridge)
ridge
plot(ridge$finalModel,xvar="lambda",label=T)
plot(ridge$finalModel,xvar="dev",label=T)
plot(varImp(ridge,scale=F))

# Lasso regression
lasso=train(PRECTOTCORR[-1]~TS[-1]+PS[-1]+T2MDEW[-1]+T2MWET[-1]+QV2M[-1]+RH2M[-1]+T2M[-1]+WS2M[-1],train,method="glmnet",tuneGrid=expand.grid(alpha=1,lambda=seq(0.001,1,length=10)),trControl=custom)
plot(lasso)
lasso
plot(lasso$finalModel,xvar="lambda",label=T)
plot(lasso$finalModel,xvar="dev",label=T)
plot(varImp(lasso,scale=F))

#Elastic net regression
ELN=train(PRECTOTCORR[-1]~TS[-1]+PS[-1]+T2MDEW[-1]+T2MWET[-1]+QV2M[-1]+RH2M[-1]+T2M[-1]+WS2M[-1],train,method="glmnet",tuneGrid=expand.grid(alpha=seq(0,1,length=15),lambda=seq(0.001,1,length=10)),trControl=custom)
plot(ELN)
ELN
plot(ELN$finalModel,xvar="lambda",label=T)
plot(ELN$finalModel,xvar="dev",label=T)
plot(varImp(ELN,scale=F))




