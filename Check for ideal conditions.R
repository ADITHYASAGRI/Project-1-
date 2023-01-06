X=read.csv("data.csv",header=T)
head(X)
attach(X)
y=PRECTOTCORR
x=cbind(TS,PS,T2MDEW,T2MWET,QV2M,RH2M,T2M,WS2M)
n=nrow(x)

### CHECK FOR MULTICOLINEARITY
model1=step(lm(PRECTOTCORR~TS+PS+T2MDEW+T2MWET+QV2M+RH2M+T2M+WS2M,data=X))
summary(model1)
#install.packages("car")
library(car)
a=vif(model1)

### CHECK FOR AUTOCORRELATION 
DW=durbinWatsonTest(model1)
round(cor(X),digits=2)

###Removal of Auto Correlation
beta=solve(t(x)%*%x)%*%t(x)%*%y
ycap=x%*%beta
e=y-ycap
d1=0
for(i in 2:n){
d1[i]=((e[i]-e[i-1])^2)
}
d2=sum(d1)
d3=sum(e^2)
dw=(d2/d3)
SSR=sum((ycap-mean(y))^2)
SSE=sum(e^2)
SST=SSE+SSR
Rsq=SSR/SST
library(Metrics)
RMSE=rmse(y,ycap)

##finding rho
d4=0
for(i in 2:n){
d4[i]=(e[i]*e[i-1])
}
d5=sum(d4)
d6=sum(e^2)
rho=(d5/d6)

##Ystar and xstar after
Ytstar={}
for(i in 2:n){
Ytstar[i]=y[i]-(rho*y[i-1])
}
head(Ytstar)
Xtstar1={}
for(i in 2:n){
Xtstar1[i]=x[i,1]-(rho*x[i-1,1])
}
Xtstar2={}
for(i in 2:n){
Xtstar2[i]=x[i,2]-(rho*x[i-1,2])
}
Xtstar3={}
for(i in 2:n){
Xtstar3[i]=x[i,3]-(rho*x[i-1,3])
}
Xtstar4={}
for(i in 2:n){
Xtstar4[i]=x[i,4]-(rho*x[i-1,4])
}
Xtstar5={}
for(i in 2:n){
Xtstar5[i]=x[i,5]-(rho*x[i-1,5])
}
Xtstar6={}
for(i in 2:n){
Xtstar6[i]=x[i,6]-(rho*x[i-1,6])
}
Xtstar7={}
for(i in 2:n){
Xtstar7[i]=x[i,7]-(rho*x[i-1,7])
}
Xtstar8={}
for(i in 2:n){
Xtstar8[i]=x[i,8]-(rho*x[i-1,8])
}
Xtstar=cbind(Xtstar1,Xtstar2,Xtstar3,Xtstar4,Xtstar5,Xtstar6,Xtstar7,Xtstar8);head(Xtstar)

beta1=solve(t(Xtstar[-1,])%*%Xtstar[-1,])%*%t(Xtstar[-1,])%*%Ytstar[-1]
ycap1=Xtstar[-1,]%*%beta1
e1=Ytstar[-1]-ycap1
d11=0
for(i in 2:n){
d11[i]=((e1[i]-e1[i-1])^2)
}
d21=sum(d11,na.rm=T)
d41=sum(e1^2,na.rm=T)
dw1=(d21/d41);dw1
SSR1=sum((ycap1-mean(Ytstar[-1]))^2);SSR1
SSE1=sum(e1^2);SSE1
SST1=SSE1+SSR1;SST1
Rsq1=SSR1/SST1;Rsq1
RMSE1=rmse(Ytstar[-1],ycap1);RMSE1

## CHECK FOR AUTOCORRELATION AFTER REMOVAL OF AUTOCORRELATION
n1=nrow(Xtstar[-1,]);n1
model2=step(lm(Ytstar[-1]~Xtstar1[-1]+Xtstar2[-1]+Xtstar3[-1]+Xtstar4[-1]+Xtstar5[-1]+Xtstar6[-1]+Xtstar7[-1]+Xtstar8[-1]))
summary(model1)
durbinWatsonTest(model2)

a1=vif(model2)

D=cbind(Ytstar,Xtstar1,Xtstar2,Xtstar3,Xtstar4,Xtstar5,Xtstar6,Xtstar7,Xtstar8);head(D)
#write.csv(D,"DataCorr.csv")


### CHECK FOR HETEROSCEDASTICITY 
library(lmtest)
gqtest(model2,order.by=~Xtstar1[-1]+Xtstar2[-1]+Xtstar3[-1]+Xtstar4[-1]+Xtstar5[-1]+Xtstar6[-1]+Xtstar7[-1]+Xtstar8[-1],fraction=7)




