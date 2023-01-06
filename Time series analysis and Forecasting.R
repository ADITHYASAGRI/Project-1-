data=read.csv("DataCorr.csv",header=T)
d=data[,2:10]
attach(d)


### Surface Temperature
y=TS[13:492];head(y)
plot.ts(TS,main='Time profile for surface temperature',ylab='Surface Temperature')
n=length(y);n

train=y[1:468]
n1=length(train);n1
test=y[469:480];test
ys=ts(train,frequency=12,start=c(1982,1));ys
plot.ts(ys,main="Time profile of Surface temperature",ylab="Surface Temperature")
m=round(2*sqrt(length(ys)));m
acf(c(ys),m,main="ACF of Observed series",na.rm=T)


##Test for trend
#install.packages("Kendall")
library(Kendall)
MannKendall(ys)

##Test for seasonality
s=12
c=39
mx=matrix(ys,s,c)
rn={}
for(j in 1:c){
rn[j]=rank(mx[,j])
}
M=matrix(rn,s,c)
m=rowSums(M)
i={}
for(l in 1:s){
i[l]=(m[l]-(c*(s-1)/2))^2
}
sum(i)
chi=12*sum(i)/(c*s*(s+1));chi
chit=qchisq(0.95,(s-1));chit
if(chi>chit)
cat("We reject Ho and conclude that data contains seasonality")else
cat("We accept Ho and conclude that data doesn't contains seasonality")

##Variate differencing
v=var(ys);v
d1=diff(ys);head(d1)
v1=var(d1);v1
if(v<v1)
cat("ys is stationary")else
cat("ys is not stationary")
d2=diff(d1)
v2=var(d2)
if(v1<v2)
cat("d1 is stationary")else
cat("d1 is not stationary")

plot.ts(d1,main="Plot of Stationary series")
##Augmented Dickey-Fuller Test
library(tseries)
adf.test(d1)

###ARIMA model building
d=1
acf(d1,main="ACF plot for stationary surface temperature")
q=4
pacf(d1,main="PACF plot for stationary surface temperature")
p=3

fit1=arima(train,order=c(3,1,4))
res1=fit1$residuals
k=round(2*sqrt(n1));k
LB1=Box.test(res1,lag=k,type="Ljung-Box")
BP1=Box.test(res1,lag=k,type="Box-Pierce")
SP1=shapiro.test(res1)
IC1=fit1$aic

fit2=arima(train,order=c(3,1,3))
res2=fit2$residuals
k=round(2*sqrt(n1));k
LB2=Box.test(res2,lag=k,type="Ljung-Box")
BP2=Box.test(res2,lag=k,type="Box-Pierce")
SP2=shapiro.test(res2)
IC2=fit2$aic

fit3=arima(train,order=c(3,1,2))
res3=fit3$residuals
k=round(2*sqrt(n1));k
LB3=Box.test(res3,lag=k,type="Ljung-Box")
BP3=Box.test(res3,lag=k,type="Box-Pierce")
SP3=shapiro.test(res3)
IC3=fit3$aic

fit4=arima(train,order=c(3,1,1))
res4=fit4$residuals
k=round(2*sqrt(n1));k
LB4=Box.test(res4,lag=k,type="Ljung-Box")
BP4=Box.test(res4,lag=k,type="Box-Pierce")
SP4=shapiro.test(res4)
IC4=fit4$aic

fit5=arima(train,order=c(2,1,4))
res5=fit5$residuals
k=round(2*sqrt(n1));k
LB5=Box.test(res5,lag=k,type="Ljung-Box")
BP5=Box.test(res5,lag=k,type="Box-Pierce")
SP5=shapiro.test(res5)
IC5=fit5$aic

fit6=arima(train,order=c(2,1,3))
res6=fit6$residuals
k=round(2*sqrt(n1));k
LB6=Box.test(res6,lag=k,type="Ljung-Box")
BP6=Box.test(res6,lag=k,type="Box-Pierce")
SP6=shapiro.test(res6)
IC6=fit6$aic

fit7=arima(train,order=c(2,1,2))
res7=fit7$residuals
k=round(2*sqrt(n1));k
LB7=Box.test(res7,lag=k,type="Ljung-Box")
BP7=Box.test(res7,lag=k,type="Box-Pierce")
SP7=shapiro.test(res7)
IC7=fit7$aic

fit8=arima(train,order=c(2,1,1))
res8=fit8$residuals
k=round(2*sqrt(n1));k
LB8=Box.test(res8,lag=k,type="Ljung-Box")
BP8=Box.test(res8,lag=k,type="Box-Pierce")
SP8=shapiro.test(res8)
IC8=fit8$aic

fit9=arima(train,order=c(1,1,4))
res9=fit9$residuals
k=round(2*sqrt(n1));k
LB9=Box.test(res9,lag=k,type="Ljung-Box")
BP9=Box.test(res9,lag=k,type="Box-Pierce")
SP9=shapiro.test(res9)
IC9=fit9$aic

fit10=arima(train,order=c(1,1,3))
res10=fit10$residuals
k=round(2*sqrt(n1));k
LB10=Box.test(res10,lag=k,type="Ljung-Box")
BP10=Box.test(res10,lag=k,type="Box-Pierce")
SP10=shapiro.test(res10)
IC10=fit10$aic

fit11=arima(train,order=c(1,1,2))
res11=fit11$residuals
k=round(2*sqrt(n1));k
LB11=Box.test(res11,lag=k,type="Ljung-Box")
BP11=Box.test(res11,lag=k,type="Box-Pierce")
SP11=shapiro.test(res11)
IC11=fit11$aic

fit12=arima(train,order=c(1,1,1))
res12=fit12$residuals
k=round(2*sqrt(n1));k
LB12=Box.test(res12,lag=k,type="Ljung-Box")
BP12=Box.test(res12,lag=k,type="Box-Pierce")
SP12=shapiro.test(res12)
IC12=fit12$aic

Model=c(1,2,3,4,5,6,7,8,9,10,11,12)
p=c(3,3,3,3,2,2,2,2,1,1,1,1)
d=c(rep(1,12))
q=c(4,3,2,1,4,3,2,1,4,3,2,1)
LB=c(LB1$p.value,LB2$p.value,LB3$p.value,LB4$p.value,LB5$p.value,LB6$p.value,LB7$p.value,LB8$p.value,LB9$p.value,LB10$p.value,LB11$p.value,LB12$p.value)
BP=c(BP1$p.value,BP2$p.value,BP3$p.value,BP4$p.value,BP5$p.value,BP6$p.value,BP7$p.value,BP8$p.value,BP9$p.value,BP10$p.value,BP11$p.value,BP12$p.value)
SP=c(SP1$p.value,SP2$p.value,SP3$p.value,SP4$p.value,SP5$p.value,SP6$p.value,SP7$p.value,SP8$p.value,SP9$p.value,SP10$p.value,SP11$p.value,SP12$p.value)
IC=c(IC1,IC2,IC3,IC4,IC5,IC6,IC7,IC8,IC9,IC10,IC11,IC12)
df=data.frame("Model"=Model,"p"=p,"d"=d,"q"=q,"LB"=LB,"BP"=BP,"SP"=SP,"AIC"=IC)
cat("\n Residual analysis table is\n")
print(df)
max(SP)
min(IC)
plot(res2,main="Plot of residual series of ARIMA(3,0,3)")

##Model perfomance and Forecast
pre=predict(fit2,n.ahead=36)
prev=pre$pred
cat("\n Predicted values of data are ",prev)
data.frame(test,prev[1:12])
z=as.vector(test);z
z1=as.vector(prev[1:12])
plot(z,type="l",lty=1,col=1,ylim=c(16,32),xlab="Time Period",ylab="Surface Temperature",main="Plot of Actual value and predicted value")
lines(z1,lty=2,col=4)
#install.packages("nnfor")
library(nnfor)
tab=accuracy(prev[1:12],test);tab

forecast=prev[13:36];forecast

### MLP
#install.packages("e1071")
library(e1071)
mlp.fit<-mlp(ys)
mlp.fit
plot(mlp.fit)
print(mlp.fit)
mlp.frc1=forecast(mlp.fit,h=12);mlp.frc1
mlp.frc=forecast(mlp.fit,h=36);mlp.frc
plot(mlp.frc,ylab='Temperature',xlab='Date',main='Forecast using MLP')
tab1=accuracy(mlp.frc1,test);tab1
z2=as.vector(c(mlp.frc1$mean));z2
plot(z,type="l",lty=1,col=1,ylim=c(16,32),xlab="Time Period",ylab="Surface Temperature",main="Plot of Actual value and predicted value")
lines(z2,lty=2,col=4)
legend("topleft",legend=c("Actual obs","Forecasted obs"),lty=c(1,2),col=c(1,4))
data.frame(test,mlp.frc1$mean)

###ELM
elm.fit=elm(ys)
print(elm.fit)
plot(elm.fit)
elm.frc=forecast(elm.fit,h=36);elm.frc
elm.frc1=forecast(elm.fit,h=12);elm.frc1
tab2=accuracy(elm.frc1,test);tab2
plot(elm.frc,ylab='Temperature',xlab='Date',main='Forecast using ELM')
z3=as.vector(c(elm.frc1$mean));z2
plot(z,type="l",lty=1,col=1,ylim=c(16,32),xlab="Time Period",ylab="Surface Temperature",main="Plot of Actual value and predicted value")
lines(z3,lty=2,col=4)
legend("topleft",legend=c("Actual obs","Forecasted obs"),lty=c(1,2),col=c(1,4))
data.frame(test,elm.frc1$mean)


### Specific Humidity 

y1=QV2M[13:492];head(y1)
plot.ts(y1,main='Time profile for Specific Humidity',ylab='Specific Humidity')
n2=length(y1);n2

train1=y1[1:468]
n3=length(train1);n3
test1=y1[469:480];test1
ys1=ts(train1,frequency=12,start=c(1982,1));ys1
plot.ts(ys1,main="Time profile of Specific Humidity",ylab="Specific Humidity")
m1=round(2*sqrt(length(ys1)));m1
acf(c(ys1),m1,main="ACF of Observed series",na.rm=T)

##Test for trend
#install.packages("Kendall")
library(Kendall)
MannKendall(ys1)

##Test for seasonality
s1=12
c1=39
mx1=matrix(ys1,s1,c1)
rn1={}
for(j in 1:c1){
  rn1[j]=rank(mx1[,j])
}
M1=matrix(rn1,s1,c1)
m1=rowSums(M1)
i1={}
for(l in 1:s1){
  i1[l]=(m1[l]-(c1*(s1-1)/2))^2
}
sum(i1)
chi1=12*sum(i1)/(c1*s1*(s1+1));chi1
chit1=qchisq(0.95,(s1-1));chit1
if(chi1>chit1)
  cat("We reject Ho and conclude that data contains seasonality")else
    cat("We accept Ho and conclude that data doesn't contains seasonality")

#Seasonal Differencing
S=12
t1=(S+1):length(ys1)
y1s=ys1[t1]-ys1[t1-S];y1s
plot.ts(y1,main="Plot of seasonal differenced data")

##Test for trend
#install.packages("Kendall")
library(Kendall)
MannKendall(y1s)

plot.ts(y1s,type="l",main="Plot of Stationary series",ylab="Specific Humidity")

##Augmented Dickey-Fuller Test
library(tseries)
adf.test(y1s)

acf(y1s,lag=50,main="ACF plot for stationary specific humidity")
pacf(y1s,lag=50,main="PACF plot for stationary specific humidity")
d1=0;D1=1;P1=3;p1=3;Q1=1;q1=4

f21=arima(train1,order=c(3,0,4),seasonal=list(order=c(3,1,1),period=12))
res21=f21$residuals
LB21=Box.test(res21,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP21=Box.test(res21,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC21=f21$aic

f22=arima(train1,order=c(3,0,4),seasonal=list(order=c(2,1,1),period=12))
res22=f22$residuals
LB22=Box.test(res22,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP22=Box.test(res22,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC22=f22$aic

f23=arima(train1,order=c(3,0,4),seasonal=list(order=c(1,1,1),period=12))
res23=f23$residuals
LB23=Box.test(res23,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP23=Box.test(res23,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC23=f23$aic

f24=arima(train1,order=c(3,0,3),seasonal=list(order=c(3,1,1),period=12))
res24=f24$residuals
LB24=Box.test(res24,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP24=Box.test(res24,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC24=f24$aic


f25=arima(train1,order=c(3,0,3),seasonal=list(order=c(2,1,1),period=12))
res25=f25$residuals
LB25=Box.test(res25,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP25=Box.test(res25,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC25=f25$aic

f26=arima(train1,order=c(3,0,3),seasonal=list(order=c(1,1,1),period=12))
res26=f26$residuals
LB26=Box.test(res26,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP26=Box.test(res26,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC26=f26$aic

f27=arima(train1,order=c(3,0,2),seasonal=list(order=c(3,1,1),period=12))
res27=f27$residuals
LB27=Box.test(res27,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP27=Box.test(res27,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC27=f27$aic

f28=arima(train1,order=c(3,0,2),seasonal=list(order=c(2,1,1),period=12))
res28=f28$residuals
LB28=Box.test(res28,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP28=Box.test(res28,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC28=f28$aic

f29=arima(train1,order=c(3,0,2),seasonal=list(order=c(1,1,1),period=12))
res29=f29$residuals
LB29=Box.test(res29,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP29=Box.test(res29,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC29=f29$aic

f210=arima(train1,order=c(3,0,1),seasonal=list(order=c(3,1,1),period=12))
res210=f210$residuals
LB210=Box.test(res210,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP210=Box.test(res210,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC210=f210$aic

f211=arima(train1,order=c(3,0,1),seasonal=list(order=c(2,1,1),period=12))
res211=f211$residuals
LB211=Box.test(res211,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP211=Box.test(res211,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC211=f211$aic

f212=arima(train1,order=c(3,0,1),seasonal=list(order=c(1,1,1),period=12))
res212=f212$residuals
LB212=Box.test(res212,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP212=Box.test(res212,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC212=f212$aic

f213=arima(train1,order=c(2,0,4),seasonal=list(order=c(3,1,1),period=12))
res213=f213$residuals
LB213=Box.test(res213,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP213=Box.test(res213,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC213=f213$aic

f214=arima(train1,order=c(2,0,4),seasonal=list(order=c(2,1,1),period=12))
res214=f214$residuals
LB214=Box.test(res214,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP214=Box.test(res214,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC214=f214$aic

f215=arima(train1,order=c(2,0,4),seasonal=list(order=c(1,1,1),period=12))
res215=f215$residuals
LB215=Box.test(res215,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP215=Box.test(res215,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC215=f215$aic

f216=arima(train1,order=c(2,0,3),seasonal=list(order=c(3,1,1),period=12))
res216=f216$residuals
LB216=Box.test(res216,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP216=Box.test(res216,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC216=f216$aic

f217=arima(train1,order=c(2,0,3),seasonal=list(order=c(2,1,1),period=12))
res217=f217$residuals
LB217=Box.test(res217,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP217=Box.test(res217,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC217=f217$aic

f218=arima(train1,order=c(2,0,3),seasonal=list(order=c(1,1,1),period=12))
res218=f218$residuals
LB218=Box.test(res218,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP218=Box.test(res218,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC218=f218$aic

f219=arima(train1,order=c(2,0,2),seasonal=list(order=c(3,1,1),period=12))
res219=f219$residuals
LB219=Box.test(res219,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP219=Box.test(res219,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC219=f219$aic

f220=arima(train1,order=c(2,0,2),seasonal=list(order=c(2,1,1),period=12))
res220=f220$residuals
LB220=Box.test(res220,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP220=Box.test(res220,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC220=f220$aic

f221=arima(train1,order=c(2,0,2),seasonal=list(order=c(1,1,1),period=12))
res221=f221$residuals
LB221=Box.test(res221,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP221=Box.test(res221,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC221=f221$aic

f222=arima(train1,order=c(2,0,1),seasonal=list(order=c(3,1,1),period=12))
res222=f222$residuals
LB222=Box.test(res222,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP222=Box.test(res222,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC222=f222$aic

f223=arima(train1,order=c(2,0,1),seasonal=list(order=c(2,1,1),period=12))
res223=f223$residuals
LB223=Box.test(res223,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP223=Box.test(res223,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC223=f223$aic

f224=arima(train1,order=c(2,0,1),seasonal=list(order=c(1,1,1),period=12))
res224=f224$residuals
LB224=Box.test(res224,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP224=Box.test(res224,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC224=f224$aic

f225=arima(train1,order=c(1,0,4),seasonal=list(order=c(3,1,1),period=12))
res225=f225$residuals
LB225=Box.test(res225,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP225=Box.test(res225,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC225=f225$aic

f226=arima(train1,order=c(1,0,4),seasonal=list(order=c(2,1,1),period=12))
res226=f226$residuals
LB226=Box.test(res226,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP226=Box.test(res226,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC226=f226$aic

f227=arima(train1,order=c(1,0,4),seasonal=list(order=c(1,1,1),period=12))
res227=f227$residuals
LB227=Box.test(res227,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP227=Box.test(res227,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC227=f227$aic

f228=arima(train1,order=c(1,0,3),seasonal=list(order=c(3,1,1),period=12))
res228=f228$residuals
LB228=Box.test(res228,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP228=Box.test(res228,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC228=f228$aic

f229=arima(train1,order=c(1,0,3),seasonal=list(order=c(2,1,1),period=12))
res229=f229$residuals
LB229=Box.test(res229,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP229=Box.test(res229,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC229=f229$aic

f230=arima(train1,order=c(1,0,3),seasonal=list(order=c(1,1,1),period=12))
res230=f230$residuals
LB230=Box.test(res230,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP230=Box.test(res230,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC230=f230$aic

f231=arima(train1,order=c(1,0,2),seasonal=list(order=c(3,1,1),period=12))
res231=f231$residuals
LB231=Box.test(res231,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP231=Box.test(res231,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC231=f231$aic

f232=arima(train1,order=c(1,0,2),seasonal=list(order=c(2,1,1),period=12))
res232=f232$residuals
LB232=Box.test(res232,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP232=Box.test(res232,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC232=f232$aic

f233=arima(train1,order=c(1,0,2),seasonal=list(order=c(1,1,1),period=12))
res233=f233$residuals
LB233=Box.test(res233,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP233=Box.test(res233,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC233=f233$aic

f234=arima(train1,order=c(1,0,1),seasonal=list(order=c(3,1,1),period=12))
res234=f234$residuals
LB234=Box.test(res234,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP234=Box.test(res234,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC234=f234$aic

f235=arima(train1,order=c(1,0,1),seasonal=list(order=c(2,1,1),period=12))
res235=f235$residuals
LB235=Box.test(res235,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP235=Box.test(res235,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC235=f235$aic

f236=arima(train1,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=12))
res236=f236$residuals
LB236=Box.test(res236,lag=round(2*sqrt(length(train1))),fitdf=2,type="Ljung-Box")
BP236=Box.test(res236,lag=round(2*sqrt(length(train1))),fitdf=2,type="Box-Pierce")
IC236=f236$aic



model1=c(seq(1:36))
p1=c(rep(3,12),rep(2,12),rep(1,12));p1
d1=c(rep(0,36))
q1=c(rep(4,3),rep(3,3),rep(2,3),rep(1,3),rep(4,3),rep(3,3),rep(2,3),rep(1,3),rep(4,3),rep(3,3),rep(2,3),rep(1,3));q1
P1=c(rep(c(3,2,1),12));P1
D1=c(rep(1,36))
Q1=c(rep(1,36));Q1
LB1=c(LB21$p.value,LB22$p.value,LB23$p.value,LB24$p.value,LB25$p.value,LB26$p.value,LB27$p.value,LB28$p.value,LB29$p.value,LB210$p.value,LB211$p.value,LB212$p.value,LB213$p.value,LB214$p.value,LB215$p.value,LB216$p.value,LB217$p.value,LB218$p.value,LB219$p.value,LB220$p.value,LB221$p.value,LB222$p.value,LB223$p.value,LB224$p.value,LB225$p.value,LB226$p.value,LB227$p.value,LB228$p.value,LB229$p.value,LB230$p.value,LB231$p.value,LB232$p.value,LB233$p.value,LB234$p.value,LB235$p.value,LB236$p.value)
BP1=c(BP21$p.value,BP22$p.value,BP23$p.value,BP24$p.value,BP25$p.value,BP26$p.value,BP27$p.value,BP28$p.value,BP29$p.value,BP210$p.value,BP211$p.value,BP212$p.value,BP213$p.value,BP214$p.value,BP215$p.value,BP216$p.value,BP217$p.value,BP218$p.value,BP219$p.value,BP220$p.value,BP221$p.value,BP222$p.value,BP223$p.value,BP224$p.value,BP225$p.value,BP226$p.value,BP227$p.value,BP228$p.value,BP229$p.value,BP230$p.value,BP231$p.value,BP232$p.value,BP233$p.value,BP234$p.value,BP235$p.value,BP236$p.value)
IC1=c(IC21,IC22,IC23,IC24,IC25,IC26,IC27,IC28,IC29,IC210,IC211,IC212,IC213,IC214,IC215,IC216,IC217,IC218,IC219,IC220,IC221,IC222,IC223,IC224,IC225,IC226,IC227,IC228,IC229,IC230,IC231,IC232,IC233,IC234,IC235,IC236)
tab1=data.frame("Model"=model1,"p"=p1,"d"=d1,"q"=q1,"P"=P1,"D"=D1,"Q"=Q1,"LB"=LB1,"BP"=BP1,"IC"=IC1);tab1

max(LB1)
max(BP1)
min(IC1)
plot(res27,main="Plot of residual series of SARIMA((3,0,3),(1,1,2))")

##Model perfomance and Forecast
pre1=predict(f21,n.ahead=36)
prev1=pre1$pred
cat("\n Predicted values of data are ",prev1)
data.frame(test1,prev1[1:12])
zs=as.vector(test1);zs
zs1=as.vector(prev1[1:12])
plot(zs,type="l",lty=1,col=1,ylim=c(8,18),xlab="Time Period",ylab="Specific Humidity",main="Plot of Actual value and predicted value")
lines(zs1,lty=2,col=4)
legend("topleft",legend=c("Actual obs","Forecasted obs"),lty=c(1,2),col=c(1,4))


#install.packages("nnfor")
library(nnfor)
tab1=accuracy(prev1[1:12],test1);tab1

forecast1=prev1[13:36];forecast1

### MLP
#install.packages("e1071")
library(e1071)
mlp.fit2<-mlp(ys1)
mlp.fit2
plot(mlp.fit2)
print(mlp.fit2)
mlp.frc12=forecast(mlp.fit2,h=12);mlp.frc12
mlp.frc2=forecast(mlp.fit2,h=36);mlp.frc2
plot(mlp.frc2,ylab='Specific Humidity',xlab='Date',main='Forecast using MLP')
tab3=accuracy(mlp.frc12,test1);tab3
zs2=as.vector(c(mlp.frc12$mean));zs2
plot(zs,type="l",lty=1,col=1,ylim=c(8,18),xlab="Time Period",ylab="Specific Humidity",main="Plot of Actual value and predicted value")
lines(zs2,lty=2,col=4)
legend("topleft",legend=c("Actual obs","Forecasted obs"),lty=c(1,2),col=c(1,4))
data.frame(test1,mlp.frc12$mean)

###ELM
elm.fit2=elm(ys1)
print(elm.fit2)
plot(elm.fit2)
elm.frc2=forecast(elm.fit2,h=36);elm.frc2
elm.frc12=forecast(elm.fit2,h=12);elm.frc12
tab4=accuracy(elm.frc12,test1);tab4
plot(elm.frc2,ylab='Specific Humidity',xlab='Date',main='Forecast using ELM')
zs3=as.vector(c(elm.frc12$mean));zs3
plot(zs,type="l",lty=1,col=1,ylim=c(8,18),xlab="Time Period",ylab="Specific Humidity",main="Plot of Actual value and predicted value")
lines(zs3,lty=2,col=4)
legend("topleft",legend=c("Actual obs","Forecasted obs"),lty=c(1,2),col=c(1,4))
data.frame(test1,elm.frc12$mean)

### PRECTOTCORR
y2=PRECTOTCORR[13:492];head(y2)
plot.ts(y2,main='Time profile for Precipitation',ylab='Precipitation')
n4=length(y2);n4

train2=y2[1:468]
n5=length(train2);n5
test2=y2[469:480];test2
ys2=ts(train2,frequency=12,start=c(1982,1));ys2
plot.ts(ys2,main="Time profile of PRECTOTCORR",ylab="PRECTOTCORR")
m2=round(2*sqrt(length(ys2)));m2
acf(c(ys2),m2,main="ACF of Observed series",na.rm=T)

##Test for trend
#install.packages("Kendall")
library(Kendall)
MannKendall(ys2)

##Test for seasonality
s2=12
c2=39
mx2=matrix(ys2,s2,c2)
rn2={}
for(j in 1:c2){
  rn2[j]=rank(mx2[,j])
}
M2=matrix(rn2,s2,c2)
m2=rowSums(M2)
i2={}
for(l in 1:s2){
  i2[l]=(m2[l]-(c2*(s2-1)/2))^2
}
sum(i2)
chi2=12*sum(i2)/(c2*s2*(s2+1));chi2
chit2=qchisq(0.95,(s2-1));chit2
if(chi2>chit2)
  cat("We reject Ho and conclude that data contains seasonality")else
    cat("We accept Ho and conclude that data doesn't contains seasonality")

#Seasonal Differencing
S=12
t2=(S+1):length(ys2)
y2s=ys2[t2]-ys2[t2-S];y2s


MannKendall(y2s)

plot.ts(y2s,main="Plot of Stationary Precipitation series")



##Augmented Dickey-Fuller Test
library(tseries)
adf.test(y2s)

acf(y2s,lag=50,main="ACF plot for stationary Precipitation series")
pacf(y2s,lag=100,main="PACF plot for stationary Precipitation series")
p2=1;d2=0;q2=1;P2=4;D2=1;Q2=1

f31=arima(train2,order=c(1,0,1),seasonal=list(order=c(4,1,1),period=12))
res31=f31$residuals
LB31=Box.test(res31,lag=round(2*sqrt(length(train2))),fitdf=2,type="Ljung-Box")
BP31=Box.test(res31,lag=round(2*sqrt(length(train2))),fitdf=2,type="Box-Pierce")
IC31=f31$aic

f32=arima(train2,order=c(1,0,1),seasonal=list(order=c(3,1,1),period=12))
res32=f32$residuals
LB32=Box.test(res32,lag=round(2*sqrt(length(train2))),fitdf=2,type="Ljung-Box")
BP32=Box.test(res32,lag=round(2*sqrt(length(train2))),fitdf=2,type="Box-Pierce")
IC32=f32$aic

f33=arima(train2,order=c(1,0,1),seasonal=list(order=c(2,1,1),period=12))
res33=f33$residuals
LB33=Box.test(res33,lag=round(2*sqrt(length(train2))),fitdf=2,type="Ljung-Box")
BP33=Box.test(res33,lag=round(2*sqrt(length(train2))),fitdf=2,type="Box-Pierce")
IC33=f33$aic

f34=arima(train2,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=12))
res34=f34$residuals
LB34=Box.test(res34,lag=round(2*sqrt(length(train2))),fitdf=2,type="Ljung-Box")
BP34=Box.test(res34,lag=round(2*sqrt(length(train2))),fitdf=2,type="Box-Pierce")
IC34=f34$aic


model2=c(1,2,3,4)
p2=c(rep(1,4));p2
d2=c(rep(0,4))
q2=c(rep(1,4));q2
P2=c(4,3,2,1);P2
D2=c(rep(1,4))
Q2=c(rep(1,4));Q2
LB3p=c(LB31$p.value,LB32$p.value,LB33$p.value,LB34$p.value)
BP3p=c(BP31$p.value,BP32$p.value,BP33$p.value,BP34$p.value)
IC3p=c(IC31,IC32,IC33,IC34)
tab2=data.frame("Model"=model2,"p"=p2,"d"=d2,"q"=q2,"P"=P2,"D"=D2,"Q"=Q2,"LB"=LB3p,"BP"=BP3p,"IC"=IC3p);tab2


plot(res31,main="Plot of residual series of SARIMA((1,0,1),(1,1,2))")

##Model perfomance and Forecast
pre2=predict(f31,n.ahead=36)
prev2=pre2$pred
cat("\n Predicted values of data are ",prev2)

zp=as.vector(test2);zp
zp1=as.vector(prev2[1:12])
plot(zp,type="l",lty=1,col=1,ylim=c(-1,40),xlab="Time Period",ylab="Specific Humidity",main="Plot of Actual value and predicted value")
lines(zp1,lty=2,col=4)
legend("topleft",legend=c("Actual obs","Forecasted obs"),lty=c(1,2),col=c(1,4))
data.frame(test2,prev2[1:12])
#install.packages("nnfor")
library(nnfor)
tab2=accuracy(prev2[1:12],test2);tab2

forecast2=prev2[13:36];forecast2

### MLP
#install.packages("e1071")
library(e1071)
mlp.fit3<-mlp(ys2)
mlp.fit3
plot(mlp.fit3)
print(mlp.fit3)
mlp.frc13=forecast(mlp.fit3,h=12);mlp.frc13
mlp.frc3=forecast(mlp.fit3,h=36);mlp.frc3
plot(mlp.frc3,ylab='PRECTOTCORR',xlab='Date',main='Forecast using MLP')
tab5=accuracy(mlp.frc13,test2);tab5

zp2=as.vector(c(mlp.frc13$mean));zp2
plot(zp,type="l",lty=1,col=1,ylim=c(-1,40),xlab="Time Period",ylab="Precipitation",main="Plot of Actual value and predicted value")
lines(zp2,lty=2,col=4)
legend("topleft",legend=c("Actual obs","Forecasted obs"),lty=c(1,2),col=c(1,4))
data.frame(test2,mlp.frc13$mean)

###ELM
elm.fit3=elm(ys2)
print(elm.fit3)
plot(elm.fit3)
elm.frc3=forecast(elm.fit3,h=36);elm.frc3
elm.frc13=forecast(elm.fit3,h=12);elm.frc13
tab6=accuracy(elm.frc13,test2);tab6
plot(elm.frc3,ylab='PRECTOTCORR',xlab='Date',main='Forecast using ELM')

zp3=as.vector(c(elm.frc13$mean));zp3
plot(zp,type="l",lty=1,col=1,ylim=c(-1,40),xlab="Time Period",ylab="Precipitation",main="Plot of Actual value and predicted value")
lines(zp3,lty=2,col=4)
legend("topleft",legend=c("Actual obs","Forecasted obs"),lty=c(1,2),col=c(1,4))
data.frame(test2,elm.frc13$mean)


















































































