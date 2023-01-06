x=read.csv("Data.csv",header=T)

#Descriptive Statistics 1
a=summary(x)
mn=sapply(x,mean)
md=sapply(x,median)
sd=sapply(x,sd)
min=sapply(x,min)
max=sapply(x,max)
range=sapply(x,range)
library(plotrix)
se=sapply(x,std.error)
var=sapply(x,var);var
dis=data.frame("Mean"=mn,"Median"=md,"Min"=min,"Max"=max,"Standard deviation"=sd,"Standard error"=se);dis

#Descriptive Statistics 2
library(Hmisc)
des=describe(x);des

#Descriptive Statistics 3
#install.packages("pastecs")
library(pastecs)
des1=stat.desc(x);des1

#Descriptive Statistics 4
#install.packages("psych")
library(psych)
psych::describe(x)

