library(svrpath)
library(svmpath)
library(e1071)
library(quantmod)
library(ggplot2)
library(reshape2)
library(caret)
library(forecast)

## Since this is a predictive method, we need to have date back from 9 days

d1<-as.Date("2014-12-16")
d2<-as.Date("2015-01-01")
getSymbols("^IXIC",src = 'yahoo', from=d1,to=d2)
IXIC_10d<-as.data.frame(IXIC)
dim(IXIC_10d)


source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Preliminary analysis/IXIC Preliminary Analysis.R")

IXIC_10d_returns<-as.data.frame(diff(log(IXIC_10d$IXIC.Close)))
IXIC_lag_Volume<-as.data.frame(IXIC_10d[,5])
IXIC_10d<-as.data.frame(cbind(IXIC_10d[2:nrow(IXIC_10d),4],IXIC_10d_returns,
                              IXIC_10d[2:nrow(IXIC_10d),5]))
colnames(IXIC_10d)<-c('Close','Return','Volume')
IXIC_10d

#Exogenous variables 

Brent <- read_excel("High Frequency Data/Brent.xlsx",col_types = c("date", "numeric"))
Corn <- read_excel("High Frequency Data/Corn.xlsx",col_types = c("date", "numeric"))


Brent_returns<-diff(log(Brent$Price))
Brent<-as.data.frame(Brent)
plot(Brent[,2]~Brent$Date, main='Brent Prices from 01/01/2015 to 18/02/2022',
     xlab='Date', ylab='Price', col='black', type='l')
plot(Brent_returns~Brent$Date[2:nrow(Brent)], main='Brent Returns from 01/01/2015 until 18/02/2022',
     xlab='Date',ylab='Return', type='l', col='black', ylim=c(-0.65,0.65))
abline(h=0,col='red', lty=3,lwd=3)

Corn_returns<-diff(log(Corn$Price))
plot(Corn$Price~Corn$Date, main='Corn Prices from 01/01/2015 to 18/02/2022',
     xlab='Date', ylab='Price', col='black', type='l')
plot(Corn_returns~Corn$Date[2:nrow(Corn)], main='Corn Returns from 01/01/2015 until 18/02/2022',
     xlab='Date',ylab='Return', type='l', col='black', ylim=c(-0.15,0.15))

abline(h=0,col='red', lty=3,lwd=3)

Brent_returns<-as.data.frame(Brent_returns)
Corn_returns<-as.data.frame(Corn_returns)

#lag-them by 9 i

# Brent
nrow(Brent_returns)

a<-Brent_returns
a<-as.data.frame(a)
for (i in 10:nrow(Brent_returns)) {
  a[i,1]<-Brent_returns[i-9,1]
}
a
head(Brent_returns,20)
min(a)
Brent_returns_later<-a
Brent_returns<-as.data.frame(a[10:nrow(a),])
min(Brent_returns)
nrow(Brent_returns)
plot(Brent_returns[,1],type='l')

colnames(Brent_returns)<-'Brent_returns'

# Corn
nrow(Corn_returns)

b<-Corn_returns
b<-as.data.frame(b)
for (i in 10:nrow(Corn_returns)) {
  b[i,1]<-Corn_returns[i-9,1]
}
b

head(Corn_returns,20)
min(b)
Corn_returns_later<-b
Corn_returns<-as.data.frame(b[10:nrow(b),])
min(Corn_returns)
nrow(Corn_returns)
plot(Corn_returns$`b[10:nrow(b), ]`, ylim=c(-0.1,0.1))
colnames(Corn_returns)<-'Corn_returns'

# Volume

IXIC_lag_Volume
str(IXIC_lag_Volume)
IXIC_old_volume<- as.data.frame(IXIC1[,4])
str(IXIC_old_volume)
IXIC_old_volume[IXIC_old_volume==0]<-NA
names(IXIC_lag_Volume)<-names(IXIC_old_volume)
IXIC_new_volume<-rbind(IXIC_lag_Volume,IXIC_old_volume)
IXIC_new_volume<-diff(log(IXIC_new_volume$`IXIC1[, 4]`))
IXIC_new_volume<-as.data.frame(IXIC_new_volume)
IXIC_new_volume_return<-IXIC_new_volume[1:nrow(IXIC1),1]
IXIC_new_volume_return<-as.data.frame(IXIC_new_volume_return)

for (i in 1:nrow(IXIC1)) {
  IXIC1[i,4]=IXIC_new_volume_return[i,1]
}

IXIC1

# 9 day lagged returns

IXIC1<-cbind(IXIC1,IXIC1$IXIC1_return)
IXIC_normal_returns<-as.data.frame(IXIC1[,5])
IXIC_10d_returns<-as.data.frame(IXIC_10d_returns[,1])
names(IXIC_10d_returns)<-names(IXIC_normal_returns)
IXIC_lag_returns_all<-rbind(IXIC_10d_returns,IXIC_normal_returns)
str(IXIC_lag_returns_all)
dim(IXIC_lag_returns_all)
dim(IXIC1)


for (i in 1:nrow(IXIC1)) {
  IXIC1[i,5]=IXIC_lag_returns_all[i,1]
}

dim(IXIC1)
colnames(IXIC_lag_returns_all)<-'Lag return'

# merge with Brent by date
Brent_returns<-rbind(0,Brent_returns)
Brent_returns<-as.data.frame(Brent_returns)
Brent<- cbind(Brent[10:nrow(Brent),],Brent_returns$Brent_returns)
str(Brent)
format(Brent$Date,"%d/%m/%Y")
str(Brent)
which(is.na(Brent))
nrow(Brent)
nrow(IXIC1)
colnames(IXIC1)<-c('Date','Close','Return','Volume','Lag Return')
colnames(Brent)<-c('Date','Close','Return')
head(IXIC1)
head(Brent)
IXIC_Brent<-merge(IXIC1,Brent,by="Date")
IXIC_Brent  
str(IXIC_Brent)
colnames(IXIC_Brent)<- c('Date','Close IXIC','Return IXIC','Lag Vol','Lag Return','Close Brent','Return Brent')
head(IXIC_Brent)
which(is.na(IXIC_Brent))
IXIC_Brent[is.na(IXIC_Brent)]<-0
which(is.na(IXIC_Brent))

#merge with Corn by date
Corn_returns<- as.data.frame(Corn_returns)
Corn_returns<-rbind(0,Corn_returns)
Corn<- cbind(Corn[10:nrow(Corn),],Corn_returns$Corn_returns)
str(Corn)
format(Corn$Date,"%d/%m/%Y")
str(Corn)
which(is.na(Corn))
nrow(Corn)
nrow(IXIC1)
colnames(Corn)<-c('Date','Close','Return')
IXIC_Brent_Corn<-merge(IXIC_Brent,Corn,by="Date")
str(IXIC_Brent_Corn)
colnames(IXIC_Brent_Corn)<- c('Date','Close IXIC','Return IXIC','Lag Vol','Lag Return',
                              'Close Brent','Return Brent',
                              'Close Corn','Return Corn')
str(IXIC_Brent_Corn)
head(IXIC_Brent_Corn)
which(is.na(IXIC_Brent_Corn))
IXIC_Brent_Corn[is.na(IXIC_Brent_Corn)]<-0
which(is.na(IXIC_Brent_Corn))

# IXIC Model Data
IXIC_SVR_Data<-cbind(as.numeric(IXIC_Brent_Corn$`Return IXIC`),
                     as.numeric(IXIC_Brent_Corn$`Lag Return`),
                     as.numeric(IXIC_Brent_Corn$`Return Brent`),
                     as.numeric(IXIC_Brent_Corn$`Return Corn`),
                     as.numeric(IXIC_Brent_Corn$`Lag Vol`))

IXIC_SVR_Data<-as.data.frame(IXIC_SVR_Data)
colnames(IXIC_SVR_Data)<-c('IXIC Return','Lag Return','Brent Return','Corn Return','Lag Vol')
head(IXIC_SVR_Data)
which(is.na(IXIC_SVR_Data))
IXIC_SVR_Data[is.na(IXIC_SVR_Data)]<-0
which(is.na(IXIC_SVR_Data))
str(IXIC_SVR_Data)

IXIC_SVR_Data

# Correlation between variables
cormat<-round(cor(IXIC_SVR_Data),3)
cormat

melted_cormat<-melt(cormat)
melted_cormat
ggplot(data = melted_cormat,aes(x=Var1,y=Var2,fill=value),
       main='NASDAQ  Variable Correlations')+
  geom_tile() 
