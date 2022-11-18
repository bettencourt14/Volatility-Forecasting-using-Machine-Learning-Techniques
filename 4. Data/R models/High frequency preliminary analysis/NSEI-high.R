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
getSymbols("^NSEI",src = 'yahoo', from=d1,to=d2)
NSEI_10d<-as.data.frame(NSEI)
dim(NSEI_10d)


source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Preliminary analysis/NSEI Preliminary Analysis.R")

NSEI_10d_returns<-as.data.frame(diff(log(NSEI_10d$NSEI.Close)))
NSEI_lag_Volume<-as.data.frame(NSEI_10d[,5])
NSEI_10d<-as.data.frame(cbind(NSEI_10d[2:nrow(NSEI_10d),4],NSEI_10d_returns,
                              NSEI_10d[2:nrow(NSEI_10d),5]))
colnames(NSEI_10d)<-c('Close','Return','Volume')
NSEI_10d

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

NSEI_lag_Volume
str(NSEI_lag_Volume)
NSEI_old_volume<- as.data.frame(NSEI1[,4])
str(NSEI_old_volume)
NSEI_old_volume[NSEI_old_volume==0]<-NA
names(NSEI_lag_Volume)<-names(NSEI_old_volume)
NSEI_new_volume<-rbind(NSEI_lag_Volume,NSEI_old_volume)
NSEI_new_volume<-diff(log(NSEI_new_volume$`NSEI1[, 4]`))
NSEI_new_volume<-as.data.frame(NSEI_new_volume)
NSEI_new_volume_return<-NSEI_new_volume[1:nrow(NSEI1),1]
NSEI_new_volume_return<-as.data.frame(NSEI_new_volume_return)

for (i in 1:nrow(NSEI1)) {
  NSEI1[i,4]=NSEI_new_volume_return[i,1]
}

NSEI1

# 9 day lagged returns

NSEI1<-cbind(NSEI1,NSEI1$NSEI1_return)
NSEI_normal_returns<-as.data.frame(NSEI1[,5])
NSEI_10d_returns<-as.data.frame(NSEI_10d_returns[,1])
names(NSEI_10d_returns)<-names(NSEI_normal_returns)
NSEI_lag_returns_all<-rbind(NSEI_10d_returns,NSEI_normal_returns)
str(NSEI_lag_returns_all)
dim(NSEI_lag_returns_all)
dim(NSEI1)


for (i in 1:nrow(NSEI1)) {
  NSEI1[i,5]=NSEI_lag_returns_all[i,1]
}

dim(NSEI1)
colnames(NSEI_lag_returns_all)<-'Lag return'

# merge with Brent by date
Brent_returns<-rbind(0,Brent_returns)
Brent_returns<-as.data.frame(Brent_returns)
Brent<- cbind(Brent[10:nrow(Brent),],Brent_returns$Brent_returns)
str(Brent)
format(Brent$Date,"%d/%m/%Y")
str(Brent)
which(is.na(Brent))
nrow(Brent)
nrow(NSEI1)
colnames(NSEI1)<-c('Date','Close','Return','Volume','Lag Return')
colnames(Brent)<-c('Date','Close','Return')
head(NSEI1)
head(Brent)
NSEI_Brent<-merge(NSEI1,Brent,by="Date")
NSEI_Brent  
str(NSEI_Brent)
colnames(NSEI_Brent)<- c('Date','Close NSEI','Return NSEI','Lag Vol','Lag Return','Close Brent','Return Brent')
head(NSEI_Brent)
which(is.na(NSEI_Brent))
NSEI_Brent[is.na(NSEI_Brent)]<-0
which(is.na(NSEI_Brent))

#merge with Corn by date
Corn_returns<- as.data.frame(Corn_returns)
Corn_returns<-rbind(0,Corn_returns)
Corn<- cbind(Corn[10:nrow(Corn),],Corn_returns$Corn_returns)
str(Corn)
format(Corn$Date,"%d/%m/%Y")
str(Corn)
which(is.na(Corn))
nrow(Corn)
nrow(NSEI1)
colnames(Corn)<-c('Date','Close','Return')
NSEI_Brent_Corn<-merge(NSEI_Brent,Corn,by="Date")
str(NSEI_Brent_Corn)
colnames(NSEI_Brent_Corn)<- c('Date','Close NSEI','Return NSEI','Lag Vol','Lag Return',
                              'Close Brent','Return Brent',
                              'Close Corn','Return Corn')
str(NSEI_Brent_Corn)
head(NSEI_Brent_Corn)
which(is.na(NSEI_Brent_Corn))
NSEI_Brent_Corn[is.na(NSEI_Brent_Corn)]<-0
which(is.na(NSEI_Brent_Corn))

# NSEI Model Data
NSEI_SVR_Data<-cbind(as.numeric(NSEI_Brent_Corn$`Return NSEI`),
                     as.numeric(NSEI_Brent_Corn$`Lag Return`),
                     as.numeric(NSEI_Brent_Corn$`Return Brent`),
                     as.numeric(NSEI_Brent_Corn$`Return Corn`),
                     as.numeric(NSEI_Brent_Corn$`Lag Vol`))

NSEI_SVR_Data<-as.data.frame(NSEI_SVR_Data)
colnames(NSEI_SVR_Data)<-c('NSEI Return','Lag Return','Brent Return','Corn Return','Lag Vol')
head(NSEI_SVR_Data)
which(is.na(NSEI_SVR_Data))
NSEI_SVR_Data[is.na(NSEI_SVR_Data)]<-0
which(is.na(NSEI_SVR_Data))
str(NSEI_SVR_Data)

NSEI_SVR_Data

# Correlation between variables
cormat<-round(cor(NSEI_SVR_Data),3)
cormat

melted_cormat<-melt(cormat)
melted_cormat
ggplot(data = melted_cormat,aes(x=Var1,y=Var2,fill=value),
       main='NIFTY 50  Variable Correlations')+
  geom_tile() 
