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
getSymbols("^N100",src = 'yahoo', from=d1,to=d2)
N100_10d<-as.data.frame(N100)
dim(N100_10d)


source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Preliminary analysis/N100 Preliminary Analysis.R")

N100_10d_returns<-as.data.frame(diff(log(N100_10d$N100.Close)))
N100_lag_Volume<-as.data.frame(N100_10d[,5])
N100_10d<-as.data.frame(cbind(N100_10d[2:nrow(N100_10d),4],N100_10d_returns,
                              N100_10d[2:nrow(N100_10d),5]))
colnames(N100_10d)<-c('Close','Return','Volume')
N100_10d

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

N100_lag_Volume
str(N100_lag_Volume)
N100_old_volume<- as.data.frame(N1001[,4])
str(N100_old_volume)
N100_old_volume[N100_old_volume==0]<-NA
names(N100_lag_Volume)<-names(N100_old_volume)
N100_new_volume<-rbind(N100_lag_Volume,N100_old_volume)
N100_new_volume<-diff(log(N100_new_volume$`N1001[, 4]`))
N100_new_volume<-as.data.frame(N100_new_volume)
N100_new_volume_return<-N100_new_volume[1:nrow(N1001),1]
N100_new_volume_return<-as.data.frame(N100_new_volume_return)

for (i in 1:nrow(N1001)) {
  N1001[i,4]=N100_new_volume_return[i,1]
}

N1001

# 9 day lagged returns

N1001<-cbind(N1001,N1001$N1001_return)
N100_normal_returns<-as.data.frame(N1001[,5])
N100_10d_returns<-as.data.frame(N100_10d_returns[,1])
names(N100_10d_returns)<-names(N100_normal_returns)
N100_lag_returns_all<-rbind(N100_10d_returns,N100_normal_returns)
str(N100_lag_returns_all)
dim(N100_lag_returns_all)
dim(N1001)


for (i in 1:nrow(N1001)) {
  N1001[i,5]=N100_lag_returns_all[i,1]
}

dim(N1001)
colnames(N100_lag_returns_all)<-'Lag return'

# merge with Brent by date
Brent_returns<-rbind(0,Brent_returns)
Brent_returns<-as.data.frame(Brent_returns)
Brent<- cbind(Brent[10:nrow(Brent),],Brent_returns$Brent_returns)
str(Brent)
format(Brent$Date,"%d/%m/%Y")
str(Brent)
which(is.na(Brent))
nrow(Brent)
nrow(N1001)
colnames(N1001)<-c('Date','Close','Return','Volume','Lag Return')
colnames(Brent)<-c('Date','Close','Return')
head(N1001)
head(Brent)
N100_Brent<-merge(N1001,Brent,by="Date")
N100_Brent  
str(N100_Brent)
colnames(N100_Brent)<- c('Date','Close N100','Return N100','Lag Vol','Lag Return','Close Brent','Return Brent')
head(N100_Brent)
which(is.na(N100_Brent))
N100_Brent[is.na(N100_Brent)]<-0
which(is.na(N100_Brent))

#merge with Corn by date
Corn_returns<- as.data.frame(Corn_returns)
Corn_returns<-rbind(0,Corn_returns)
Corn<- cbind(Corn[10:nrow(Corn),],Corn_returns$Corn_returns)
str(Corn)
format(Corn$Date,"%d/%m/%Y")
str(Corn)
which(is.na(Corn))
nrow(Corn)
nrow(N1001)
colnames(Corn)<-c('Date','Close','Return')
N100_Brent_Corn<-merge(N100_Brent,Corn,by="Date")
str(N100_Brent_Corn)
colnames(N100_Brent_Corn)<- c('Date','Close N100','Return N100','Lag Vol','Lag Return',
                              'Close Brent','Return Brent',
                              'Close Corn','Return Corn')
str(N100_Brent_Corn)
head(N100_Brent_Corn)
which(is.na(N100_Brent_Corn))
N100_Brent_Corn[is.na(N100_Brent_Corn)]<-0
which(is.na(N100_Brent_Corn))

# N100 Model Data
N100_SVR_Data<-cbind(as.numeric(N100_Brent_Corn$`Return N100`),
                     as.numeric(N100_Brent_Corn$`Lag Return`),
                     as.numeric(N100_Brent_Corn$`Return Brent`),
                     as.numeric(N100_Brent_Corn$`Return Corn`),
                     as.numeric(N100_Brent_Corn$`Lag Vol`))

N100_SVR_Data<-as.data.frame(N100_SVR_Data)
colnames(N100_SVR_Data)<-c('N100 Return','Lag Return','Brent Return','Corn Return','Lag Vol')
head(N100_SVR_Data)
which(is.na(N100_SVR_Data))
N100_SVR_Data[is.na(N100_SVR_Data)]<-0
which(is.na(N100_SVR_Data))
str(N100_SVR_Data)

N100_SVR_Data

# Correlation between variables
cormat<-round(cor(N100_SVR_Data),3)
cormat

melted_cormat<-melt(cormat)
melted_cormat
ggplot(data = melted_cormat,aes(x=Var1,y=Var2,fill=value),
       main='Euronext 100  Variable Correlations')+
  geom_tile() 
