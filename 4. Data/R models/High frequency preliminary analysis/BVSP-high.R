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
getSymbols("^BVSP",src = 'yahoo', from=d1,to=d2)
BVSP_10d<-as.data.frame(BVSP)
dim(BVSP_10d)


source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Preliminary analysis/BVSP Preliminary Analysis.R")

BVSP_10d_returns<-as.data.frame(diff(log(BVSP_10d$BVSP.Close)))
BVSP_lag_Volume<-as.data.frame(BVSP_10d[,5])
BVSP_10d<-as.data.frame(cbind(BVSP_10d[2:nrow(BVSP_10d),4],BVSP_10d_returns,
                              BVSP_10d[2:nrow(BVSP_10d),5]))
colnames(BVSP_10d)<-c('Close','Return','Volume')
BVSP_10d

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

BVSP_lag_Volume
str(BVSP_lag_Volume)
BVSP_old_volume<- as.data.frame(BVSP1[,4])
str(BVSP_old_volume)
BVSP_old_volume[BVSP_old_volume==0]<-NA
names(BVSP_lag_Volume)<-names(BVSP_old_volume)
BVSP_new_volume<-rbind(BVSP_lag_Volume,BVSP_old_volume)
BVSP_new_volume<-diff(log(BVSP_new_volume$`BVSP1[, 4]`))
BVSP_new_volume<-as.data.frame(BVSP_new_volume)
BVSP_new_volume_return<-BVSP_new_volume[1:nrow(BVSP1),1]
BVSP_new_volume_return<-as.data.frame(BVSP_new_volume_return)

for (i in 1:nrow(BVSP1)) {
  BVSP1[i,4]=BVSP_new_volume_return[i,1]
}

BVSP1

# 9 day lagged returns

BVSP1<-cbind(BVSP1,BVSP1$BVSP1_return)
BVSP_normal_returns<-as.data.frame(BVSP1[,5])
BVSP_10d_returns<-as.data.frame(BVSP_10d_returns[,1])
names(BVSP_10d_returns)<-names(BVSP_normal_returns)
BVSP_lag_returns_all<-rbind(BVSP_10d_returns,BVSP_normal_returns)
str(BVSP_lag_returns_all)
dim(BVSP_lag_returns_all)
dim(BVSP1)


for (i in 1:nrow(BVSP1)) {
  BVSP1[i,5]=BVSP_lag_returns_all[i,1]
}

dim(BVSP1)
colnames(BVSP_lag_returns_all)<-'Lag return'

# merge with Brent by date
Brent_returns<-rbind(0,Brent_returns)
Brent_returns<-as.data.frame(Brent_returns)
Brent<- cbind(Brent[10:nrow(Brent),],Brent_returns$Brent_returns)
str(Brent)
format(Brent$Date,"%d/%m/%Y")
str(Brent)
which(is.na(Brent))
nrow(Brent)
nrow(BVSP1)
colnames(BVSP1)<-c('Date','Close','Return','Volume','Lag Return')
colnames(Brent)<-c('Date','Close','Return')
head(BVSP1)
head(Brent)
BVSP_Brent<-merge(BVSP1,Brent,by="Date")
BVSP_Brent  
str(BVSP_Brent)
colnames(BVSP_Brent)<- c('Date','Close BVSP','Return BVSP','Lag Vol','Lag Return','Close Brent','Return Brent')
head(BVSP_Brent)
which(is.na(BVSP_Brent))
BVSP_Brent[is.na(BVSP_Brent)]<-0
which(is.na(BVSP_Brent))

#merge with Corn by date
Corn_returns<- as.data.frame(Corn_returns)
Corn_returns<-rbind(0,Corn_returns)
Corn<- cbind(Corn[10:nrow(Corn),],Corn_returns$Corn_returns)
str(Corn)
format(Corn$Date,"%d/%m/%Y")
str(Corn)
which(is.na(Corn))
nrow(Corn)
nrow(BVSP1)
colnames(Corn)<-c('Date','Close','Return')
BVSP_Brent_Corn<-merge(BVSP_Brent,Corn,by="Date")
str(BVSP_Brent_Corn)
colnames(BVSP_Brent_Corn)<- c('Date','Close BVSP','Return BVSP','Lag Vol','Lag Return',
                              'Close Brent','Return Brent',
                              'Close Corn','Return Corn')
str(BVSP_Brent_Corn)
head(BVSP_Brent_Corn)
which(is.na(BVSP_Brent_Corn))
BVSP_Brent_Corn[is.na(BVSP_Brent_Corn)]<-0
which(is.na(BVSP_Brent_Corn))

# BVSP Model Data
BVSP_SVR_Data<-cbind(as.numeric(BVSP_Brent_Corn$`Return BVSP`),
                     as.numeric(BVSP_Brent_Corn$`Lag Return`),
                     as.numeric(BVSP_Brent_Corn$`Return Brent`),
                     as.numeric(BVSP_Brent_Corn$`Return Corn`),
                     as.numeric(BVSP_Brent_Corn$`Lag Vol`))

BVSP_SVR_Data<-as.data.frame(BVSP_SVR_Data)
colnames(BVSP_SVR_Data)<-c('BVSP Return','Lag Return','Brent Return','Corn Return','Lag Vol')
head(BVSP_SVR_Data)
which(is.na(BVSP_SVR_Data))
BVSP_SVR_Data[is.na(BVSP_SVR_Data)]<-0
which(is.na(BVSP_SVR_Data))
str(BVSP_SVR_Data)

BVSP_SVR_Data

# Correlation between variables
cormat<-round(cor(BVSP_SVR_Data),3)
cormat

melted_cormat<-melt(cormat)
melted_cormat
ggplot(data = melted_cormat,aes(x=Var1,y=Var2,fill=value),
       main='IBOVESPA  Variable Correlations')+
  geom_tile() 

# Linear Regression
BVSP_lr<-lm(BVSP_SVR_Data$`BVSP Return`~
              BVSP_SVR_Data$`Lag Return`+
              BVSP_SVR_Data$`Brent Return`+
              BVSP_SVR_Data$`Lag Vol`+
              BVSP_SVR_Data$`Corn Return`,
            BVSP_SVR_Data)

BVSP_lr_predicted<-predict(BVSP_lr,BVSP_SVR_Data)

# Support Vector Regression

BVSP_SVR<-svm(BVSP_SVR_Data$`BVSP Return`~
                BVSP_SVR_Data$`Lag Return`+
                BVSP_SVR_Data$`Brent Return`+
                BVSP_SVR_Data$`Corn Return`+
                BVSP_SVR_Data$`Lag Vol`,
              BVSP_SVR_Data)
BVSP_SVR_predicted<- predict(BVSP_SVR,BVSP_SVR_Data)



############################################################################################
##############################################################################################
############################## BVSP Forecast values for period under analysis #####################

# test data

vol_t<-nrow(BVSP_new_volume_return)-(nrow(BVSP2)-1)
brent_t<-nrow(Brent_returns_later)-(nrow(BVSP2)-1)
corn_t<-nrow(Corn_returns_later)-(nrow(BVSP2)-1)
lag_return_t<-nrow(BVSP_lag_returns_all)-(nrow(BVSP2)-1)-9
lag_return_t1<-nrow(BVSP_lag_returns_all)-(nrow(BVSP2))
return_t<-nrow(BVSP1)-(nrow(BVSP2)-1)
BVSP_forecast_vol<- BVSP_new_volume_return[vol_t:nrow(BVSP_new_volume_return),1]
BVSP_forecast_Brent_Return<-Brent_returns_later[brent_t:nrow(Brent_returns_later),1]
BVSP_forecast_Corn_Return<- Corn_returns_later[corn_t:nrow(Corn_returns_later),1]
BVSP_forecast_lag_return<-BVSP_lag_returns_all[lag_return_t:lag_return_t1,1]
BVSP_forecast_retuns<- as.numeric(BVSP1[return_t:nrow(BVSP1),3])

BVSP_SVR_forecast_Data<-cbind(BVSP_forecast_retuns,BVSP_forecast_lag_return,BVSP_forecast_Brent_Return,
                              BVSP_forecast_Corn_Return,BVSP_forecast_vol)
BVSP_SVR_forecast_Data<-as.data.frame(BVSP_SVR_forecast_Data)

head(BVSP_SVR_forecast_Data)
dim(BVSP_SVR_forecast_Data)
colnames(BVSP_SVR_forecast_Data)<-c('BVSP Return','Lag Return','Brent Return', 'Corn Return','Lag Vol')

BVSP_SVR_total_Data<-rbind(BVSP_SVR_Data,BVSP_SVR_forecast_Data)
which(is.na(BVSP_SVR_total_Data))
BVSP_SVR_forecast_Data[is.na(BVSP_SVR_forecast_Data)]<-0
BVSP_SVR_total_Data[is.na(BVSP_SVR_total_Data)]<-0
tail(BVSP_SVR_total_Data)

# tunning up the Support vector model
names(BVSP_SVR_forecast_Data)=names(BVSP_SVR_total_Data)

