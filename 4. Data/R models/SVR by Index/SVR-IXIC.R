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

# Linear Regression
IXIC_lr<-lm(IXIC_SVR_Data$`IXIC Return`~
             IXIC_SVR_Data$`Lag Return`+
             IXIC_SVR_Data$`Brent Return`+
             IXIC_SVR_Data$`Lag Vol`+
             IXIC_SVR_Data$`Corn Return`,
           IXIC_SVR_Data)

IXIC_lr_predicted<-predict(IXIC_lr,IXIC_SVR_Data)

# Support Vector Regression

IXIC_SVR<-svm(IXIC_SVR_Data$`IXIC Return`~
               IXIC_SVR_Data$`Lag Return`+
               IXIC_SVR_Data$`Brent Return`+
               IXIC_SVR_Data$`Corn Return`+
               IXIC_SVR_Data$`Lag Vol`,
             IXIC_SVR_Data)
IXIC_SVR_predicted<- predict(IXIC_SVR,IXIC_SVR_Data)

# tunning up the Support vector model
set.seed(994)

IXIC_tune<-tune(svm,IXIC_SVR_Data$`IXIC Return`~
                 IXIC_SVR_Data$`Lag Return`+
                 IXIC_SVR_Data$`Brent Return`+
                 IXIC_SVR_Data$`Corn Return`+
                 IXIC_SVR_Data$`Lag Vol`,
               data = IXIC_SVR_Data, ranges = list(epsilon=seq(0,0.4,0.1),cost=2^(2:4)))
print(IXIC_tune) # to be used on the Forecasting
plot(IXIC_tune)

IXIC_SVR_best_model<-IXIC_tune$best.model
IXIC_SVR_predicted_best_model<- predict(IXIC_SVR_best_model,IXIC_SVR_Data)

# Charts
plot(IXIC_SVR_Data$`IXIC Return`~IXIC_SVR_Data$`Brent Return`,xlab='Brent Returns',
     ylab='NASDAQ  Returns', main='NASDAQ  vs. Brent Multiple Model Capacity',
     type='p',lwd=3)
points(IXIC_SVR_Data$`IXIC Return`,IXIC_lr_predicted,col='blue',pch=4)
points(IXIC_SVR_Data$`IXIC Return`,IXIC_SVR_predicted,col='red',pch=4)
points(IXIC_SVR_Data$`IXIC Return`,IXIC_SVR_predicted_best_model,col='orange',pch=4)

plot(IXIC_SVR_Data$`IXIC Return`~IXIC_SVR_Data$`Corn Return`,xlab='Corn Returns',
     ylab='NASDAQ  Returns', main='NASDAQ  vs. Corn Multiple Model Capacity',
     type='p',lwd=3)
points(IXIC_SVR_Data$`IXIC Return`,IXIC_lr_predicted,col='blue',pch=4)
points(IXIC_SVR_Data$`IXIC Return`,IXIC_SVR_predicted,col='red',pch=4)
points(IXIC_SVR_Data$`IXIC Return`,IXIC_SVR_predicted_best_model,col='orange',pch=4)

plot(IXIC_SVR_Data$`IXIC Return`~IXIC_SVR_Data$`Lag Vol`,xlab='Lag Vol Returns',
     ylab='NASDAQ  Returns', main='NASDAQ  vs. Lag Vol Multiple Model Capacity',
     type='p',lwd=3)
points(IXIC_SVR_Data$`IXIC Return`,IXIC_lr_predicted,col='blue',pch=4)
points(IXIC_SVR_Data$`IXIC Return`,IXIC_SVR_predicted,col='red',pch=4)
points(IXIC_SVR_Data$`IXIC Return`,IXIC_SVR_predicted_best_model,col='orange',pch=4)

plot(IXIC_SVR_Data$`IXIC Return`~IXIC_SVR_Data$`Lag Return`,xlab='Lag Returns',
     ylab='NASDAQ  Returns', main='NASDAQ  vs. Lag Returns Multiple Model Capacity',
     type='p',lwd=3)
points(IXIC_SVR_Data$`IXIC Return`,IXIC_lr_predicted,col='blue',pch=4)
points(IXIC_SVR_Data$`IXIC Return`,IXIC_SVR_predicted,col='red',pch=4)
points(IXIC_SVR_Data$`IXIC Return`,IXIC_SVR_predicted_best_model,col='orange',pch=4)


plot(abs(as.numeric(IXIC_Brent_Corn$`Return IXIC`))~IXIC_Brent_Corn$Date,type='l',col='grey',
     main='NASDAQ Actual Absolute Returns vs SVR model', xlab='Date',ylab='Return')
lines(abs(IXIC_SVR_predicted_best_model)~
        IXIC_Brent_Corn$Date,col='Orange')

# IXIC Training Error function for Optimal model
IXIC_SVR_predicted_best_model<-as.data.frame(IXIC_SVR_predicted_best_model)
IXIC_actual_returns<-as.data.frame(IXIC_SVR_Data$`IXIC Return`)

for (i in 1:nrow(IXIC_actual_returns)) {
  d<-abs(IXIC_SVR_predicted_best_model[i,1]-IXIC_actual_returns[i,1])
  d<-as.data.frame(d)
  
  if (i==1) {
    IXIC_SVR_training_abs_values<-d  
  }
  else{
    IXIC_SVR_training_abs_values<-rbind(IXIC_SVR_training_abs_values,d)
  }
}

IXIC_SVR_training_abs_values

for (i in 1:nrow(IXIC_actual_returns)) {
  d<-abs(IXIC_SVR_predicted_best_model[i,1]-IXIC_actual_returns[i,1])**2
  d<-as.data.frame(d)
  
  if (i==1) {
    IXIC_SVR_training_squared_values<-d  
  }
  else{
    IXIC_SVR_training_squared_values<-rbind(IXIC_SVR_training_squared_values,d)
  }
}
IXIC_SVR_training_squared_values

# IXIC Error in training

IXIC_SVR_training_abs_error<-sum(IXIC_SVR_training_abs_values)/nrow(IXIC_SVR_training_abs_values)
IXIC_SVR_training_mean_squared_error<-sum(IXIC_SVR_training_squared_values)/nrow(IXIC_SVR_training_squared_values)
IXIC_SVR_training_mean_root_error<- sqrt(IXIC_SVR_training_mean_squared_error)


############################################################################################
##############################################################################################
############################## IXIC Forecast values for period under analysis #####################

# test data

vol_t<-nrow(IXIC_new_volume_return)-(nrow(IXIC2)-1)
brent_t<-nrow(Brent_returns_later)-(nrow(IXIC2)-1)
corn_t<-nrow(Corn_returns_later)-(nrow(IXIC2)-1)
lag_return_t<-nrow(IXIC_lag_returns_all)-(nrow(IXIC2)-1)-9
lag_return_t1<-nrow(IXIC_lag_returns_all)-(nrow(IXIC2))
return_t<-nrow(IXIC1)-(nrow(IXIC2)-1)
IXIC_forecast_vol<- IXIC_new_volume_return[vol_t:nrow(IXIC_new_volume_return),1]
IXIC_forecast_Brent_Return<-Brent_returns_later[brent_t:nrow(Brent_returns_later),1]
IXIC_forecast_Corn_Return<- Corn_returns_later[corn_t:nrow(Corn_returns_later),1]
IXIC_forecast_lag_return<-IXIC_lag_returns_all[lag_return_t:lag_return_t1,1]
IXIC_forecast_retuns<- as.numeric(IXIC1[return_t:nrow(IXIC1),3])

IXIC_SVR_forecast_Data<-cbind(IXIC_forecast_retuns,IXIC_forecast_lag_return,IXIC_forecast_Brent_Return,
                             IXIC_forecast_Corn_Return,IXIC_forecast_vol)
IXIC_SVR_forecast_Data<-as.data.frame(IXIC_SVR_forecast_Data)

head(IXIC_SVR_forecast_Data)
dim(IXIC_SVR_forecast_Data)
colnames(IXIC_SVR_forecast_Data)<-c('IXIC Return','Lag Return','Brent Return', 'Corn Return','Lag Vol')

IXIC_SVR_total_Data<-rbind(IXIC_SVR_Data,IXIC_SVR_forecast_Data)
which(is.na(IXIC_SVR_total_Data))
IXIC_SVR_forecast_Data[is.na(IXIC_SVR_forecast_Data)]<-0
IXIC_SVR_total_Data[is.na(IXIC_SVR_total_Data)]<-0
tail(IXIC_SVR_total_Data)

# tunning up the Support vector model
names(IXIC_SVR_forecast_Data)=names(IXIC_SVR_total_Data)

set.seed(994)
IXIC_forecast_tune<-tune(svm,as.numeric(IXIC_SVR_total_Data$`IXIC Return`)~
                          as.numeric(IXIC_SVR_total_Data$`Lag Return`)+
                          as.numeric(IXIC_SVR_total_Data$`Brent Return`) +
                          as.numeric(IXIC_SVR_total_Data$`Corn Return`)+
                          as.numeric(IXIC_SVR_total_Data$`Lag Vol`),
                        data = IXIC_SVR_total_Data[,], 
                        ranges = list(epsilon=seq(0,0.4,0.1),cost=2^(2:4)))

print(IXIC_forecast_tune) # to be used on the Forecasting
plot(IXIC_forecast_tune)

IXIC_SVR_forecast_best_model<-IXIC_forecast_tune$best.model
IXIC_SVR_forecast_predicted_best_model<- predict(IXIC_SVR_forecast_best_model,IXIC_SVR_forecast_Data)

# Isolate last 9 predictons
IXIC_SVR_forecast<-as.data.frame(IXIC_SVR_forecast_predicted_best_model)
for_t<-nrow(IXIC_SVR_forecast)-(nrow(IXIC2)-1)
IXIC_SVR_forecast<-IXIC_SVR_forecast[for_t:nrow(IXIC_SVR_forecast),1]
IXIC_actual_returns_test<-IXIC2_returns

# Charts

plot(abs(as.numeric(IXIC2_returns$`diff(log(IXIC2$Close))`))~IXIC2$Date,type='l',col='grey',
     main='NASDAQ Actual Absolute Returns vs SVR model for Test', xlab='Date',
     ylab='Return',ylim=c(0,0.1))
lines(abs(IXIC_SVR_forecast)~
        IXIC2$Date,col='Orange')

# IXIC Training Error function for Optimal model
IXIC_SVR_forecast_predicted_best_model<-as.data.frame(IXIC_SVR_forecast_predicted_best_model)
IXIC_actual_returns_test<-as.data.frame(IXIC_actual_returns_test)

for (i in 1:nrow(IXIC_actual_returns_test)) {
  d<-abs(IXIC_SVR_forecast_predicted_best_model[i,1]-IXIC_actual_returns_test[i,1])
  d<-as.data.frame(d)
  
  if (i==1) {
    IXIC_SVR_test_abs_values<-d  
  }
  else{
    IXIC_SVR_test_abs_values<-rbind(IXIC_SVR_test_abs_values,d)
  }
}

IXIC_SVR_test_abs_values

for (i in 1:nrow(IXIC_actual_returns_test)) {
  d<-abs(IXIC_SVR_forecast_predicted_best_model[i,1]-IXIC_actual_returns_test[i,1])**2
  d<-as.data.frame(d)
  
  if (i==1) {
    IXIC_SVR_test_squared_values<-d  
  }
  else{
    IXIC_SVR_test_squared_values<-rbind(IXIC_SVR_test_squared_values,d)
  }
}
IXIC_SVR_test_squared_values

# IXIC Error in training

IXIC_SVR_test_abs_error<-sum(IXIC_SVR_test_abs_values)/nrow(IXIC_SVR_test_abs_values)
IXIC_SVR_test_mean_squared_error<-sum(IXIC_SVR_test_squared_values)/nrow(IXIC_SVR_test_squared_values)
IXIC_SVR_test_mean_root_error<- sqrt(IXIC_SVR_test_mean_squared_error)

