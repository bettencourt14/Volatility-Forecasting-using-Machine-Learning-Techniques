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
plot(Brent$Price~Brent$Date, main='Brent Prices from 01/01/2015 to 18/02/2022',
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
plot(Brent_returns$`a[10:nrow(a), ]`)

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
colnames(N1001)<-c('Date','Close','Return','Volume')
colnames(Brent)<-c('Date','Close','Return')
head(N1001)
head(Brent)
N100_Brent<-merge(N1001,Brent,by="Date")
N100_Brent  
str(N100_Brent)
colnames(N100_Brent)<- c('Date','Close N100','Return N100','Lag Vol','Close Brent','Return Brent')
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
colnames(N100_Brent_Corn)<- c('Date','Close N100','Return N100','Lag Vol',
                         'Close Brent','Return Brent',
                         'Close Corn','Return Corn')
str(N100_Brent_Corn)
head(N100_Brent_Corn)
which(is.na(N100_Brent_Corn))
N100_Brent_Corn[is.na(N100_Brent_Corn)]<-0
which(is.na(N100_Brent_Corn))


# N100 Model Data
N100_SVR_Data<-cbind(as.numeric(N100_Brent_Corn$`Return N100`),
                     as.numeric(N100_Brent_Corn$`Return Brent`),
                     as.numeric(N100_Brent_Corn$`Return Corn`),
                     as.numeric(N100_Brent_Corn$`Lag Vol`))

N100_SVR_Data<-as.data.frame(N100_SVR_Data)
colnames(N100_SVR_Data)<-c('N100 Return','Brent Return','Corn Return','Lag Vol')
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
       main='Euronext 100 Variable Correlations')+
  geom_tile() 

# Linear Regression
N100_lr<-lm(N100_SVR_Data$`N100 Return`~
              N100_SVR_Data$`Brent Return`+
              N100_SVR_Data$`Lag Vol`+
              N100_SVR_Data$`Corn Return`,
            N100_SVR_Data)

N100_lr_predicted<-predict(N100_lr,N100_SVR_Data)

# Support Vector Regression

N100_SVR<-svm(N100_SVR_Data$`N100 Return`~
                N100_SVR_Data$`Brent Return`+
                N100_SVR_Data$`Corn Return`+
                N100_SVR_Data$`Lag Vol`,
              N100_SVR_Data)
N100_SVR_predicted<- predict(N100_SVR,N100_SVR_Data)

# tunning up the Support vector model
N100_tune<-tune(svm,N100_SVR_Data$`N100 Return`~
                  N100_SVR_Data$`Brent Return`+
                  N100_SVR_Data$`Corn Return`+
                  N100_SVR_Data$`Lag Vol`,
                data = N100_SVR_Data, ranges = list(epsilon=seq(0,0.4,0.1),cost=2^(2:4)))
print(N100_tune) # to be used on the Forecasting
plot(N100_tune)

N100_SVR_best_model<-N100_tune$best.model
N100_SVR_predicted_best_model<- predict(N100_SVR_best_model,N100_SVR_Data)

# Charts
plot(N100_SVR_Data$`N100 Return`~N100_SVR_Data$`Brent Return`,xlab='Brent Returns',
     ylab='Euronext 100 Returns', main='Euronext 100 vs. Brent Multiple Model Capacity',
     type='p',lwd=3)
points(N100_SVR_Data$`N100 Return`,N100_lr_predicted,col='blue',pch=4)
points(N100_SVR_Data$`N100 Return`,N100_SVR_predicted,col='red',pch=4)
points(N100_SVR_Data$`N100 Return`,N100_SVR_predicted_best_model,col='orange',pch=4)

plot(N100_SVR_Data$`N100 Return`~N100_SVR_Data$`Corn Return`,xlab='Corn Returns',
     ylab='Euronext 100 Returns', main='Euronext 100 vs. Corn Multiple Model Capacity',
     type='p',lwd=3)
points(N100_SVR_Data$`N100 Return`,N100_lr_predicted,col='blue',pch=4)
points(N100_SVR_Data$`N100 Return`,N100_SVR_predicted,col='red',pch=4)
points(N100_SVR_Data$`N100 Return`,N100_SVR_predicted_best_model,col='orange',pch=4)

plot(N100_SVR_Data$`N100 Return`~N100_SVR_Data$`Lag Vol`,xlab='Lag Vol Returns',
     ylab='Euronext 100 Returns', main='Euronext 100 vs. Lag Vol Multiple Model Capacity',
     type='p',lwd=3)
points(N100_SVR_Data$`N100 Return`,N100_lr_predicted,col='blue',pch=4)
points(N100_SVR_Data$`N100 Return`,N100_SVR_predicted,col='red',pch=4)
points(N100_SVR_Data$`N100 Return`,N100_SVR_predicted_best_model,col='orange',pch=4)

plot(abs(as.numeric(N100_Brent_Corn$`Return N100`))~N100_Brent_Corn$Date,type='l',col='grey',
     main='Actual Absolute Returns vs SVR model', xlab='Date',ylab='Return')
lines(abs(N100_SVR_predicted_best_model$N100_SVR_predicted_best_model)~
        N100_Brent_Corn$Date,col='Orange')

# N100 Training Error function for Optimal model
N100_SVR_predicted_best_model<-as.data.frame(N100_SVR_predicted_best_model)
N100_actual_returns<-as.data.frame(N100_SVR_Data$`N100 Return`)

for (i in 1:nrow(N100_actual_returns)) {
  d<-abs(N100_SVR_predicted_best_model[i,1]-N100_actual_returns[i,1])
  d<-as.data.frame(d)
  
  if (i==1) {
  N100_SVR_training_abs_values<-d  
  }
  else{
    N100_SVR_training_abs_values<-rbind(N100_SVR_training_abs_values,d)
  }
}

N100_SVR_training_abs_values

for (i in 1:nrow(N100_actual_returns)) {
  d<-abs(N100_SVR_predicted_best_model[i,1]-N100_actual_returns[i,1])**2
  d<-as.data.frame(d)
  
  if (i==1) {
    N100_SVR_training_squared_values<-d  
  }
  else{
    N100_SVR_training_squared_values<-rbind(N100_SVR_training_squared_values,d)
  }
}
N100_SVR_training_squared_values

# N100 Error in training

N100_SVR_training_abs_error<-sum(N100_SVR_training_abs_values)/nrow(N100_SVR_training_abs_values)
N100_SVR_training_mean_squared_error<-sum(N100_SVR_training_squared_values)/nrow(N100_SVR_training_squared_values)
N100_SVR_training_mean_root_error<- sqrt(N100_SVR_training_mean_squared_error)


############################################################################################
##############################################################################################
############################## N100 Forecast values for period under analysis #####################

# test data

vol_t<-nrow(N100_new_volume_return)-8
brent_t<-nrow(Brent_returns_later)-8
corn_t<-nrow(Corn_returns_later)-8
return_t<-nrow(N1001)-8
N100_forecast_vol<- N100_new_volume_return[vol_t:nrow(N100_new_volume_return),1]
N100_forecast_Brent_Return<-Brent_returns_later[brent_t:nrow(Brent_returns_later),1]
N100_forecast_Corn_Return<- Corn_returns_later[corn_t:nrow(Corn_returns_later),1]
N100_forecast_retuns<- as.numeric(N1001[return_t:nrow(N1001),3])

N100_SVR_forecast_Data<-cbind(N100_forecast_retuns,N100_forecast_Brent_Return,
                              N100_forecast_Corn_Return,N100_forecast_vol)
N100_SVR_forecast_Data<-as.data.frame(N100_SVR_forecast_Data)
head(N100_SVR_forecast_Data)
colnames(N100_SVR_forecast_Data)<-c('N100 Return','Brent Return', 'Corn Return','Lag Vol')

N100_SVR_total_Data<-rbind(N100_SVR_Data,N100_SVR_forecast_Data)


# tunning up the Support vector model

N100_forecast_tune<-tune(svm,N100_SVR_Data$`N100 Return`~
                           N100_SVR_Data$`Brent Return`+
                           N100_SVR_Data$`Corn Return`+
                           N100_SVR_Data$`Lag Vol`,
                data = N100_SVR_total_Data, 
                ranges = list(epsilon=seq(0,0.4,0.1),cost=2^(2:4)))

print(N100_forecast_tune) # to be used on the Forecasting
plot(N100_forecast_tune)

N100_SVR_forecast_best_model<-N100_forecast_tune$best.model
N100_SVR_forecast_predicted_best_model<- predict(N100_SVR_best_model,N100_SVR_Data)

# Isolate last 9 predictons
N100_SVR_forecast<-as.data.frame(N100_SVR_forecast_predicted_best_model)
for_t<-nrow(N100_SVR_forecast)-8
N100_SVR_forecast<-N100_SVR_forecast[for_t:nrow(N100_SVR_forecast),1]
N100_actual_returns_test<-N1002_returns

# Charts

plot(abs(as.numeric(N1002_returns$`diff(log(N1002$Close))`))~N1002$Date,type='l',col='grey',
     main='Actual Absolute Returns vs SVR model for Test', xlab='Date',ylab='Return')
lines(abs(N100_SVR_forecast)~
        N1002$Date,col='Orange')

# N100 Training Error function for Optimal model
N100_SVR_forecast_predicted_best_model<-as.data.frame(N100_SVR_forecast_predicted_best_model)
N100_actual_returns_test<-as.data.frame(N100_actual_returns_test)

for (i in 1:nrow(N100_actual_returns_test)) {
  d<-abs(N100_SVR_forecast_predicted_best_model[i,1]-N100_actual_returns_test[i,1])
  d<-as.data.frame(d)
  
  if (i==1) {
    N100_SVR_test_abs_values<-d  
  }
  else{
    N100_SVR_test_abs_values<-rbind(N100_SVR_test_abs_values,d)
  }
}

N100_SVR_test_abs_values

for (i in 1:nrow(N100_actual_returns_test)) {
  d<-abs(N100_SVR_forecast_predicted_best_model[i,1]-N100_actual_returns_test[i,1])**2
  d<-as.data.frame(d)
  
  if (i==1) {
    N100_SVR_test_squared_values<-d  
  }
  else{
    N100_SVR_test_squared_values<-rbind(N100_SVR_test_squared_values,d)
  }
}
N100_SVR_test_squared_values

# N100 Error in training

N100_SVR_test_abs_error<-sum(N100_SVR_test_abs_values)/nrow(N100_SVR_test_abs_values)
N100_SVR_test_mean_squared_error<-sum(N100_SVR_test_squared_values)/nrow(N100_SVR_test_squared_values)
N100_SVR_test_mean_root_error<- sqrt(N100_SVR_test_mean_squared_error)

