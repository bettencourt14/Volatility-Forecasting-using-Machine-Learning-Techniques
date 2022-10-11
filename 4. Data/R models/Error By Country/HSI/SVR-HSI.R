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
getSymbols("^HSI",src = 'yahoo', from=d1,to=d2)
HSI_10d<-as.data.frame(HSI)
dim(HSI_10d)


source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Preliminary analysis/HSI Preliminary Analysis.R")

HSI_10d_returns<-as.data.frame(diff(log(HSI_10d$HSI.Close)))
HSI_lag_Volume<-as.data.frame(HSI_10d[,5])
HSI_10d<-as.data.frame(cbind(HSI_10d[2:nrow(HSI_10d),4],HSI_10d_returns,
                             HSI_10d[2:nrow(HSI_10d),5]))
colnames(HSI_10d)<-c('Close','Return','Volume')
HSI_10d

#Exogenous variables 

Brent <- read_excel("High Frequency Data/Brent.xlsx",col_types = c("date", "numeric"))
Corn <- read_excel("High Frequency Data/Corn.xlsx",col_types = c("date", "numeric"))


Brent_returns<-diff(log(Brent$Price))
Brent<-as.data.frame(Brent)
Corn_returns<-diff(log(Corn$Price))

par(mar=c(5,5,5,5))
plot(x=Brent$Date,y=Brent$Price,
     xlab='Date', ylab='Brent Price', col='darkblue', type='l')
par(new=T)
plot(Corn$Date,y=Corn$Price, main='Brent And Corn Prices',
     xlab='', ylab='', col='darkorange', type='l',axes=FALSE)
axis(side=4,at=pretty(range(Corn$Price)))
mtext('Corn Price',side=4,line=3)
legend('topleft',
       legend=c('Brent Prices','Corn Prices'),
       col=c('darkblue','darkorange'),
       pch = c(9,9))



plot(Brent_returns~Brent$Date[2:nrow(Brent)], main='Brent Returns',
     xlab='Date',ylab='Return', type='l', col='darkblue', ylim=c(-0.65,0.65))
abline(h=0,col='darkorange', lty=4,lwd=4)
legend('topleft',
       legend=c('Actual Return','Zero Volatility'),
       col=c('darkblue','darkorange'),
       pch = c(9,9))

plot(Corn_returns~Corn$Date[2:nrow(Corn)], main='Corn Returns',
     xlab='Date',ylab='Return', type='l', col='darkblue', ylim=c(-0.15,0.15))
abline(h=0,col='darkorange', lty=4,lwd=4)
legend('topleft',
       legend=c('Actual Return','Zero Volatility'),
       col=c('darkblue','darkorange'),
       pch = c(9,9))

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

HSI_lag_Volume
str(HSI_lag_Volume)
HSI_old_volume<- as.data.frame(HSI1[,4])
str(HSI_old_volume)
HSI_old_volume[HSI_old_volume==0]<-NA
names(HSI_lag_Volume)<-names(HSI_old_volume)
HSI_new_volume<-rbind(HSI_lag_Volume,HSI_old_volume)
HSI_new_volume<-diff(log(HSI_new_volume$`HSI1[, 4]`))
HSI_new_volume<-as.data.frame(HSI_new_volume)
HSI_new_volume_return<-HSI_new_volume[1:nrow(HSI1),1]
HSI_new_volume_return<-as.data.frame(HSI_new_volume_return)

for (i in 1:nrow(HSI1)) {
  HSI1[i,4]=HSI_new_volume_return[i,1]
}

HSI1

# 9 day lagged returns

HSI1<-cbind(HSI1,HSI1$HSI1_return)
HSI_normal_returns<-as.data.frame(HSI1[,5])
HSI_10d_returns<-as.data.frame(HSI_10d_returns[,1])
names(HSI_10d_returns)<-names(HSI_normal_returns)
HSI_lag_returns_all<-rbind(HSI_10d_returns,HSI_normal_returns)
str(HSI_lag_returns_all)
dim(HSI_lag_returns_all)
dim(HSI1)


for (i in 1:nrow(HSI1)) {
  HSI1[i,5]=HSI_lag_returns_all[i,1]
}

dim(HSI1)
colnames(HSI_lag_returns_all)<-'Lag return'

# merge with Brent by date
Brent_returns<-rbind(0,Brent_returns)
Brent_returns<-as.data.frame(Brent_returns)
Brent<- cbind(Brent[10:nrow(Brent),],Brent_returns$Brent_returns)
str(Brent)
format(Brent$Date,"%d/%m/%Y")
str(Brent)
which(is.na(Brent))
nrow(Brent)
nrow(HSI1)
colnames(HSI1)<-c('Date','Close','Return','Volume','Lag Return')
colnames(Brent)<-c('Date','Close','Return')
head(HSI1)
head(Brent)
HSI_Brent<-merge(HSI1,Brent,by="Date")
HSI_Brent  
str(HSI_Brent)
colnames(HSI_Brent)<- c('Date','Close HSI','Return HSI','Lag Vol','Lag Return','Close Brent','Return Brent')
head(HSI_Brent)
which(is.na(HSI_Brent))
HSI_Brent[is.na(HSI_Brent)]<-0
which(is.na(HSI_Brent))

#merge with Corn by date
Corn_returns<- as.data.frame(Corn_returns)
Corn_returns<-rbind(0,Corn_returns)
Corn<- cbind(Corn[10:nrow(Corn),],Corn_returns$Corn_returns)
str(Corn)
format(Corn$Date,"%d/%m/%Y")
str(Corn)
which(is.na(Corn))
nrow(Corn)
nrow(HSI1)
colnames(Corn)<-c('Date','Close','Return')
HSI_Brent_Corn<-merge(HSI_Brent,Corn,by="Date")
str(HSI_Brent_Corn)
colnames(HSI_Brent_Corn)<- c('Date','Close HSI','Return HSI','Lag Vol','Lag Return',
                             'Close Brent','Return Brent',
                             'Close Corn','Return Corn')
str(HSI_Brent_Corn)
head(HSI_Brent_Corn)
which(is.na(HSI_Brent_Corn))
HSI_Brent_Corn[is.na(HSI_Brent_Corn)]<-0
which(is.na(HSI_Brent_Corn))

# HSI Model Data
HSI_SVR_Data<-cbind(as.numeric(HSI_Brent_Corn$`Return HSI`),
                    as.numeric(HSI_Brent_Corn$`Lag Return`),
                    as.numeric(HSI_Brent_Corn$`Return Brent`),
                    as.numeric(HSI_Brent_Corn$`Return Corn`),
                    as.numeric(HSI_Brent_Corn$`Lag Vol`))

HSI_SVR_Data<-as.data.frame(HSI_SVR_Data)
colnames(HSI_SVR_Data)<-c('HSI Return','Lag Return','Brent Return','Corn Return','Lag Vol')
head(HSI_SVR_Data)
which(is.na(HSI_SVR_Data))
HSI_SVR_Data[is.na(HSI_SVR_Data)]<-0
which(is.na(HSI_SVR_Data))
str(HSI_SVR_Data)

HSI_SVR_Data

# Correlation between variables
cormat<-round(cor(HSI_SVR_Data),3)
cormat

melted_cormat<-melt(cormat)
melted_cormat
ggplot(data = melted_cormat,aes(x=Var1,y=Var2,fill=value),
       main='Hang Seng  Variable Correlations')+
  geom_tile() 

# Linear Regression
HSI_lr<-lm(HSI_SVR_Data$`HSI Return`~
             HSI_SVR_Data$`Lag Return`+
             HSI_SVR_Data$`Brent Return`+
             HSI_SVR_Data$`Lag Vol`+
             HSI_SVR_Data$`Corn Return`,
           HSI_SVR_Data)

HSI_lr_predicted<-predict(HSI_lr,HSI_SVR_Data)

# Support Vector Regression

HSI_SVR<-svm(HSI_SVR_Data$`HSI Return`~
               HSI_SVR_Data$`Lag Return`+
               HSI_SVR_Data$`Brent Return`+
               HSI_SVR_Data$`Corn Return`+
               HSI_SVR_Data$`Lag Vol`,
             HSI_SVR_Data)
HSI_SVR_predicted<- predict(HSI_SVR,HSI_SVR_Data)

# tunning up the Support vector model
set.seed(994)

HSI_tune<-tune(svm,HSI_SVR_Data$`HSI Return`~
                 HSI_SVR_Data$`Lag Return`+
                 HSI_SVR_Data$`Brent Return`+
                 HSI_SVR_Data$`Corn Return`+
                 HSI_SVR_Data$`Lag Vol`,
               data = HSI_SVR_Data, ranges = list(epsilon=seq(0,0.4,0.1),cost=2^(2:4)))
print(HSI_tune) # to be used on the Forecasting
plot(HSI_tune, main='Hang Seng SVR Model Performance on Training Dataset',col='darkblue')

HSI_SVR_best_model<-HSI_tune$best.model
HSI_SVR_predicted_best_model<- predict(HSI_SVR_best_model,HSI_SVR_Data)

# Charts
plot(HSI_SVR_Data$`HSI Return`~HSI_SVR_Data$`Brent Return`,xlab='Brent Returns',
     ylab='Hang Seng  Returns', main='Hang Seng  vs. Brent Multiple Model Capacity',
     type='p',lwd=3)
points(HSI_SVR_Data$`HSI Return`,HSI_lr_predicted,col='blue',pch=4)
points(HSI_SVR_Data$`HSI Return`,HSI_SVR_predicted,col='red',pch=4)
points(HSI_SVR_Data$`HSI Return`,HSI_SVR_predicted_best_model,col='orange',pch=4)

plot(HSI_SVR_Data$`HSI Return`~HSI_SVR_Data$`Corn Return`,xlab='Corn Returns',
     ylab='Hang Seng  Returns', main='Hang Seng  vs. Corn Multiple Model Capacity',
     type='p',lwd=3)
points(HSI_SVR_Data$`HSI Return`,HSI_lr_predicted,col='blue',pch=4)
points(HSI_SVR_Data$`HSI Return`,HSI_SVR_predicted,col='red',pch=4)
points(HSI_SVR_Data$`HSI Return`,HSI_SVR_predicted_best_model,col='orange',pch=4)

plot(HSI_SVR_Data$`HSI Return`~HSI_SVR_Data$`Lag Vol`,xlab='Lag Vol Returns',
     ylab='Hang Seng  Returns', main='Hang Seng  vs. Lag Vol Multiple Model Capacity',
     type='p',lwd=3)
points(HSI_SVR_Data$`HSI Return`,HSI_lr_predicted,col='blue',pch=4)
points(HSI_SVR_Data$`HSI Return`,HSI_SVR_predicted,col='red',pch=4)
points(HSI_SVR_Data$`HSI Return`,HSI_SVR_predicted_best_model,col='orange',pch=4)

plot(HSI_SVR_Data$`HSI Return`~HSI_SVR_Data$`Lag Return`,xlab='Lag Returns',
     ylab='Hang Seng  Returns', main='Hang Seng  vs. Lag Returns Multiple Model Capacity',
     type='p',lwd=3)
points(HSI_SVR_Data$`HSI Return`,HSI_lr_predicted,col='blue',pch=4)
points(HSI_SVR_Data$`HSI Return`,HSI_SVR_predicted,col='red',pch=4)
points(HSI_SVR_Data$`HSI Return`,HSI_SVR_predicted_best_model,col='orange',pch=4)


plot(abs(as.numeric(HSI_Brent_Corn$`Return HSI`))~HSI_Brent_Corn$Date,type='l',col='darkblue',
     main='Hang Seng SVR Training', xlab='Date',ylab='Volatility')
lines(abs(HSI_SVR_predicted_best_model)~
        HSI_Brent_Corn$Date,col='darkorange')
legend('topleft',
       legend=c('Absolute Actual Return','SVR Forecast'),
       col=c('darkblue','darkorange'),
       pch=c(9,9))

# HSI Training Error function for Optimal model
HSI_SVR_predicted_best_model<-as.data.frame(HSI_SVR_predicted_best_model)
HSI_actual_returns<-as.data.frame(HSI_SVR_Data$`HSI Return`)

for (i in 1:nrow(HSI_actual_returns)) {
  d<-abs(HSI_SVR_predicted_best_model[i,1]-HSI_actual_returns[i,1])
  d<-as.data.frame(d)
  
  if (i==1) {
    HSI_SVR_training_abs_values<-d  
  }
  else{
    HSI_SVR_training_abs_values<-rbind(HSI_SVR_training_abs_values,d)
  }
}

HSI_SVR_training_abs_values

for (i in 1:nrow(HSI_actual_returns)) {
  d<-abs(HSI_SVR_predicted_best_model[i,1]-HSI_actual_returns[i,1])**2
  d<-as.data.frame(d)
  
  if (i==1) {
    HSI_SVR_training_squared_values<-d  
  }
  else{
    HSI_SVR_training_squared_values<-rbind(HSI_SVR_training_squared_values,d)
  }
}
HSI_SVR_training_squared_values

# HSI Error in training

HSI_SVR_training_abs_error<-sum(HSI_SVR_training_abs_values)/nrow(HSI_SVR_training_abs_values)
HSI_SVR_training_mean_squared_error<-sum(HSI_SVR_training_squared_values)/nrow(HSI_SVR_training_squared_values)
HSI_SVR_training_mean_root_error<- sqrt(HSI_SVR_training_mean_squared_error)


############################################################################################
##############################################################################################
############################## HSI Forecast values for period under analysis #####################

# test data

vol_t<-nrow(HSI_new_volume_return)-(nrow(HSI2)-1)
brent_t<-nrow(Brent_returns_later)-(nrow(HSI2)-1)
corn_t<-nrow(Corn_returns_later)-(nrow(HSI2)-1)
lag_return_t<-nrow(HSI_lag_returns_all)-(nrow(HSI2)-1)-9
lag_return_t1<-nrow(HSI_lag_returns_all)-(nrow(HSI2))
return_t<-nrow(HSI1)-(nrow(HSI2)-1)
HSI_forecast_vol<- HSI_new_volume_return[vol_t:nrow(HSI_new_volume_return),1]
HSI_forecast_Brent_Return<-Brent_returns_later[brent_t:nrow(Brent_returns_later),1]
HSI_forecast_Corn_Return<- Corn_returns_later[corn_t:nrow(Corn_returns_later),1]
HSI_forecast_lag_return<-HSI_lag_returns_all[lag_return_t:lag_return_t1,1]
HSI_forecast_retuns<- as.numeric(HSI1[return_t:nrow(HSI1),3])

HSI_SVR_forecast_Data<-cbind(HSI_forecast_retuns,HSI_forecast_lag_return,HSI_forecast_Brent_Return,
                             HSI_forecast_Corn_Return,HSI_forecast_vol)
HSI_SVR_forecast_Data<-as.data.frame(HSI_SVR_forecast_Data)

head(HSI_SVR_forecast_Data)
dim(HSI_SVR_forecast_Data)
colnames(HSI_SVR_forecast_Data)<-c('HSI Return','Lag Return','Brent Return', 'Corn Return','Lag Vol')

HSI_SVR_total_Data<-rbind(HSI_SVR_Data,HSI_SVR_forecast_Data)
which(is.na(HSI_SVR_total_Data))
HSI_SVR_forecast_Data[is.na(HSI_SVR_forecast_Data)]<-0
HSI_SVR_total_Data[is.na(HSI_SVR_total_Data)]<-0
tail(HSI_SVR_total_Data)

# tunning up the Support vector model
names(HSI_SVR_forecast_Data)=names(HSI_SVR_total_Data)

set.seed(994)
HSI_forecast_tune<-tune(svm,as.numeric(HSI_SVR_total_Data$`HSI Return`)~
                          as.numeric(HSI_SVR_total_Data$`Lag Return`)+
                          as.numeric(HSI_SVR_total_Data$`Brent Return`) +
                          as.numeric(HSI_SVR_total_Data$`Corn Return`)+
                          as.numeric(HSI_SVR_total_Data$`Lag Vol`),
                        data = HSI_SVR_total_Data[,], 
                        ranges = list(epsilon=seq(0,0.4,0.1),cost=2^(2:4)))

print(HSI_forecast_tune) # to be used on the Forecasting
plot(HSI_forecast_tune, main=' Hang Seng SVR Model Performance Test Dataset')

HSI_SVR_forecast_best_model<-HSI_forecast_tune$best.model
HSI_SVR_forecast_predicted_best_model<- predict(HSI_SVR_forecast_best_model,HSI_SVR_forecast_Data)

# Isolate last 9 predictons
HSI_SVR_forecast<-as.data.frame(HSI_SVR_forecast_predicted_best_model)
for_t<-nrow(HSI_SVR_forecast)-(nrow(HSI2)-1)
HSI_SVR_forecast<-HSI_SVR_forecast[for_t:nrow(HSI_SVR_forecast),1]
HSI_actual_returns_test<-HSI2_returns

# Charts

plot(abs(as.numeric(HSI2_returns$`diff(log(HSI2$Close))`))~HSI2$Date,type='l',col='darkblue',
     main='Hang Seng SVR Test', xlab='Date',
     ylab='Return',ylim=c(0,0.1))
lines(abs(HSI_SVR_forecast)~
        HSI2$Date,col='darkorange')
legend('topleft',
       legend=c('Absolute Actual Return','SVR Forecast'),
       col=c('darkblue','darkorange'),
       pch = c(9,9))

# HSI Training Error function for Optimal model
HSI_SVR_forecast<-as.data.frame(HSI_SVR_forecast)
HSI_actual_returns_test<-as.data.frame(HSI_actual_returns_test)

for (i in 1:nrow(HSI_actual_returns_test)) {
  d<-abs(HSI_SVR_forecast[i,1]-HSI_actual_returns_test[i,1])
  d<-as.data.frame(d)
  
  if (i==1) {
    HSI_SVR_test_abs_values<-d  
  }
  else{
    HSI_SVR_test_abs_values<-rbind(HSI_SVR_test_abs_values,d)
  }
}

HSI_SVR_test_abs_values

for (i in 1:nrow(HSI_actual_returns_test)) {
  d<-abs(HSI_SVR_forecast[i,1]-HSI_actual_returns_test[i,1])**2
  d<-as.data.frame(d)
  
  if (i==1) {
    HSI_SVR_test_squared_values<-d  
  }
  else{
    HSI_SVR_test_squared_values<-rbind(HSI_SVR_test_squared_values,d)
  }
}
HSI_SVR_test_squared_values

# HSI Error in test

HSI_SVR_test_abs_error<-sum(HSI_SVR_test_abs_values)/nrow(HSI_SVR_test_abs_values)
HSI_SVR_test_mean_squared_error<-sum(HSI_SVR_test_squared_values)/nrow(HSI_SVR_test_squared_values)
HSI_SVR_test_mean_root_error<- sqrt(HSI_SVR_test_mean_squared_error)

