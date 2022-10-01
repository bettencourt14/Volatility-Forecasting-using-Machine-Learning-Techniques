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

# Linear Regression
NSEI_lr<-lm(NSEI_SVR_Data$`NSEI Return`~
             NSEI_SVR_Data$`Lag Return`+
             NSEI_SVR_Data$`Brent Return`+
             NSEI_SVR_Data$`Lag Vol`+
             NSEI_SVR_Data$`Corn Return`,
           NSEI_SVR_Data)

NSEI_lr_predicted<-predict(NSEI_lr,NSEI_SVR_Data)

# Support Vector Regression

NSEI_SVR<-svm(NSEI_SVR_Data$`NSEI Return`~
               NSEI_SVR_Data$`Lag Return`+
               NSEI_SVR_Data$`Brent Return`+
               NSEI_SVR_Data$`Corn Return`+
               NSEI_SVR_Data$`Lag Vol`,
             NSEI_SVR_Data)
NSEI_SVR_predicted<- predict(NSEI_SVR,NSEI_SVR_Data)

# tunning up the Support vector model
set.seed(994)

NSEI_tune<-tune(svm,NSEI_SVR_Data$`NSEI Return`~
                 NSEI_SVR_Data$`Lag Return`+
                 NSEI_SVR_Data$`Brent Return`+
                 NSEI_SVR_Data$`Corn Return`+
                 NSEI_SVR_Data$`Lag Vol`,
               data = NSEI_SVR_Data, ranges = list(epsilon=seq(0,0.4,0.1),cost=2^(2:4)))
print(NSEI_tune) # to be used on the Forecasting
plot(NSEI_tune)

NSEI_SVR_best_model<-NSEI_tune$best.model
NSEI_SVR_predicted_best_model<- predict(NSEI_SVR_best_model,NSEI_SVR_Data)

# Charts
plot(NSEI_SVR_Data$`NSEI Return`~NSEI_SVR_Data$`Brent Return`,xlab='Brent Returns',
     ylab='NIFTY 50  Returns', main='NIFTY 50  vs. Brent Multiple Model Capacity',
     type='p',lwd=3)
points(NSEI_SVR_Data$`NSEI Return`,NSEI_lr_predicted,col='blue',pch=4)
points(NSEI_SVR_Data$`NSEI Return`,NSEI_SVR_predicted,col='red',pch=4)
points(NSEI_SVR_Data$`NSEI Return`,NSEI_SVR_predicted_best_model,col='orange',pch=4)

plot(NSEI_SVR_Data$`NSEI Return`~NSEI_SVR_Data$`Corn Return`,xlab='Corn Returns',
     ylab='NIFTY 50  Returns', main='NIFTY 50  vs. Corn Multiple Model Capacity',
     type='p',lwd=3)
points(NSEI_SVR_Data$`NSEI Return`,NSEI_lr_predicted,col='blue',pch=4)
points(NSEI_SVR_Data$`NSEI Return`,NSEI_SVR_predicted,col='red',pch=4)
points(NSEI_SVR_Data$`NSEI Return`,NSEI_SVR_predicted_best_model,col='orange',pch=4)

plot(NSEI_SVR_Data$`NSEI Return`~NSEI_SVR_Data$`Lag Vol`,xlab='Lag Vol Returns',
     ylab='NIFTY 50  Returns', main='NIFTY 50  vs. Lag Vol Multiple Model Capacity',
     type='p',lwd=3)
points(NSEI_SVR_Data$`NSEI Return`,NSEI_lr_predicted,col='blue',pch=4)
points(NSEI_SVR_Data$`NSEI Return`,NSEI_SVR_predicted,col='red',pch=4)
points(NSEI_SVR_Data$`NSEI Return`,NSEI_SVR_predicted_best_model,col='orange',pch=4)

plot(NSEI_SVR_Data$`NSEI Return`~NSEI_SVR_Data$`Lag Return`,xlab='Lag Returns',
     ylab='NIFTY 50  Returns', main='NIFTY 50  vs. Lag Returns Multiple Model Capacity',
     type='p',lwd=3)
points(NSEI_SVR_Data$`NSEI Return`,NSEI_lr_predicted,col='blue',pch=4)
points(NSEI_SVR_Data$`NSEI Return`,NSEI_SVR_predicted,col='red',pch=4)
points(NSEI_SVR_Data$`NSEI Return`,NSEI_SVR_predicted_best_model,col='orange',pch=4)


plot(abs(as.numeric(NSEI_Brent_Corn$`Return NSEI`))~NSEI_Brent_Corn$Date,type='l',col='grey',
     main='NIFTY 50 Actual Absolute Returns vs SVR model', xlab='Date',ylab='Return')
lines(abs(NSEI_SVR_predicted_best_model)~
        NSEI_Brent_Corn$Date,col='Orange')

# NSEI Training Error function for Optimal model
NSEI_SVR_predicted_best_model<-as.data.frame(NSEI_SVR_predicted_best_model)
NSEI_actual_returns<-as.data.frame(NSEI_SVR_Data$`NSEI Return`)

for (i in 1:nrow(NSEI_actual_returns)) {
  d<-abs(NSEI_SVR_predicted_best_model[i,1]-NSEI_actual_returns[i,1])
  d<-as.data.frame(d)
  
  if (i==1) {
    NSEI_SVR_training_abs_values<-d  
  }
  else{
    NSEI_SVR_training_abs_values<-rbind(NSEI_SVR_training_abs_values,d)
  }
}

NSEI_SVR_training_abs_values

for (i in 1:nrow(NSEI_actual_returns)) {
  d<-abs(NSEI_SVR_predicted_best_model[i,1]-NSEI_actual_returns[i,1])**2
  d<-as.data.frame(d)
  
  if (i==1) {
    NSEI_SVR_training_squared_values<-d  
  }
  else{
    NSEI_SVR_training_squared_values<-rbind(NSEI_SVR_training_squared_values,d)
  }
}
NSEI_SVR_training_squared_values

# NSEI Error in training

NSEI_SVR_training_abs_error<-sum(NSEI_SVR_training_abs_values)/nrow(NSEI_SVR_training_abs_values)
NSEI_SVR_training_mean_squared_error<-sum(NSEI_SVR_training_squared_values)/nrow(NSEI_SVR_training_squared_values)
NSEI_SVR_training_mean_root_error<- sqrt(NSEI_SVR_training_mean_squared_error)


############################################################################################
##############################################################################################
############################## NSEI Forecast values for period under analysis #####################

# test data

vol_t<-nrow(NSEI_new_volume_return)-(nrow(NSEI2)-1)
brent_t<-nrow(Brent_returns_later)-(nrow(NSEI2)-1)
corn_t<-nrow(Corn_returns_later)-(nrow(NSEI2)-1)
lag_return_t<-nrow(NSEI_lag_returns_all)-(nrow(NSEI2)-1)-9
lag_return_t1<-nrow(NSEI_lag_returns_all)-(nrow(NSEI2))
return_t<-nrow(NSEI1)-(nrow(NSEI2)-1)
NSEI_forecast_vol<- NSEI_new_volume_return[vol_t:nrow(NSEI_new_volume_return),1]
NSEI_forecast_Brent_Return<-Brent_returns_later[brent_t:nrow(Brent_returns_later),1]
NSEI_forecast_Corn_Return<- Corn_returns_later[corn_t:nrow(Corn_returns_later),1]
NSEI_forecast_lag_return<-NSEI_lag_returns_all[lag_return_t:lag_return_t1,1]
NSEI_forecast_retuns<- as.numeric(NSEI1[return_t:nrow(NSEI1),3])

NSEI_SVR_forecast_Data<-cbind(NSEI_forecast_retuns,NSEI_forecast_lag_return,NSEI_forecast_Brent_Return,
                             NSEI_forecast_Corn_Return,NSEI_forecast_vol)
NSEI_SVR_forecast_Data<-as.data.frame(NSEI_SVR_forecast_Data)

head(NSEI_SVR_forecast_Data)
dim(NSEI_SVR_forecast_Data)
colnames(NSEI_SVR_forecast_Data)<-c('NSEI Return','Lag Return','Brent Return', 'Corn Return','Lag Vol')

NSEI_SVR_total_Data<-rbind(NSEI_SVR_Data,NSEI_SVR_forecast_Data)
which(is.na(NSEI_SVR_total_Data))
NSEI_SVR_forecast_Data[is.na(NSEI_SVR_forecast_Data)]<-0
NSEI_SVR_total_Data[is.na(NSEI_SVR_total_Data)]<-0
tail(NSEI_SVR_total_Data)

# tunning up the Support vector model
names(NSEI_SVR_forecast_Data)=names(NSEI_SVR_total_Data)

set.seed(994)
NSEI_forecast_tune<-tune(svm,as.numeric(NSEI_SVR_total_Data$`NSEI Return`)~
                          as.numeric(NSEI_SVR_total_Data$`Lag Return`)+
                          as.numeric(NSEI_SVR_total_Data$`Brent Return`) +
                          as.numeric(NSEI_SVR_total_Data$`Corn Return`)+
                          as.numeric(NSEI_SVR_total_Data$`Lag Vol`),
                        data = NSEI_SVR_total_Data[,], 
                        ranges = list(epsilon=seq(0,0.4,0.1),cost=2^(2:4)))

print(NSEI_forecast_tune) # to be used on the Forecasting
plot(NSEI_forecast_tune)

NSEI_SVR_forecast_best_model<-NSEI_forecast_tune$best.model
NSEI_SVR_forecast_predicted_best_model<- predict(NSEI_SVR_forecast_best_model,NSEI_SVR_forecast_Data)

# Isolate last 9 predictons
NSEI_SVR_forecast<-as.data.frame(NSEI_SVR_forecast_predicted_best_model)
for_t<-nrow(NSEI_SVR_forecast)-(nrow(NSEI2)-1)
NSEI_SVR_forecast<-NSEI_SVR_forecast[for_t:nrow(NSEI_SVR_forecast),1]
NSEI_actual_returns_test<-NSEI2_returns

# Charts

plot(abs(as.numeric(NSEI2_returns$`diff(log(NSEI2$Close))`))~NSEI2$Date,type='l',col='grey',
     main='NIFTY 50 Actual Absolute Returns vs SVR model for Test', xlab='Date',
     ylab='Return',ylim=c(0,0.1))
lines(abs(NSEI_SVR_forecast)~
        NSEI2$Date,col='Orange')

# NSEI Training Error function for Optimal model
NSEI_SVR_forecast_predicted_best_model<-as.data.frame(NSEI_SVR_forecast_predicted_best_model)
NSEI_actual_returns_test<-as.data.frame(NSEI_actual_returns_test)

for (i in 1:nrow(NSEI_actual_returns_test)) {
  d<-abs(NSEI_SVR_forecast_predicted_best_model[i,1]-NSEI_actual_returns_test[i,1])
  d<-as.data.frame(d)
  
  if (i==1) {
    NSEI_SVR_test_abs_values<-d  
  }
  else{
    NSEI_SVR_test_abs_values<-rbind(NSEI_SVR_test_abs_values,d)
  }
}

NSEI_SVR_test_abs_values

for (i in 1:nrow(NSEI_actual_returns_test)) {
  d<-abs(NSEI_SVR_forecast_predicted_best_model[i,1]-NSEI_actual_returns_test[i,1])**2
  d<-as.data.frame(d)
  
  if (i==1) {
    NSEI_SVR_test_squared_values<-d  
  }
  else{
    NSEI_SVR_test_squared_values<-rbind(NSEI_SVR_test_squared_values,d)
  }
}
NSEI_SVR_test_squared_values

# NSEI Error in training

NSEI_SVR_test_abs_error<-sum(NSEI_SVR_test_abs_values)/nrow(NSEI_SVR_test_abs_values)
NSEI_SVR_test_mean_squared_error<-sum(NSEI_SVR_test_squared_values)/nrow(NSEI_SVR_test_squared_values)
NSEI_SVR_test_mean_root_error<- sqrt(NSEI_SVR_test_mean_squared_error)

