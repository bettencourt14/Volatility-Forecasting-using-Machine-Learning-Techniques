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
colnames(BVSP1)<-c('Date','Close','Return','Volume')
colnames(Brent)<-c('Date','Close','Return')
head(BVSP1)
head(Brent)
BVSP_Brent<-merge(BVSP1,Brent,by="Date")
BVSP_Brent  
str(BVSP_Brent)
colnames(BVSP_Brent)<- c('Date','Close BVSP','Return BVSP','Lag Vol','Close Brent','Return Brent')
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
colnames(BVSP_Brent_Corn)<- c('Date','Close BVSP','Return BVSP','Lag Vol',
                         'Close Brent','Return Brent',
                         'Close Corn','Return Corn')
str(BVSP_Brent_Corn)
head(BVSP_Brent_Corn)
which(is.na(BVSP_Brent_Corn))
BVSP_Brent_Corn[is.na(BVSP_Brent_Corn)]<-0
which(is.na(BVSP_Brent_Corn))


# BVSP Model Data
BVSP_SVR_Data<-cbind(as.numeric(BVSP_Brent_Corn$`Return BVSP`),
                     as.numeric(BVSP_Brent_Corn$`Return Brent`),
                     as.numeric(BVSP_Brent_Corn$`Return Corn`),
                     as.numeric(BVSP_Brent_Corn$`Lag Vol`))

BVSP_SVR_Data<-as.data.frame(BVSP_SVR_Data)
colnames(BVSP_SVR_Data)<-c('BVSP Return','Brent Return','Corn Return','Lag Vol')
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
       main='IBOVESPA Variable Correlations')+
  geom_tile() 

# Linear Regression
BVSP_lr<-lm(BVSP_SVR_Data$`BVSP Return`~
              BVSP_SVR_Data$`Brent Return`+
              BVSP_SVR_Data$`Lag Vol`+
              BVSP_SVR_Data$`Corn Return`,
            BVSP_SVR_Data)

BVSP_lr_predicted<-predict(BVSP_lr,BVSP_SVR_Data)

# Support Vector Regression

BVSP_SVR<-svm(BVSP_SVR_Data$`BVSP Return`~
                BVSP_SVR_Data$`Brent Return`+
                BVSP_SVR_Data$`Corn Return`+
                BVSP_SVR_Data$`Lag Vol`,
              BVSP_SVR_Data)
BVSP_SVR_predicted<- predict(BVSP_SVR,BVSP_SVR_Data)

# tunning up the Support vector model
BVSP_tune<-tune(svm,BVSP_SVR_Data$`BVSP Return`~
                  BVSP_SVR_Data$`Brent Return`+
                  BVSP_SVR_Data$`Corn Return`+
                  BVSP_SVR_Data$`Lag Vol`,
                data = BVSP_SVR_Data, ranges = list(epsilon=seq(0,0.4,0.1),cost=2^(2:4)))
print(BVSP_tune) # to be used on the Forecasting
plot(BVSP_tune)

BVSP_SVR_best_model<-BVSP_tune$best.model
BVSP_SVR_predicted_best_model<- predict(BVSP_SVR_best_model,BVSP_SVR_Data)

# Charts
plot(BVSP_SVR_Data$`BVSP Return`~BVSP_SVR_Data$`Brent Return`,xlab='Brent Returns',
     ylab='IBOVESPA Returns', main='IBOVESPA vs. Brent Multiple Model Capacity',
     type='p',lwd=3)
points(BVSP_SVR_Data$`BVSP Return`,BVSP_lr_predicted,col='blue',pch=4)
points(BVSP_SVR_Data$`BVSP Return`,BVSP_SVR_predicted,col='red',pch=4)
points(BVSP_SVR_Data$`BVSP Return`,BVSP_SVR_predicted_best_model,col='orange',pch=4)

plot(BVSP_SVR_Data$`BVSP Return`~BVSP_SVR_Data$`Corn Return`,xlab='Corn Returns',
     ylab='IBOVESPA Returns', main='IBOVESPA vs. Corn Multiple Model Capacity',
     type='p',lwd=3)
points(BVSP_SVR_Data$`BVSP Return`,BVSP_lr_predicted,col='blue',pch=4)
points(BVSP_SVR_Data$`BVSP Return`,BVSP_SVR_predicted,col='red',pch=4)
points(BVSP_SVR_Data$`BVSP Return`,BVSP_SVR_predicted_best_model,col='orange',pch=4)

plot(BVSP_SVR_Data$`BVSP Return`~BVSP_SVR_Data$`Lag Vol`,xlab='Lag Vol Returns',
     ylab='IBOVESPA Returns', main='IBOVESPA vs. Lag Vol Multiple Model Capacity',
     type='p',lwd=3)
points(BVSP_SVR_Data$`BVSP Return`,BVSP_lr_predicted,col='blue',pch=4)
points(BVSP_SVR_Data$`BVSP Return`,BVSP_SVR_predicted,col='red',pch=4)
points(BVSP_SVR_Data$`BVSP Return`,BVSP_SVR_predicted_best_model,col='orange',pch=4)

plot(abs(as.numeric(BVSP_Brent_Corn$`Return BVSP`))~BVSP_Brent_Corn$Date,type='l',col='grey',
     main='Actual Absolute Returns vs SVR model', xlab='Date',ylab='Return')
lines(abs(BVSP_SVR_predicted_best_model)~
        BVSP_Brent_Corn$Date,col='Orange')

# BVSP Training Error function for Optimal model
BVSP_SVR_predicted_best_model<-as.data.frame(BVSP_SVR_predicted_best_model)
BVSP_actual_returns<-as.data.frame(BVSP_SVR_Data$`BVSP Return`)

for (i in 1:nrow(BVSP_actual_returns)) {
  d<-abs(BVSP_SVR_predicted_best_model[i,1]-BVSP_actual_returns[i,1])
  d<-as.data.frame(d)
  
  if (i==1) {
  BVSP_SVR_training_abs_values<-d  
  }
  else{
    BVSP_SVR_training_abs_values<-rbind(BVSP_SVR_training_abs_values,d)
  }
}

BVSP_SVR_training_abs_values

for (i in 1:nrow(BVSP_actual_returns)) {
  d<-abs(BVSP_SVR_predicted_best_model[i,1]-BVSP_actual_returns[i,1])**2
  d<-as.data.frame(d)
  
  if (i==1) {
    BVSP_SVR_training_squared_values<-d  
  }
  else{
    BVSP_SVR_training_squared_values<-rbind(BVSP_SVR_training_squared_values,d)
  }
}
BVSP_SVR_training_squared_values

# BVSP Error in training

BVSP_SVR_training_abs_error<-sum(BVSP_SVR_training_abs_values)/nrow(BVSP_SVR_training_abs_values)
BVSP_SVR_training_mean_squared_error<-sum(BVSP_SVR_training_squared_values)/nrow(BVSP_SVR_training_squared_values)
BVSP_SVR_training_mean_root_error<- sqrt(BVSP_SVR_training_mean_squared_error)


############################################################################################
##############################################################################################
############################## BVSP Forecast values for period under analysis #####################

# test data

vol_t<-nrow(BVSP_new_volume_return)-(nrow(BVSP2)-1)
brent_t<-nrow(Brent_returns_later)-(nrow(BVSP2)-1)
corn_t<-nrow(Corn_returns_later)-(nrow(BVSP2)-1)
return_t<-nrow(BVSP1)-(nrow(BVSP2)-1)
BVSP_forecast_vol<- BVSP_new_volume_return[vol_t:nrow(BVSP_new_volume_return),1]
BVSP_forecast_Brent_Return<-Brent_returns_later[brent_t:nrow(Brent_returns_later),1]
BVSP_forecast_Corn_Return<- Corn_returns_later[corn_t:nrow(Corn_returns_later),1]
BVSP_forecast_retuns<- as.numeric(BVSP1[return_t:nrow(BVSP1),3])

BVSP_SVR_forecast_Data<-cbind(BVSP_forecast_retuns,BVSP_forecast_Brent_Return,
                              BVSP_forecast_Corn_Return,BVSP_forecast_vol)
BVSP_SVR_forecast_Data<-as.data.frame(BVSP_SVR_forecast_Data)
head(BVSP_SVR_forecast_Data)
dim(BVSP_SVR_forecast_Data)
colnames(BVSP_SVR_forecast_Data)<-c('BVSP Return','Brent Return', 'Corn Return','Lag Vol')

BVSP_SVR_total_Data<-rbind(BVSP_SVR_Data,BVSP_SVR_forecast_Data)


# tunning up the Support vector model

BVSP_forecast_tune<-tune(svm,BVSP_SVR_Data$`BVSP Return`~
                           BVSP_SVR_Data$`Brent Return`+
                           BVSP_SVR_Data$`Corn Return`+
                           BVSP_SVR_Data$`Lag Vol`,
                data = BVSP_SVR_total_Data, 
                ranges = list(epsilon=seq(0,0.4,0.1),cost=2^(2:4)))

print(BVSP_forecast_tune) # to be used on the Forecasting
plot(BVSP_forecast_tune)

BVSP_SVR_forecast_best_model<-BVSP_forecast_tune$best.model
BVSP_SVR_forecast_predicted_best_model<- predict(BVSP_SVR_best_model,BVSP_SVR_Data)

# Isolate last 9 predictons
BVSP_SVR_forecast<-as.data.frame(BVSP_SVR_forecast_predicted_best_model)
for_t<-nrow(BVSP_SVR_forecast)-(nrow(BVSP2)-1)
BVSP_SVR_forecast<-BVSP_SVR_forecast[for_t:nrow(BVSP_SVR_forecast),1]
BVSP_actual_returns_test<-BVSP2_returns

# Charts


plot(abs(as.numeric(BVSP2_returns$`diff(log(BVSP2$Close))`))~BVSP2$Date,type='l',col='grey',
     main='Actual Absolute Returns vs SVR model for Test', xlab='Date',
     ylab='Return',ylim=c(0,0.1))
lines(abs(BVSP_SVR_forecast)~
        BVSP2$Date,col='Orange')

# BVSP Training Error function for Optimal model
BVSP_SVR_forecast_predicted_best_model<-as.data.frame(BVSP_SVR_forecast_predicted_best_model)
BVSP_actual_returns_test<-as.data.frame(BVSP_actual_returns_test)

for (i in 1:nrow(BVSP_actual_returns_test)) {
  d<-abs(BVSP_SVR_forecast_predicted_best_model[i,1]-BVSP_actual_returns_test[i,1])
  d<-as.data.frame(d)
  
  if (i==1) {
    BVSP_SVR_test_abs_values<-d  
  }
  else{
    BVSP_SVR_test_abs_values<-rbind(BVSP_SVR_test_abs_values,d)
  }
}

BVSP_SVR_test_abs_values

for (i in 1:nrow(BVSP_actual_returns_test)) {
  d<-abs(BVSP_SVR_forecast_predicted_best_model[i,1]-BVSP_actual_returns_test[i,1])**2
  d<-as.data.frame(d)
  
  if (i==1) {
    BVSP_SVR_test_squared_values<-d  
  }
  else{
    BVSP_SVR_test_squared_values<-rbind(BVSP_SVR_test_squared_values,d)
  }
}
BVSP_SVR_test_squared_values

# BVSP Error in training

BVSP_SVR_test_abs_error<-sum(BVSP_SVR_test_abs_values)/nrow(BVSP_SVR_test_abs_values)
BVSP_SVR_test_mean_squared_error<-sum(BVSP_SVR_test_squared_values)/nrow(BVSP_SVR_test_squared_values)
BVSP_SVR_test_mean_root_error<- sqrt(BVSP_SVR_test_mean_squared_error)

