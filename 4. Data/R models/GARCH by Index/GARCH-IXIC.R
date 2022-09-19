#### GARCH MODEL

source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Preliminary analysis/IXIC Preliminary Analysis.R")

#IXIC autocorrelaction function
acf(IXIC1_return_t,lag.max = 10, main="Autocorrelation Function- NASDAQ")
#IXIC GARCH Model
IXIC1_s<- ugarchspec()
IXIC1_m<-ugarchfit(data = IXIC1_return_t,spec =IXIC1_s)
IXIC1_m
plot(IXIC1_m, which ="all")
plot(IXIC1_m, which=3)
names(IXIC1_m@fit)
min(IXIC1_return_t)    #ABS returns are grey and conditional SD is Blue
max(IXIC1_m@fit$sigma)
### measuring capacity prediction of GARCH on training set.
IXIC1_Garch_SD<-as.data.frame(IXIC1_m@fit$sigma) 
IXIC1_Garch_SD
mean(IXIC1_Garch_SD$`IXIC1_m@fit$sigma`)
mean(IXIC1_return_t)
sd(IXIC1_return_t)
dim(as.data.frame(IXIC1_return_t))
dim(IXIC1_Garch_SD)
IXIC1_return_t<-as.data.frame(IXIC1_return_t)
for (i in 1:nrow(IXIC1_return_t)) {
  a<- abs(IXIC1_return_t[i,1])-IXIC1_Garch_SD[i,1]
  a<-as.data.frame(a)
  
  if (i==1) {
    IXIC1_Garch_accuracy<-a
  }
  else{
    IXIC1_Garch_accuracy<- rbind(IXIC1_Garch_accuracy,a)
  }
}
IXIC1_Garch_accuracy<-as.data.frame(IXIC1_Garch_accuracy)
IXIC1_Garch_accuracy<-rbind('NA',IXIC1_Garch_accuracy)
IXIC1_Garch_SD<-rbind('NA',as.data.frame(IXIC1_Garch_SD))
IXIC1<-cbind(IXIC1,IXIC1_Garch_SD,IXIC1_Garch_accuracy)
colnames(IXIC1)<- c('Date','Close','Return','Volume','Garch Condicional SD','Garch Accuracy')
head(IXIC1)
IXIC1_Garch_accuracy_t<-as.data.frame(as.numeric(IXIC1[2:nrow(IXIC1),6]))
IXIC1_Garch_SD_t<-as.data.frame(as.numeric(IXIC1[2:nrow(IXIC1),5]))

plot(abs(IXIC1_return_t$IXIC1_return_t)~IXIC1[2:nrow(IXIC1),1],type='l',lwd=1
     ,main='NASDAQ (GARCH) Actual Absolute Return vs. Conditional SD',xlab='Date',ylab='Volatility'
     ,col='grey')
lines(abs(as.numeric(IXIC1[2:nrow(IXIC1),5]))
      ~IXIC1[2:nrow(IXIC1),1],type = 'l',col='Orange',lwd=1) # We can see that Model not cover a lot

plot(as.numeric(IXIC1[2:nrow(IXIC1),6])~IXIC1[2:nrow(IXIC1),1], type='l',xlab='Date'
     ,ylab='Volatility',main='NASDAQ Inneficiency of GARCH Model',col='grey')
abline(h=0,col='red',lwd=3,lty=3)


# IXIC2 Garch Forecast
IXIC2_f <- ugarchforecast(fitORspec = IXIC1_m, n.ahead = nrow(IXIC2))
plot(fitted(IXIC2_f))
plot(sigma(IXIC2_f)~IXIC2$Date,type='l',
     ylab='Conditional SD',xlab='Date',main='Forecasted GARCH Values for NASDAQ')
print(IXIC2_f,which='all')
IXIC2_returns<-diff(log(IXIC2$Close))
IXIC2_returns
IXIC2_Volatility<- sd(IXIC2_returns)
IXIC2_Volatility
IXIC2_forecast_sigma<-sigma(IXIC2_f)
IXIC2_forecast_sigma
IXIC2_returns<-as.data.frame(IXIC2_returns)
for (i in 1:nrow(IXIC2_returns)) {
  b<-abs(IXIC2_returns[i,1]-IXIC2_forecast_sigma[i,1])
  b<- as.data.frame(b)
  
  if (i==1) {
    IXIC2_Garch_accuracy<- b  
  }
  
  else{
    IXIC2_Garch_accuracy<-rbind(IXIC2_Garch_accuracy,b)
  }
}
IXIC2_Garch_accuracy<-as.data.frame(IXIC2_Garch_accuracy)
IXIC2_returns<-rbind(log(IXIC2[1,2]/IXIC1[nrow(IXIC1),2]),as.data.frame(IXIC2_returns))
IXIC2_Garch_accuracy<-rbind(IXIC2_returns[1,1]-IXIC2_forecast_sigma[1,1],IXIC2_Garch_accuracy)
IXIC2_forecast_sigma<-as.data.frame(IXIC2_forecast_sigma)
IXIC2<-cbind(IXIC2$Date,IXIC2$Close,IXIC2_returns,IXIC2$Volume,IXIC2_forecast_sigma,IXIC2_Garch_accuracy)
colnames(IXIC2)<-c('Date','Close','Return','Volume','Garch Forecasted SD','Garch Forecast Accuracy')
head(IXIC2)
hist(as.numeric(IXIC2[2:nrow(IXIC2),3]),breaks=seq(from=-0.15, to=0.15,by=0.025),
     xlab= "Daily Returns",main='NASDAQ Returns from 21-02-2022 to 04-03-2022',
     las=1,col = "light grey")

plot(abs(as.numeric(IXIC2[2:nrow(IXIC2),3]))~IXIC2[2:nrow(IXIC2),1],type='l',lwd=1
     ,main='NASDAQ (GARCH) Actual Absolute Return vs. Forecasted Conditional SD',xlab='Date',ylab='Volatility'
     ,col='black')
lines(abs(as.numeric(IXIC2[2:nrow(IXIC2),5]))
      ~IXIC2[2:nrow(IXIC2),1],type = 'l',col='Red',lwd=0.1) 
plot(as.numeric(IXIC2[2:nrow(IXIC2),6])~IXIC2[2:nrow(IXIC2),1],type='l',lwd=1,
     main='NASDAQ Inneficienfy of GARCH Forecasting Model',xlab='Date',ylab='Volatility'
     ,col='black')

# IXIC training set errors
IXIC1_squared_error<- IXIC1[,6]
for (i in 2:nrow(IXIC1)) {
  f<-abs(as.numeric(IXIC1[i,3])-as.numeric(IXIC1[i,5]))**2
  f<-as.data.frame(f)
  
  if (i==1) {
    IXIC1_squared_error<-f
  }
  else{
    IXIC1_squared_error<-rbind(IXIC1_squared_error,f)
  }
}
IXIC1_squared_error_col<-as.data.frame(IXIC1_squared_error)
IXIC_Garch_training_squared_error<-sum(as.numeric(IXIC1_squared_error[2:nrow(IXIC1_squared_error),1]))/(nrow(IXIC1)-1)
IXIC_Garch_training_root_error<- sqrt(IXIC_Garch_training_squared_error)
IXIC_Garch_training_abs_error<- sum(abs(as.numeric(IXIC1[2:nrow(IXIC1),6])))/(nrow(IXIC1)-1)

# IXIC Test set error
for (i in 1:nrow(IXIC2_returns)) {
  g<-(abs(IXIC2_returns[i,1])-IXIC2_forecast_sigma[i,1])**2
  g<- as.data.frame(g)
  
  if (i==1) {
    IXIC2_squared_error<- g 
  }
  
  else{
    IXIC2_squared_error<-rbind(IXIC2_squared_error,g)
  }
}
IXIC2_squared_error<-as.data.frame(IXIC2_squared_error)
IXIC2_squared_error_value<-sum(as.numeric(IXIC2_squared_error[,1]))
IXIC2_sqrt_error_value<-sqrt(IXIC2_squared_error_value)
for (i in 1:nrow(IXIC2_returns)) {
  h<-abs(abs(IXIC2_returns[i,1])-IXIC2_forecast_sigma[i,1])
  h<- as.data.frame(h)
  
  if (i==1) {
    IXIC2_mean_error<- h
  }
  
  else{
    IXIC2_mean_error<-rbind(IXIC2_mean_error,h)
  }
}
IXIC_Garch_abs_error<-sum(IXIC2_mean_error$h)/nrow(IXIC2)
IXIC_Garch_squared_error<-IXIC2_squared_error_value/nrow(IXIC2)
IXIC_Garch_root_error<-IXIC2_sqrt_error_value

head(IXIC2)



