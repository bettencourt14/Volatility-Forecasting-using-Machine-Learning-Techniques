#### GARCH MODEL

source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Preliminary analysis/BVSP Preliminary Analysis.R")

#BVSP autocorrelaction function
acf(BVSP1_return_t,lag.max = 10, main="Autocorrelation Function- IBOVESPA")
#BVSP GARCH Model
BVSP1_s<- ugarchspec()
BVSP1_m<-ugarchfit(data = BVSP1_return_t,spec =BVSP1_s)
BVSP1_m
plot(BVSP1_m, which ="all")
plot(BVSP1_m, which=3)
names(BVSP1_m@fit)
min(BVSP1_return_t)    #ABS returns are grey and conditional SD is Blue
max(BVSP1_m@fit$sigma)
### measuring capacity prediction of GARCH on training set.
BVSP1_Garch_SD<-as.data.frame(BVSP1_m@fit$sigma) 
BVSP1_Garch_SD
mean(BVSP1_Garch_SD$`BVSP1_m@fit$sigma`)
mean(BVSP1_return_t)
sd(BVSP1_return_t)
dim(as.data.frame(BVSP1_return_t))
dim(BVSP1_Garch_SD)
BVSP1_return_t<-as.data.frame(BVSP1_return_t)
for (i in 1:nrow(BVSP1_return_t)) {
  a<- abs(BVSP1_return_t[i,1])-BVSP1_Garch_SD[i,1]
  a<-as.data.frame(a)
  
  if (i==1) {
    BVSP1_Garch_accuracy<-a
  }
  else{
    BVSP1_Garch_accuracy<- rbind(BVSP1_Garch_accuracy,a)
  }
}
BVSP1_Garch_accuracy<-as.data.frame(BVSP1_Garch_accuracy)
BVSP1_Garch_accuracy<-rbind('NA',BVSP1_Garch_accuracy)
BVSP1_Garch_SD<-rbind('NA',as.data.frame(BVSP1_Garch_SD))
BVSP1<-cbind(BVSP1,BVSP1_Garch_SD,BVSP1_Garch_accuracy)
colnames(BVSP1)<- c('Date','Close','Return','Volume','Garch Condicional SD','Garch Accuracy')
head(BVSP1)
BVSP1_Garch_accuracy_t<-as.data.frame(as.numeric(BVSP1[2:nrow(BVSP1),6]))
BVSP1_Garch_SD_t<-as.data.frame(as.numeric(BVSP1[2:nrow(BVSP1),5]))

plot(abs(BVSP1_return_t$BVSP1_return_t)~BVSP1[2:nrow(BVSP1),1],type='l',lwd=1
     ,main='IBOVESPA (GARCH) Actual Absolute Return vs. Conditional SD',xlab='Date',ylab='Volatility'
     ,col='grey')
lines(abs(as.numeric(BVSP1[2:nrow(BVSP1),5]))
      ~BVSP1[2:nrow(BVSP1),1],type = 'l',col='Orange',lwd=1) # We can see that Model not cover a lot

plot(as.numeric(BVSP1[2:nrow(BVSP1),6])~BVSP1[2:nrow(BVSP1),1], type='l',xlab='Date'
     ,ylab='Volatility',main='IBOVESPA Inneficiency of GARCH Model',col='grey')
abline(h=0,col='red',lwd=3,lty=3)


# BVSP2 Garch Forecast
BVSP2_f <- ugarchforecast(fitORspec = BVSP1_m, n.ahead = nrow(BVSP2))
plot(fitted(BVSP2_f))
plot(sigma(BVSP2_f)~BVSP2$Date,type='l',
     ylab='Conditional SD',xlab='Date',main='Forecasted GARCH Values for IBOVESPA')
print(BVSP2_f,which='all')
BVSP2_returns<-diff(log(BVSP2$Close))
BVSP2_returns
BVSP2_Volatility<- sd(BVSP2_returns)
BVSP2_Volatility
BVSP2_forecast_sigma<-sigma(BVSP2_f)
BVSP2_forecast_sigma
BVSP2_returns<-as.data.frame(BVSP2_returns)
for (i in 1:nrow(BVSP2_returns)) {
  b<-abs(BVSP2_returns[i,1]-BVSP2_forecast_sigma[i,1])
  b<- as.data.frame(b)
  
  if (i==1) {
    BVSP2_Garch_accuracy<- b  
  }
  
  else{
    BVSP2_Garch_accuracy<-rbind(BVSP2_Garch_accuracy,b)
  }
}
BVSP2_Garch_accuracy<-as.data.frame(BVSP2_Garch_accuracy)
BVSP2_returns<-rbind(log(BVSP2[1,2]/BVSP1[nrow(BVSP1),2]),as.data.frame(BVSP2_returns))
BVSP2_Garch_accuracy<-rbind(BVSP2_returns[1,1]-BVSP2_forecast_sigma[1,1],BVSP2_Garch_accuracy)
BVSP2_forecast_sigma<-as.data.frame(BVSP2_forecast_sigma)
BVSP2<-cbind(BVSP2$Date,BVSP2$Close,BVSP2_returns,BVSP2$Volume,BVSP2_forecast_sigma,BVSP2_Garch_accuracy)
colnames(BVSP2)<-c('Date','Close','Return','Volume','Garch Forecasted SD','Garch Forecast Accuracy')
head(BVSP2)
hist(as.numeric(BVSP2[2:nrow(BVSP2),3]),breaks=seq(from=-0.15, to=0.15,by=0.025),
     xlab= "Daily Returns",main='IBOVESPA Returns from 21-02-2022 to 04-03-2022',
     las=1,col = "light grey")

plot(abs(as.numeric(BVSP2[2:nrow(BVSP2),3]))~BVSP2[2:nrow(BVSP2),1],type='l',lwd=1
     ,main='IBOVESPA (GARCH) Actual Absolute Return vs. Forecasted Conditional SD',xlab='Date',ylab='Volatility'
     ,col='black')
lines(abs(as.numeric(BVSP2[2:nrow(BVSP2),5]))
      ~BVSP2[2:nrow(BVSP2),1],type = 'l',col='Red',lwd=0.1) 
plot(as.numeric(BVSP2[2:nrow(BVSP2),6])~BVSP2[2:nrow(BVSP2),1],type='l',lwd=1,
     main='IBOVESPA Inneficienfy of GARCH Forecasting Model',xlab='Date',ylab='Volatility'
     ,col='black')

# BVSP training set errors
BVSP1_squared_error<- BVSP1[,6]
for (i in 2:nrow(BVSP1)) {
  f<-abs(as.numeric(BVSP1[i,3])-as.numeric(BVSP1[i,5]))**2
  f<-as.data.frame(f)
  
  if (i==1) {
    BVSP1_squared_error<-f
  }
  else{
    BVSP1_squared_error<-rbind(BVSP1_squared_error,f)
  }
}
BVSP1_squared_error_col<-as.data.frame(BVSP1_squared_error)
BVSP_Garch_training_squared_error<-sum(as.numeric(BVSP1_squared_error[2:nrow(BVSP1_squared_error),1]))/(nrow(BVSP1)-1)
BVSP_Garch_training_root_error<- sqrt(BVSP_Garch_training_squared_error)
BVSP_Garch_training_abs_error<- sum(abs(as.numeric(BVSP1[2:nrow(BVSP1),6])))/(nrow(BVSP1)-1)

# BVSP Test set error
for (i in 1:nrow(BVSP2_returns)) {
  g<-(abs(BVSP2_returns[i,1])-BVSP2_forecast_sigma[i,1])**2
  g<- as.data.frame(g)
  
  if (i==1) {
    BVSP2_squared_error<- g 
  }
  
  else{
    BVSP2_squared_error<-rbind(BVSP2_squared_error,g)
  }
}
BVSP2_squared_error<-as.data.frame(BVSP2_squared_error)
BVSP2_squared_error_value<-sum(as.numeric(BVSP2_squared_error[,1]))
BVSP2_sqrt_error_value<-sqrt(BVSP2_squared_error_value)
for (i in 1:nrow(BVSP2_returns)) {
  h<-abs(abs(BVSP2_returns[i,1])-BVSP2_forecast_sigma[i,1])
  h<- as.data.frame(h)
  
  if (i==1) {
    BVSP2_mean_error<- h
  }
  
  else{
    BVSP2_mean_error<-rbind(BVSP2_mean_error,h)
  }
}
BVSP_Garch_abs_error<-sum(BVSP2_mean_error$h)/nrow(BVSP2)
BVSP_Garch_squared_error<-BVSP2_squared_error_value/nrow(BVSP2)
BVSP_Garch_root_error<-BVSP2_sqrt_error_value

head(BVSP2)



