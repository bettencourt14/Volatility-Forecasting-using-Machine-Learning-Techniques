#### GARCH MODEL

source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Preliminary analysis/HSI Preliminary Analysis.R")

#HSI autocorrelaction function
acf(HSI1_return_t,lag.max = 10, main="Autocorrelation Function- Hang Seng")
#HSI GARCH Model
HSI1_s<- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                     distribution.model = "std")
ctrl = list(RHO = 1,DELTA = 1e-8,MAJIT = 100,MINIT = 650,TOL = 1e-6)
HSI1_m<-ugarchfit(data = HSI1_return_t,spec =HSI1_s,solver = 'solnp',solver.control = ctrl)
HSI1_m
plot(HSI1_m, which ="all")
plot(HSI1_m, which=3)
names(HSI1_m@fit)
min(HSI1_return_t)    #ABS returns are grey and conditional SD is Blue
max(HSI1_m@fit$sigma)
### measuring capacity prediction of GARCH on training set.
HSI1_Garch_SD<-as.data.frame(HSI1_m@fit$sigma) 
HSI1_Garch_SD
mean(HSI1_Garch_SD$`HSI1_m@fit$sigma`)
mean(HSI1_return_t)
sd(HSI1_return_t)
dim(as.data.frame(HSI1_return_t))
dim(HSI1_Garch_SD)
HSI1_return_t<-as.data.frame(HSI1_return_t)
for (i in 1:nrow(HSI1_return_t)) {
  a<- abs(HSI1_return_t[i,1])-HSI1_Garch_SD[i,1]
  a<-as.data.frame(a)
  
  if (i==1) {
    HSI1_Garch_accuracy<-a
  }
  else{
    HSI1_Garch_accuracy<- rbind(HSI1_Garch_accuracy,a)
  }
}
HSI1_Garch_accuracy<-as.data.frame(HSI1_Garch_accuracy)
HSI1_Garch_accuracy<-rbind('NA',HSI1_Garch_accuracy)
HSI1_Garch_SD<-rbind('NA',as.data.frame(HSI1_Garch_SD))
HSI1<-cbind(HSI1,HSI1_Garch_SD,HSI1_Garch_accuracy)
colnames(HSI1)<- c('Date','Close','Return','Volume','Garch Condicional SD','Garch Accuracy')
head(HSI1)
HSI1_Garch_accuracy_t<-as.data.frame(as.numeric(HSI1[2:nrow(HSI1),6]))
HSI1_Garch_SD_t<-as.data.frame(as.numeric(HSI1[2:nrow(HSI1),5]))

plot(abs(HSI1_return_t$HSI1_return_t)~HSI1[2:nrow(HSI1),1],type='l',lwd=1
     ,main='Hang Seng GARCH (1,1) Training Dataset',
     xlab='Date',ylab='Volatility'
     ,col='darkblue')
lines(abs(as.numeric(HSI1[2:nrow(HSI1),5]))
      ~HSI1[2:nrow(HSI1),1],type = 'l',col='darkorange',lwd=2) # We can see that Model not cover a lot
legend('topleft',
       legend = c('Absolute Returns','Conditional SD'),
       col = c('darkblue','darkorange'),
       pch = c(9,9))

plot(as.numeric(HSI1[2:nrow(HSI1),6])~HSI1[2:nrow(HSI1),1], type='l',xlab='Date'
     ,ylab='Volatility',main='Hang Seng Inneficiency of GARCH (1,1) Model on Training Dataset',
     col='darkblue')
abline(h=0,col='darkorange',lwd=4,lty=4)
legend('topleft',
       legend = c('Surplus/Deficit of Forecasted Volatility','Expected Volatility'),
       col=c('darkblue','darkorange'),
       pch=c(9,9))


# HSI2 Garch Forecast
HSI2_f <- ugarchforecast(fitORspec = HSI1_m, n.ahead = nrow(HSI2))
plot(fitted(HSI2_f))
plot(sigma(HSI2_f)~HSI2$Date,type='l',
     ylab='Conditional SD',xlab='Date',main='Forecasted GARCH Values for Hang Seng')
print(HSI2_f,which='all')
HSI2_returns<-diff(log(HSI2$Close))
HSI2_returns
HSI2_Volatility<- sd(HSI2_returns)
HSI2_Volatility
HSI2_forecast_sigma<-sigma(HSI2_f)
HSI2_forecast_sigma
HSI2_returns<-as.data.frame(HSI2_returns)
for (i in 1:nrow(HSI2_returns)) {
  b<-abs(HSI2_returns[i,1]-HSI2_forecast_sigma[i,1])
  b<- as.data.frame(b)
  
  if (i==1) {
    HSI2_Garch_accuracy<- b  
  }
  
  else{
    HSI2_Garch_accuracy<-rbind(HSI2_Garch_accuracy,b)
  }
}
HSI2_Garch_accuracy<-as.data.frame(HSI2_Garch_accuracy)
HSI2_returns<-rbind(log(HSI2[1,2]/HSI1[nrow(HSI1),2]),as.data.frame(HSI2_returns))
HSI2_Garch_accuracy<-rbind(HSI2_returns[1,1]-HSI2_forecast_sigma[1,1],HSI2_Garch_accuracy)
HSI2_forecast_sigma<-as.data.frame(HSI2_forecast_sigma)
HSI2<-cbind(HSI2$Date,HSI2$Close,HSI2_returns,HSI2$Volume,HSI2_forecast_sigma,HSI2_Garch_accuracy)
colnames(HSI2)<-c('Date','Close','Return','Volume','Garch Forecasted SD','Garch Forecast Accuracy')
head(HSI2)
hist(as.numeric(HSI2[2:nrow(HSI2),3]),breaks=seq(from=-0.15, to=0.15,by=0.025),
     xlab= "Daily Returns",main='Hang Seng Returns from 21-02-2022 to 04-03-2022',
     las=1,col = "light grey")

plot(abs(as.numeric(HSI2[2:nrow(HSI2),3]))~HSI2[2:nrow(HSI2),1],type='l',lwd=1
     ,main='Hang Seng GARCH (1,1) Test Set',
     xlab='Date',ylab='Volatility'
     ,col='darkblue')
lines(abs(as.numeric(HSI2[2:nrow(HSI2),5]))
      ~HSI2[2:nrow(HSI2),1],type = 'l',col='darkorange',lwd=0.1) 
legend('topleft',
       legend = c('Absolute Returns','Conditional SD'),
       col = c('darkblue','darkorange'),
       pch = c(9,9))

plot(as.numeric(HSI2[2:nrow(HSI2),6])~HSI2[2:nrow(HSI2),1],type='l',lwd=1,
     main='Hang Seng Inneficienfy of GARCH Forecasting Model',xlab='Date',ylab='Volatility'
     ,col='black')

# HSI training set errors
HSI1_squared_error<- HSI1[,6]
for (i in 2:nrow(HSI1)) {
  f<-abs(as.numeric(HSI1[i,3])-as.numeric(HSI1[i,5]))**2
  f<-as.data.frame(f)
  
  if (i==1) {
    HSI1_squared_error<-f
  }
  else{
    HSI1_squared_error<-rbind(HSI1_squared_error,f)
  }
}
HSI1_squared_error_col<-as.data.frame(HSI1_squared_error)
HSI_Garch_training_squared_error<-sum(as.numeric(HSI1_squared_error[2:nrow(HSI1_squared_error),1]))/(nrow(HSI1)-1)
HSI_Garch_training_root_error<- sqrt(HSI_Garch_training_squared_error)
HSI_Garch_training_abs_error<- sum(abs(as.numeric(HSI1[2:nrow(HSI1),6])))/(nrow(HSI1)-1)

# HSI Test set error
for (i in 1:nrow(HSI2_returns)) {
  g<-(abs(HSI2_returns[i,1])-HSI2_forecast_sigma[i,1])**2
  g<- as.data.frame(g)
  
  if (i==1) {
    HSI2_squared_error<- g 
  }
  
  else{
    HSI2_squared_error<-rbind(HSI2_squared_error,g)
  }
}
HSI2_squared_error<-as.data.frame(HSI2_squared_error)
HSI2_squared_error_value<-sum(as.numeric(HSI2_squared_error[,1]))
HSI2_sqrt_error_value<-sqrt(HSI2_squared_error_value)
for (i in 1:nrow(HSI2_returns)) {
  h<-abs(abs(HSI2_returns[i,1])-HSI2_forecast_sigma[i,1])
  h<- as.data.frame(h)
  
  if (i==1) {
    HSI2_mean_error<- h
  }
  
  else{
    HSI2_mean_error<-rbind(HSI2_mean_error,h)
  }
}
HSI_Garch_abs_error<-sum(HSI2_mean_error$h)/nrow(HSI2)
HSI_Garch_squared_error<-HSI2_squared_error_value/nrow(HSI2)
HSI_Garch_root_error<-HSI2_sqrt_error_value

head(HSI2)



