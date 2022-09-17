#### GARCH MODEL

source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Preliminary analysis/N100 Preliminary Analysis.R")

#N100 autocorrelaction function
acf(N1001_return_t,lag.max = 10, main="Autocorrelation Function- Euronext 100")
#N100 GARCH Model
N1001_s<- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                     distribution.model = "std")
ctrl = list(RHO = 1,DELTA = 1e-8,MAJIT = 100,MINIT = 650,TOL = 1e-6)
N1001_m<-ugarchfit(data = N1001_return_t,spec =N1001_s,solver = 'solnp',solver.control = ctrl)
N1001_m
plot(N1001_m, which ="all")
plot(N1001_m, which=3)
names(N1001_m@fit)
min(N1001_return_t)    #ABS returns are grey and conditional SD is Blue
max(N1001_m@fit$sigma)
### measuring capacity prediction of GARCH on training set.
N1001_Garch_SD<-as.data.frame(N1001_m@fit$sigma) 
N1001_Garch_SD
mean(N1001_Garch_SD$`N1001_m@fit$sigma`)
mean(N1001_return_t)
sd(N1001_return_t)
dim(as.data.frame(N1001_return_t))
dim(N1001_Garch_SD)
N1001_return_t<-as.data.frame(N1001_return_t)
for (i in 1:nrow(N1001_return_t)) {
  a<- abs(N1001_return_t[i,1])-N1001_Garch_SD[i,1]
  a<-as.data.frame(a)
  
  if (i==1) {
    N1001_Garch_accuracy<-a
  }
  else{
    N1001_Garch_accuracy<- rbind(N1001_Garch_accuracy,a)
  }
}
N1001_Garch_accuracy<-as.data.frame(N1001_Garch_accuracy)
N1001_Garch_accuracy<-rbind('NA',N1001_Garch_accuracy)
N1001_Garch_SD<-rbind('NA',as.data.frame(N1001_Garch_SD))
N1001<-cbind(N1001,N1001_Garch_SD,N1001_Garch_accuracy)
colnames(N1001)<- c('Date','Close','Return','Volume','Garch Condicional SD','Garch Accuracy')
head(N1001)
N1001_Garch_accuracy_t<-as.data.frame(as.numeric(N1001[2:nrow(N1001),6]))
N1001_Garch_SD_t<-as.data.frame(as.numeric(N1001[2:nrow(N1001),5]))

plot(abs(N1001_return_t$N1001_return_t)~N1001[2:nrow(N1001),1],type='l',lwd=1
     ,main='Euronext 100 (GARCH) Actual Absolute Return vs. Conditional SD',xlab='Date',ylab='Volatility'
     ,col='grey')
lines(abs(as.numeric(N1001[2:nrow(N1001),5]))
      ~N1001[2:nrow(N1001),1],type = 'l',col='Orange',lwd=1) # We can see that Model not cover a lot

plot(as.numeric(N1001[2:nrow(N1001),6])~N1001[2:nrow(N1001),1], type='l',xlab='Date'
     ,ylab='Volatility',main='Euronext 100 Inneficiency of GARCH Model',col='grey')
abline(h=0,col='red',lwd=3,lty=3)


# N1002 Garch Forecast
N1002_f <- ugarchforecast(fitORspec = N1001_m, n.ahead = nrow(N1002))
plot(fitted(N1002_f))
plot(sigma(N1002_f)~N1002$Date,type='l',
     ylab='Conditional SD',xlab='Date',main='Forecasted GARCH Values for Euronext 100')
print(N1002_f,which='all')
N1002_returns<-diff(log(N1002$Close))
N1002_returns
N1002_Volatility<- sd(N1002_returns)
N1002_Volatility
N1002_forecast_sigma<-sigma(N1002_f)
N1002_forecast_sigma
N1002_returns<-as.data.frame(N1002_returns)
for (i in 1:nrow(N1002_returns)) {
  b<-abs(N1002_returns[i,1]-N1002_forecast_sigma[i,1])
  b<- as.data.frame(b)
  
  if (i==1) {
    N1002_Garch_accuracy<- b  
  }
  
  else{
    N1002_Garch_accuracy<-rbind(N1002_Garch_accuracy,b)
  }
}
N1002_Garch_accuracy<-as.data.frame(N1002_Garch_accuracy)
N1002_returns<-rbind(log(N1002[1,2]/N1001[nrow(N1001),2]),as.data.frame(N1002_returns))
N1002_Garch_accuracy<-rbind(N1002_returns[1,1]-N1002_forecast_sigma[1,1],N1002_Garch_accuracy)
N1002_forecast_sigma<-as.data.frame(N1002_forecast_sigma)
N1002<-cbind(N1002$Date,N1002$Close,N1002_returns,N1002$Volume,N1002_forecast_sigma,N1002_Garch_accuracy)
colnames(N1002)<-c('Date','Close','Return','Volume','Garch Forecasted SD','Garch Forecast Accuracy')
head(N1002)
hist(as.numeric(N1002[2:nrow(N1002),3]),breaks=seq(from=-0.15, to=0.15,by=0.025),
     xlab= "Daily Returns",main='Euronext 100 Returns from 21-02-2022 to 04-03-2022',
     las=1,col = "light grey")

plot(abs(as.numeric(N1002[2:nrow(N1002),3]))~N1002[2:nrow(N1002),1],type='l',lwd=1
     ,main='Euronext 100 (GARCH) Actual Absolute Return vs. Forecasted Conditional SD',xlab='Date',ylab='Volatility'
     ,col='black')
lines(abs(as.numeric(N1002[2:nrow(N1002),5]))
      ~N1002[2:nrow(N1002),1],type = 'l',col='Red',lwd=0.1) 
plot(as.numeric(N1002[2:nrow(N1002),6])~N1002[2:nrow(N1002),1],type='l',lwd=1,
     main='Euronext 100 Inneficienfy of GARCH Forecasting Model',xlab='Date',ylab='Volatility'
     ,col='black')

# N100 training set errors
N1001_squared_error<- N1001[,6]
for (i in 2:nrow(N1001)) {
  f<-abs(as.numeric(N1001[i,3])-as.numeric(N1001[i,5]))**2
  f<-as.data.frame(f)
  
  if (i==1) {
    N1001_squared_error<-f
  }
  else{
    N1001_squared_error<-rbind(N1001_squared_error,f)
  }
}
N1001_squared_error_col<-as.data.frame(N1001_squared_error)
N100_Garch_training_squared_error<-sum(as.numeric(N1001_squared_error[2:nrow(N1001_squared_error),1]))/(nrow(N1001)-1)
N100_Garch_training_root_error<- sqrt(N100_Garch_training_squared_error)
N100_Garch_training_abs_error<- sum(abs(as.numeric(N1001[2:nrow(N1001),6])))/(nrow(N1001)-1)

# N100 Test set error
for (i in 1:nrow(N1002_returns)) {
  g<-(abs(N1002_returns[i,1])-N1002_forecast_sigma[i,1])**2
  g<- as.data.frame(g)
  
  if (i==1) {
    N1002_squared_error<- g 
  }
  
  else{
    N1002_squared_error<-rbind(N1002_squared_error,g)
  }
}
N1002_squared_error<-as.data.frame(N1002_squared_error)
N1002_squared_error_value<-sum(as.numeric(N1002_squared_error[,1]))
N1002_sqrt_error_value<-sqrt(N1002_squared_error_value)
for (i in 1:nrow(N1002_returns)) {
  h<-abs(abs(N1002_returns[i,1])-N1002_forecast_sigma[i,1])
  h<- as.data.frame(h)
  
  if (i==1) {
    N1002_mean_error<- h
  }
  
  else{
    N1002_mean_error<-rbind(N1002_mean_error,h)
  }
}
N100_Garch_abs_error<-sum(N1002_mean_error$h)/nrow(N1002)
N100_Garch_squared_error<-N1002_squared_error_value/nrow(N1002)
N100_Garch_root_error<-N1002_sqrt_error_value

head(N1002)



