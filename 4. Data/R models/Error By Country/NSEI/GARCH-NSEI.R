#### GARCH MODEL

source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Preliminary analysis/NSEI Preliminary Analysis.R")

#NSEI autocorrelaction function
acf(NSEI1_return_t,lag.max = 10, main="Autocorrelation Function- NIFTY 50")
#NSEI GARCH Model
NSEI1_s<- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                     distribution.model = "std")
ctrl = list(RHO = 1,DELTA = 1e-8,MAJIT = 100,MINIT = 650,TOL = 1e-6)
NSEI1_m<-ugarchfit(data = NSEI1_return_t,spec =NSEI1_s,solver = 'solnp',solver.control = ctrl)
NSEI1_m
plot(NSEI1_m, which ="all")
plot(NSEI1_m, which=3)
names(NSEI1_m@fit)
min(NSEI1_return_t)    #ABS returns are grey and conditional SD is Blue
max(NSEI1_m@fit$sigma)
### measuring capacity prediction of GARCH on training set.
NSEI1_Garch_SD<-as.data.frame(NSEI1_m@fit$sigma) 
NSEI1_Garch_SD
mean(NSEI1_Garch_SD$`NSEI1_m@fit$sigma`)
mean(NSEI1_return_t)
sd(NSEI1_return_t)
dim(as.data.frame(NSEI1_return_t))
dim(NSEI1_Garch_SD)
NSEI1_return_t<-as.data.frame(NSEI1_return_t)
for (i in 1:nrow(NSEI1_return_t)) {
  a<- abs(NSEI1_return_t[i,1])-NSEI1_Garch_SD[i,1]
  a<-as.data.frame(a)
  
  if (i==1) {
    NSEI1_Garch_accuracy<-a
  }
  else{
    NSEI1_Garch_accuracy<- rbind(NSEI1_Garch_accuracy,a)
  }
}
NSEI1_Garch_accuracy<-as.data.frame(NSEI1_Garch_accuracy)
NSEI1_Garch_accuracy<-rbind('NA',NSEI1_Garch_accuracy)
NSEI1_Garch_SD<-rbind('NA',as.data.frame(NSEI1_Garch_SD))
NSEI1<-cbind(NSEI1,NSEI1_Garch_SD,NSEI1_Garch_accuracy)
colnames(NSEI1)<- c('Date','Close','Return','Volume','Garch Condicional SD','Garch Accuracy')
head(NSEI1)
NSEI1_Garch_accuracy_t<-as.data.frame(as.numeric(NSEI1[2:nrow(NSEI1),6]))
NSEI1_Garch_SD_t<-as.data.frame(as.numeric(NSEI1[2:nrow(NSEI1),5]))

plot(abs(NSEI1_return_t$NSEI1_return_t)~NSEI1[2:nrow(NSEI1),1],type='l',lwd=1
     ,main='NIFTY 50 GARCH (1,1) Training Dataset',
     xlab='Date',ylab='Volatility'
     ,col='darkblue')
lines(abs(as.numeric(NSEI1[2:nrow(NSEI1),5]))
      ~NSEI1[2:nrow(NSEI1),1],type = 'l',col='darkorange',lwd=2) # We can see that Model not cover a lot
legend('topleft',
       legend = c('Absolute Returns','Conditional SD'),
       col = c('darkblue','darkorange'),
       pch = c(9,9))

plot(as.numeric(NSEI1[2:nrow(NSEI1),6])~NSEI1[2:nrow(NSEI1),1], type='l',xlab='Date'
     ,ylab='Volatility',main='NIFTY 50 Inneficiency of GARCH (1,1) Model on Training Dataset',
     col='darkblue')
abline(h=0,col='darkorange',lwd=4,lty=4)
legend('topleft',
       legend = c('Surplus/Deficit of Forecasted Volatility','Expected Volatility'),
       col=c('darkblue','darkorange'),
       pch=c(9,9))


# NSEI2 Garch Forecast
NSEI2_f <- ugarchforecast(fitORspec = NSEI1_m, n.ahead = nrow(NSEI2))
plot(fitted(NSEI2_f))
plot(sigma(NSEI2_f)~NSEI2$Date,type='l',
     ylab='Conditional SD',xlab='Date',main='Forecasted GARCH Values for NIFTY 50')
print(NSEI2_f,which='all')
NSEI2_returns<-diff(log(NSEI2$Close))
NSEI2_returns
NSEI2_Volatility<- sd(NSEI2_returns)
NSEI2_Volatility
NSEI2_forecast_sigma<-sigma(NSEI2_f)
NSEI2_forecast_sigma
NSEI2_returns<-as.data.frame(NSEI2_returns)
for (i in 1:nrow(NSEI2_returns)) {
  b<-abs(NSEI2_returns[i,1]-NSEI2_forecast_sigma[i,1])
  b<- as.data.frame(b)
  
  if (i==1) {
    NSEI2_Garch_accuracy<- b  
  }
  
  else{
    NSEI2_Garch_accuracy<-rbind(NSEI2_Garch_accuracy,b)
  }
}
NSEI2_Garch_accuracy<-as.data.frame(NSEI2_Garch_accuracy)
NSEI2_returns<-rbind(log(NSEI2[1,2]/NSEI1[nrow(NSEI1),2]),as.data.frame(NSEI2_returns))
NSEI2_Garch_accuracy<-rbind(NSEI2_returns[1,1]-NSEI2_forecast_sigma[1,1],NSEI2_Garch_accuracy)
NSEI2_forecast_sigma<-as.data.frame(NSEI2_forecast_sigma)
NSEI2<-cbind(NSEI2$Date,NSEI2$Close,NSEI2_returns,NSEI2$Volume,NSEI2_forecast_sigma,NSEI2_Garch_accuracy)
colnames(NSEI2)<-c('Date','Close','Return','Volume','Garch Forecasted SD','Garch Forecast Accuracy')
head(NSEI2)
hist(as.numeric(NSEI2[2:nrow(NSEI2),3]),breaks=seq(from=-0.15, to=0.15,by=0.025),
     xlab= "Daily Returns",main='NIFTY 50 Returns from 21-02-2022 to 04-03-2022',
     las=1,col = "light grey")

plot(abs(as.numeric(NSEI2[2:nrow(NSEI2),3]))~NSEI2[2:nrow(NSEI2),1],type='l',lwd=1
     ,main='NIFTY 50 GARCH (1,1) Test Set',
     xlab='Date',ylab='Volatility'
     ,col='darkblue')
lines(abs(as.numeric(NSEI2[2:nrow(NSEI2),5]))
      ~NSEI2[2:nrow(NSEI2),1],type = 'l',col='darkorange',lwd=0.1) 
legend('topright',
       legend = c('Absolute Returns','Conditional SD'),
       col = c('darkblue','darkorange'),
       pch = c(9,9))

plot(as.numeric(NSEI2[2:nrow(NSEI2),6])~NSEI2[2:nrow(NSEI2),1],type='l',lwd=1,
     main='NIFTY 50 Inneficienfy of GARCH Forecasting Model',xlab='Date',ylab='Volatility'
     ,col='black')

# NSEI training set errors
NSEI1_squared_error<- NSEI1[,6]
for (i in 2:nrow(NSEI1)) {
  f<-abs(as.numeric(NSEI1[i,3])-as.numeric(NSEI1[i,5]))**2
  f<-as.data.frame(f)
  
  if (i==1) {
    NSEI1_squared_error<-f
  }
  else{
    NSEI1_squared_error<-rbind(NSEI1_squared_error,f)
  }
}
NSEI1_squared_error_col<-as.data.frame(NSEI1_squared_error)
NSEI_Garch_training_squared_error<-sum(as.numeric(NSEI1_squared_error[2:nrow(NSEI1_squared_error),1]))/(nrow(NSEI1)-1)
NSEI_Garch_training_root_error<- sqrt(NSEI_Garch_training_squared_error)
NSEI_Garch_training_abs_error<- sum(abs(as.numeric(NSEI1[2:nrow(NSEI1),6])))/(nrow(NSEI1)-1)

# NSEI Test set error
for (i in 1:nrow(NSEI2_returns)) {
  g<-(abs(NSEI2_returns[i,1])-NSEI2_forecast_sigma[i,1])**2
  g<- as.data.frame(g)
  
  if (i==1) {
    NSEI2_squared_error<- g 
  }
  
  else{
    NSEI2_squared_error<-rbind(NSEI2_squared_error,g)
  }
}
NSEI2_squared_error<-as.data.frame(NSEI2_squared_error)
NSEI2_squared_error_value<-sum(as.numeric(NSEI2_squared_error[,1]))
NSEI2_sqrt_error_value<-sqrt(NSEI2_squared_error_value)
for (i in 1:nrow(NSEI2_returns)) {
  h<-abs(abs(NSEI2_returns[i,1])-NSEI2_forecast_sigma[i,1])
  h<- as.data.frame(h)
  
  if (i==1) {
    NSEI2_mean_error<- h
  }
  
  else{
    NSEI2_mean_error<-rbind(NSEI2_mean_error,h)
  }
}
NSEI_Garch_abs_error<-sum(NSEI2_mean_error$h)/nrow(NSEI2)
NSEI_Garch_squared_error<-NSEI2_squared_error_value/nrow(NSEI2)
NSEI_Garch_root_error<-NSEI2_sqrt_error_value

head(NSEI2)



