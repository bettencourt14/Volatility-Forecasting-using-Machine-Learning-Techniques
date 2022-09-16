# N100 big source
options(scipen = 999)
rm(list=ls(all=TRUE))
#Libraries
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(rugarch)
library(readxl)
library(ggplot2)
library(readr)
library(dplyr)
library(scales)
library(zoo)
library(gmodels)
library(Metrics)
library(httr)


setwd("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data")

#Data from 10/01/2015 until 04/03/2022, download from Yahoo Finance

#Daily prices for our indexes
N100 <- read_excel(paste0(getwd(),"/N100.xlsx"),
                   col_types = c("date", "numeric", "numeric"))
N100<-as.data.frame(N100)
print(N100)
#Plot N100 prices
plot(N100$Close ~N100$Date,type="l",col="black",axes="T",xlab="Date",ylab="Closing Prices", 
     main="Euronext 100 Daily Prices",lwd=1)
# Missing Values 
length(which((N100$Close==0)))
N100$Close[N100$Close==0]<-NA
colSums(is.na(N100))
N100<- na.omit(N100)
which(is.na(N100))
#Remove 2022-03-01 until 2022-03-30
N1001_date<- N100[N100$Date>='2015-01-01'&
                    N100$Date<'2022-02-20',]
N1001<-as.data.frame(N1001_date)
head(N1001)
N1002<-as.data.frame(N100[N100$Date>='2022-02-20'&
                            N100$Date<='2022-03-04',])
print(N1002)
tail(N1002)
#Daily Returns
N1001_x <- N1001$Close
N1001_return<- diff(log(N1001_x))
which(is.na(N1001_return))
print(N1001_return)
mean(N1001_return)
sd(N1001_return)
summary(N1001_return)
N1001_return<-as.data.frame(N1001_return)
N1001_return<-rbind('NA',N1001_return)
N1001<-cbind(N1001$Date,N1001$Close,N1001_return,N1001$Volume)
head(N1001)
N1001_return_t<- as.numeric(N1001[2:nrow(N1001),3])
hist(N1001_return_t, breaks=seq(from=-0.2, to=0.2,by=0.002),
     xlab= "Daily Returns",main=paste0('Euronext 100 Returns from 01-01-2015 until 20-02-2022'),
     las=1,col = "grey")

plot(N1001_return_t~N1001[2:nrow(N1001),1],
     type='l',lwd=0.5, main='Euronext 100 Volatility from 01-01-2015 until 20-02-2022',
     xlab='Date',ylab='Volatility')
abline(h=0, col='red', lty=3,lwd=3)

#table with daily and annualized return Statistics
N1001_return_annualized= mean(N1001_return_t)*252*100
N1001_return_annualized
N1001_volatility_annualized= sd(N1001_return_t)*sqrt(252)*100
N1001_volatility_annualized
N1001_Sharpe_Ratio<- N1001_return_annualized/N1001_volatility_annualized
N1001_Sharpe_Ratio



#N100 Monte Carlo Simulation 

nsim=nrow(N1002)+1
N1001_daily_return<- N1001_return_annualized/100/252
N1001_daily_volatility<-N1001_volatility_annualized/100/sqrt(252)
N1001_T<- nrow(N1002)+1
N1001_T
N1001_S0=as.numeric(N1001[nrow(N1001),2])
N1001_S0

mc_function=function(nsim,N,daily_mean,daily_sd,S0)
{
  Z=rnorm(nsim,0,1)
  WT=sqrt(N)*Z
  ST=S0*exp((daily_mean-0.5*daily_sd^2)*N+daily_sd*WT)
  
  
  output_list=list(ST=ST)
  output_list
}

set.seed(994)

for (i in 1:1000)
{
  a <- mc_function(nsim =nsim,N=N1001_T,daily_mean=N1001_daily_return,
                   daily_sd = N1001_daily_volatility, S0 = N1001_S0)
  a <- as.data.frame(a)
  if(i==1)
  {
    N1001_MC_results <- a 
  }
  else {
    N1001_MC_results <- cbind(N1001_MC_results,a)
  }
  
}


N1001_MC_results[1,]<- N1001_S0
N1001_MC_results

for (i in 1:ncol(N1001_MC_results)){
  colnames(N1001_MC_results)[i] <- paste0('Sim ',i)
}

for(i in 1:ncol(N1001_MC_results))
{
  b <- diff(log(N1001_MC_results[,i]))
  b <- as.data.frame(b)
  if(i==1)
  {
    N1001_MC_returns <- b
  }
  else {
    N1001_MC_returns <- cbind(N1001_MC_returns,b)
  }
  
}

for (i in 1:ncol(N1001_MC_returns)){
  colnames(N1001_MC_returns)[i] <- paste0('Sim ',i)
}


print(N1001_MC_returns)
N1001_MC_returns<- na.omit(N1001_MC_returns)

N1002_actual_prices<-c(N1001_S0,N1002$Close)
N1002_actual_prices
N1002_actual_returns<-diff(log(N1002_actual_prices))
N1002_actual_returns<- as.data.frame(N1002_actual_returns)
dim(N1002_actual_returns)

### Error for each day of the model by assuming daily price as correct one

N1002_daily_error_aux<- N1001_MC_returns
for (i in 1:ncol(N1002_daily_error_aux)) {
  N1002_daily_error_aux[,i]<-N1002_actual_returns
}
N1002_daily_error_aux<-as.data.frame(N1002_daily_error_aux)

for (i in 1:ncol(N1002_daily_error_aux)) {
  d<- abs(N1001_MC_returns[,i]-N1002_daily_error_aux[,i])
  d<- as.data.frame(d)
  
  if (i==1) {
    N100_error<-d
    
  }
  else{
    N100_error<-cbind(N100_error,d)
  }
}

N100_error    #use this for Error measurmments

for (i in 1:ncol(N100_error)){
  colnames(N100_error)[i] <- paste0('Sim ',i)
}

N100_error<-as.data.frame(N100_error)
N100_error_means<-rowMeans(N100_error)
N1002_actual_returns
N1002_print<-cbind(N1002_actual_returns,N100_error_means)
N1002_print
hist(N1002_actual_returns$N1002_actual_returns,col='light grey',
     breaks=seq(from=-0.2, to=0.2,by=0.025),
     xlab= "Daily Returns",main="Actual Returns of Euronext 100 for 21-02-2022 until 04-03-2022",
     las=1,ylim=c(0,8))
plot(N1002_actual_returns$N1002_actual_returns~N1002$Date,type='l',col='black',ylim=c(-0.15,0.15), 
     xlab="Simulated Days",ylab="Return",lwd=1, main='Euronext 100
     Actual vs. Average Simulated Returns')
lines(N100_error_means~N1002$Date,type='l',col='Orange')

##  Absolute difference between the N1002_Actual_Returns and N100_error_menans
for (i in 1:nrow(as.data.frame(N100_error_means))) {
  
  e<- abs(N1002_print[i,1]-N1002_print[i,2])
  e<-as.data.frame(e)
  
  if (i==1) {
    N100_abs_error<-e  
  }
  
  else{
    N100_abs_error<- rbind(N100_abs_error,e)
  }
}
e
N100_abs_error

plot(N100_abs_error$e~N1002$Date,xlab='Days',ylab='Difference to actual', main='Euronext 100 Monte Carlo Innefiency',
     type='l',lwd=2)

## Absolute squared error

for (i in 1:nrow(as.data.frame(N100_error_means))) {
  
  g<- abs(N1002_print[i,1]-N1002_print[i,2])**2
  g<-as.data.frame(g)
  
  if (i==1) {
    N100_mean_error<-g  
  }
  else{
    N100_mean_error<-rbind(N100_mean_error,g)
  }
}
g
N100_mean_error
# Mean Errors
N100_MC_absoulte_error<- sum(N100_abs_error$e)/nrow(N100_abs_error)
N100_MC_squared_mean_error<- sum(N100_mean_error$g)/nrow(N100_mean_error)
N100_MC_root_mean_error<-sqrt(N100_MC_squared_mean_error)

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


