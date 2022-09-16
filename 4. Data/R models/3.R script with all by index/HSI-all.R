# HSI big source
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
HSI <- read_excel(paste0(getwd(),"/HSI.xlsx"),
                   col_types = c("date", "numeric", "numeric"))
HSI<-as.data.frame(HSI)
print(HSI)
#Plot HSI prices
plot(HSI$Close ~HSI$Date,type="l",col="black",axes="T",xlab="Date",ylab="Closing Prices", 
     main="Hang Seng Daily Prices",lwd=1)
# Missing Values 
length(which((HSI$Close==0)))
HSI$Close[HSI$Close==0]<-NA
colSums(is.na(HSI))
HSI<- na.omit(HSI)
which(is.na(HSI))
#Remove 2022-03-01 until 2022-03-30
HSI1_date<- HSI[HSI$Date>='2015-01-01'&
                    HSI$Date<'2022-02-20',]
HSI1<-as.data.frame(HSI1_date)
head(HSI1)
HSI2<-as.data.frame(HSI[HSI$Date>='2022-02-20'&
                            HSI$Date<='2022-03-04',])
print(HSI2)
tail(HSI2)
#Daily Returns
HSI1_x <- HSI1$Close
HSI1_return<- diff(log(HSI1_x))
which(is.na(HSI1_return))
print(HSI1_return)
mean(HSI1_return)
sd(HSI1_return)
summary(HSI1_return)
HSI1_return<-as.data.frame(HSI1_return)
HSI1_return<-rbind('NA',HSI1_return)
HSI1<-cbind(HSI1$Date,HSI1$Close,HSI1_return,HSI1$Volume)
head(HSI1)
HSI1_return_t<- as.numeric(HSI1[2:nrow(HSI1),3])
hist(HSI1_return_t, breaks=seq(from=-0.2, to=0.2,by=0.002),
     xlab= "Daily Returns",main=paste0('Hang Seng Returns from 01-01-2015 until 20-02-2022'),
     las=1,col = "grey")

plot(HSI1_return_t~HSI1[2:nrow(HSI1),1],
     type='l',lwd=0.5, main='Hang Seng Volatility from 01-01-2015 until 20-02-2022',
     xlab='Date',ylab='Volatility')
abline(h=0, col='red', lty=3,lwd=3)

#table with daily and annualized return Statistics
HSI1_return_annualized= mean(HSI1_return_t)*252*100
HSI1_return_annualized
HSI1_volatility_annualized= sd(HSI1_return_t)*sqrt(252)*100
HSI1_volatility_annualized
HSI1_Sharpe_Ratio<- HSI1_return_annualized/HSI1_volatility_annualized
HSI1_Sharpe_Ratio



#HSI Monte Carlo Simulation 

nsim=nrow(HSI2)+1
HSI1_daily_return<- HSI1_return_annualized/100/252
HSI1_daily_volatility<-HSI1_volatility_annualized/100/sqrt(252)
HSI1_T<- nrow(HSI2)+1
HSI1_T
HSI1_S0=as.numeric(HSI1[nrow(HSI1),2])
HSI1_S0

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
  a <- mc_function(nsim =nsim,N=HSI1_T,daily_mean=HSI1_daily_return,
                   daily_sd = HSI1_daily_volatility, S0 = HSI1_S0)
  a <- as.data.frame(a)
  if(i==1)
  {
    HSI1_MC_results <- a 
  }
  else {
    HSI1_MC_results <- cbind(HSI1_MC_results,a)
  }
  
}


HSI1_MC_results[1,]<- HSI1_S0
HSI1_MC_results

for (i in 1:ncol(HSI1_MC_results)){
  colnames(HSI1_MC_results)[i] <- paste0('Sim ',i)
}

for(i in 1:ncol(HSI1_MC_results))
{
  b <- diff(log(HSI1_MC_results[,i]))
  b <- as.data.frame(b)
  if(i==1)
  {
    HSI1_MC_returns <- b
  }
  else {
    HSI1_MC_returns <- cbind(HSI1_MC_returns,b)
  }
  
}

for (i in 1:ncol(HSI1_MC_returns)){
  colnames(HSI1_MC_returns)[i] <- paste0('Sim ',i)
}


print(HSI1_MC_returns)
HSI1_MC_returns<- na.omit(HSI1_MC_returns)

HSI2_actual_prices<-c(HSI1_S0,HSI2$Close)
HSI2_actual_prices
HSI2_actual_returns<-diff(log(HSI2_actual_prices))
HSI2_actual_returns<- as.data.frame(HSI2_actual_returns)
dim(HSI2_actual_returns)

### Error for each day of the model by assuming daily price as correct one

HSI2_daily_error_aux<- HSI1_MC_returns
for (i in 1:ncol(HSI2_daily_error_aux)) {
  HSI2_daily_error_aux[,i]<-HSI2_actual_returns
}
HSI2_daily_error_aux<-as.data.frame(HSI2_daily_error_aux)

for (i in 1:ncol(HSI2_daily_error_aux)) {
  d<- abs(HSI1_MC_returns[,i]-HSI2_daily_error_aux[,i])
  d<- as.data.frame(d)
  
  if (i==1) {
    HSI_error<-d
    
  }
  else{
    HSI_error<-cbind(HSI_error,d)
  }
}

HSI_error    #use this for Error measurmments

for (i in 1:ncol(HSI_error)){
  colnames(HSI_error)[i] <- paste0('Sim ',i)
}

HSI_error<-as.data.frame(HSI_error)
HSI_error_means<-rowMeans(HSI_error)
HSI2_actual_returns
HSI2_print<-cbind(HSI2_actual_returns,HSI_error_means)
HSI2_print
hist(HSI2_actual_returns$HSI2_actual_returns,col='light grey',
     breaks=seq(from=-0.2, to=0.2,by=0.025),
     xlab= "Daily Returns",main="Actual Returns of Hang Seng for 21-02-2022 until 04-03-2022",
     las=1,ylim=c(0,8))
plot(HSI2_actual_returns$HSI2_actual_returns~HSI2$Date,type='l',col='black',ylim=c(-0.15,0.15), 
     xlab="Simulated Days",ylab="Return",lwd=1, main='Hang Seng
     Actual vs. Average Simulated Returns')
lines(HSI_error_means~HSI2$Date,type='l',col='Orange')

##  Absolute difference between the HSI2_Actual_Returns and HSI_error_menans
for (i in 1:nrow(as.data.frame(HSI_error_means))) {
  
  e<- abs(HSI2_print[i,1]-HSI2_print[i,2])
  e<-as.data.frame(e)
  
  if (i==1) {
    HSI_abs_error<-e  
  }
  
  else{
    HSI_abs_error<- rbind(HSI_abs_error,e)
  }
}
e
HSI_abs_error

plot(HSI_abs_error$e~HSI2$Date,xlab='Days',ylab='Difference to actual', main='Hang Seng Monte Carlo Innefiency',
     type='l',lwd=2)

## Absolute squared error

for (i in 1:nrow(as.data.frame(HSI_error_means))) {
  
  g<- abs(HSI2_print[i,1]-HSI2_print[i,2])**2
  g<-as.data.frame(g)
  
  if (i==1) {
    HSI_mean_error<-g  
  }
  else{
    HSI_mean_error<-rbind(HSI_mean_error,g)
  }
}
g
HSI_mean_error
# Mean Errors
HSI_MC_absoulte_error<- sum(HSI_abs_error$e)/nrow(HSI_abs_error)
HSI_MC_squared_mean_error<- sum(HSI_mean_error$g)/nrow(HSI_mean_error)
HSI_MC_root_mean_error<-sqrt(HSI_MC_squared_mean_error)

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
     ,main='Hang Seng (GARCH) Actual Absolute Return vs. Conditional SD',xlab='Date',ylab='Volatility'
     ,col='grey')
lines(abs(as.numeric(HSI1[2:nrow(HSI1),5]))
      ~HSI1[2:nrow(HSI1),1],type = 'l',col='Orange',lwd=1) # We can see that Model not cover a lot

plot(as.numeric(HSI1[2:nrow(HSI1),6])~HSI1[2:nrow(HSI1),1], type='l',xlab='Date'
     ,ylab='Volatility',main='Hang Seng Inneficiency of GARCH Model',col='grey')
abline(h=0,col='red',lwd=3,lty=3)


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
     ,main='Hang Seng (GARCH) Actual Absolute Return vs. Forecasted Conditional SD',xlab='Date',ylab='Volatility'
     ,col='black')
lines(abs(as.numeric(HSI2[2:nrow(HSI2),5]))
      ~HSI2[2:nrow(HSI2),1],type = 'l',col='Red',lwd=0.1) 
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


