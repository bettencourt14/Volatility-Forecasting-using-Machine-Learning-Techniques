# NSEI big source
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
NSEI <- read_excel(paste0(getwd(),"/NSEI.xlsx"),
                   col_types = c("date", "numeric", "numeric"))
NSEI<-as.data.frame(NSEI)
print(NSEI)
#Plot NSEI prices
plot(NSEI$Close ~NSEI$Date,type="l",col="black",axes="T",xlab="Date",ylab="Closing Prices", 
     main="NIFTY 50 Daily Prices",lwd=1)
# Missing Values 
length(which((NSEI$Close==0)))
NSEI$Close[NSEI$Close==0]<-NA
colSums(is.na(NSEI))
NSEI<- na.omit(NSEI)
which(is.na(NSEI))
#Remove 2022-03-01 until 2022-03-30
NSEI1_date<- NSEI[NSEI$Date>='2015-01-01'&
                    NSEI$Date<'2022-02-20',]
NSEI1<-as.data.frame(NSEI1_date)
head(NSEI1)
NSEI2<-as.data.frame(NSEI[NSEI$Date>='2022-02-20'&
                            NSEI$Date<='2022-03-04',])
print(NSEI2)
tail(NSEI2)
#Daily Returns
NSEI1_x <- NSEI1$Close
NSEI1_return<- diff(log(NSEI1_x))
which(is.na(NSEI1_return))
print(NSEI1_return)
mean(NSEI1_return)
sd(NSEI1_return)
summary(NSEI1_return)
NSEI1_return<-as.data.frame(NSEI1_return)
NSEI1_return<-rbind('NA',NSEI1_return)
NSEI1<-cbind(NSEI1$Date,NSEI1$Close,NSEI1_return,NSEI1$Volume)
head(NSEI1)
NSEI1_return_t<- as.numeric(NSEI1[2:nrow(NSEI1),3])
hist(NSEI1_return_t, breaks=seq(from=-0.2, to=0.2,by=0.002),
     xlab= "Daily Returns",main=paste0('NIFTY 50 Returns from 01-01-2015 until 20-02-2022'),
     las=1,col = "grey")

plot(NSEI1_return_t~NSEI1[2:nrow(NSEI1),1],
     type='l',lwd=0.5, main='NIFTY 50 Volatility from 01-01-2015 until 20-02-2022',
     xlab='Date',ylab='Volatility')
abline(h=0, col='red', lty=3,lwd=3)

#table with daily and annualized return Statistics
NSEI1_return_annualized= mean(NSEI1_return_t)*252*100
NSEI1_return_annualized
NSEI1_volatility_annualized= sd(NSEI1_return_t)*sqrt(252)*100
NSEI1_volatility_annualized
NSEI1_Sharpe_Ratio<- NSEI1_return_annualized/NSEI1_volatility_annualized
NSEI1_Sharpe_Ratio



#NSEI Monte Carlo Simulation 

nsim=nrow(NSEI2)+1
NSEI1_daily_return<- NSEI1_return_annualized/100/252
NSEI1_daily_volatility<-NSEI1_volatility_annualized/100/sqrt(252)
NSEI1_T<- nrow(NSEI2)+1
NSEI1_T
NSEI1_S0=as.numeric(NSEI1[nrow(NSEI1),2])
NSEI1_S0

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
  a <- mc_function(nsim =nsim,N=NSEI1_T,daily_mean=NSEI1_daily_return,
                   daily_sd = NSEI1_daily_volatility, S0 = NSEI1_S0)
  a <- as.data.frame(a)
  if(i==1)
  {
    NSEI1_MC_results <- a 
  }
  else {
    NSEI1_MC_results <- cbind(NSEI1_MC_results,a)
  }
  
}


NSEI1_MC_results[1,]<- NSEI1_S0
NSEI1_MC_results

for (i in 1:ncol(NSEI1_MC_results)){
  colnames(NSEI1_MC_results)[i] <- paste0('Sim ',i)
}

for(i in 1:ncol(NSEI1_MC_results))
{
  b <- diff(log(NSEI1_MC_results[,i]))
  b <- as.data.frame(b)
  if(i==1)
  {
    NSEI1_MC_returns <- b
  }
  else {
    NSEI1_MC_returns <- cbind(NSEI1_MC_returns,b)
  }
  
}

for (i in 1:ncol(NSEI1_MC_returns)){
  colnames(NSEI1_MC_returns)[i] <- paste0('Sim ',i)
}


print(NSEI1_MC_returns)
NSEI1_MC_returns<- na.omit(NSEI1_MC_returns)

NSEI2_actual_prices<-c(NSEI1_S0,NSEI2$Close)
NSEI2_actual_prices
NSEI2_actual_returns<-diff(log(NSEI2_actual_prices))
NSEI2_actual_returns<- as.data.frame(NSEI2_actual_returns)
dim(NSEI2_actual_returns)

### Error for each day of the model by assuming daily price as correct one

NSEI2_daily_error_aux<- NSEI1_MC_returns
for (i in 1:ncol(NSEI2_daily_error_aux)) {
  NSEI2_daily_error_aux[,i]<-NSEI2_actual_returns
}
NSEI2_daily_error_aux<-as.data.frame(NSEI2_daily_error_aux)

for (i in 1:ncol(NSEI2_daily_error_aux)) {
  d<- abs(NSEI1_MC_returns[,i]-NSEI2_daily_error_aux[,i])
  d<- as.data.frame(d)
  
  if (i==1) {
    NSEI_error<-d
    
  }
  else{
    NSEI_error<-cbind(NSEI_error,d)
  }
}

NSEI_error    #use this for Error measurmments

for (i in 1:ncol(NSEI_error)){
  colnames(NSEI_error)[i] <- paste0('Sim ',i)
}

NSEI_error<-as.data.frame(NSEI_error)
NSEI_error_means<-rowMeans(NSEI_error)
NSEI2_actual_returns
NSEI2_print<-cbind(NSEI2_actual_returns,NSEI_error_means)
NSEI2_print
hist(NSEI2_actual_returns$NSEI2_actual_returns,col='light grey',
     breaks=seq(from=-0.2, to=0.2,by=0.025),
     xlab= "Daily Returns",main="Actual Returns of NIFTY 50 for 21-02-2022 until 04-03-2022",
     las=1,ylim=c(0,8))
plot(NSEI2_actual_returns$NSEI2_actual_returns~NSEI2$Date,type='l',col='black',ylim=c(-0.15,0.15), 
     xlab="Simulated Days",ylab="Return",lwd=1, main='NIFTY 50
     Actual vs. Average Simulated Returns')
lines(NSEI_error_means~NSEI2$Date,type='l',col='Orange')

##  Absolute difference between the NSEI2_Actual_Returns and NSEI_error_menans
for (i in 1:nrow(as.data.frame(NSEI_error_means))) {
  
  e<- abs(NSEI2_print[i,1]-NSEI2_print[i,2])
  e<-as.data.frame(e)
  
  if (i==1) {
    NSEI_abs_error<-e  
  }
  
  else{
    NSEI_abs_error<- rbind(NSEI_abs_error,e)
  }
}
e
NSEI_abs_error

plot(NSEI_abs_error$e~NSEI2$Date,xlab='Days',ylab='Difference to actual', main='NIFTY 50 Monte Carlo Innefiency',
     type='l',lwd=2)

## Absolute squared error

for (i in 1:nrow(as.data.frame(NSEI_error_means))) {
  
  g<- abs(NSEI2_print[i,1]-NSEI2_print[i,2])**2
  g<-as.data.frame(g)
  
  if (i==1) {
    NSEI_mean_error<-g  
  }
  else{
    NSEI_mean_error<-rbind(NSEI_mean_error,g)
  }
}
g
NSEI_mean_error
# Mean Errors
NSEI_MC_absoulte_error<- sum(NSEI_abs_error$e)/nrow(NSEI_abs_error)
NSEI_MC_squared_mean_error<- sum(NSEI_mean_error$g)/nrow(NSEI_mean_error)
NSEI_MC_root_mean_error<-sqrt(NSEI_MC_squared_mean_error)

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
     ,main='NIFTY 50 (GARCH) Actual Absolute Return vs. Conditional SD',xlab='Date',ylab='Volatility'
     ,col='grey')
lines(abs(as.numeric(NSEI1[2:nrow(NSEI1),5]))
      ~NSEI1[2:nrow(NSEI1),1],type = 'l',col='Orange',lwd=1) # We can see that Model not cover a lot

plot(as.numeric(NSEI1[2:nrow(NSEI1),6])~NSEI1[2:nrow(NSEI1),1], type='l',xlab='Date'
     ,ylab='Volatility',main='NIFTY 50 Inneficiency of GARCH Model',col='grey')
abline(h=0,col='red',lwd=3,lty=3)


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
     ,main='NIFTY 50 (GARCH) Actual Absolute Return vs. Forecasted Conditional SD',xlab='Date',ylab='Volatility'
     ,col='black')
lines(abs(as.numeric(NSEI2[2:nrow(NSEI2),5]))
      ~NSEI2[2:nrow(NSEI2),1],type = 'l',col='Red',lwd=0.1) 
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


