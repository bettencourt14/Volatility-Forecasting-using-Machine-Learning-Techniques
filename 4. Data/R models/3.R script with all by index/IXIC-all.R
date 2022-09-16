# IXIC big source
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
IXIC <- read_excel(paste0(getwd(),"/IXIC.xlsx"),
                   col_types = c("date", "numeric", "numeric"))
IXIC<-as.data.frame(IXIC)
print(IXIC)
#Plot IXIC prices
plot(IXIC$Close ~IXIC$Date,type="l",col="black",axes="T",xlab="Date",ylab="Closing Prices", 
     main="NASDAQ Daily Prices",lwd=1)
# Missing Values 
length(which((IXIC$Close==0)))
IXIC$Close[IXIC$Close==0]<-NA
colSums(is.na(IXIC))
IXIC<- na.omit(IXIC)
which(is.na(IXIC))
#Remove 2022-03-01 until 2022-03-30
IXIC1_date<- IXIC[IXIC$Date>='2015-01-01'&
                    IXIC$Date<'2022-02-20',]
IXIC1<-as.data.frame(IXIC1_date)
head(IXIC1)
IXIC2<-as.data.frame(IXIC[IXIC$Date>='2022-02-20'&
                            IXIC$Date<='2022-03-04',])
print(IXIC2)
tail(IXIC2)
#Daily Returns
IXIC1_x <- IXIC1$Close
IXIC1_return<- diff(log(IXIC1_x))
which(is.na(IXIC1_return))
print(IXIC1_return)
mean(IXIC1_return)
sd(IXIC1_return)
summary(IXIC1_return)
IXIC1_return<-as.data.frame(IXIC1_return)
IXIC1_return<-rbind('NA',IXIC1_return)
IXIC1<-cbind(IXIC1$Date,IXIC1$Close,IXIC1_return,IXIC1$Volume)
head(IXIC1)
IXIC1_return_t<- as.numeric(IXIC1[2:nrow(IXIC1),3])
hist(IXIC1_return_t, breaks=seq(from=-0.2, to=0.2,by=0.002),
     xlab= "Daily Returns",main=paste0('NASDAQ Returns from 01-01-2015 until 20-02-2022'),
     las=1,col = "grey")

plot(IXIC1_return_t~IXIC1[2:nrow(IXIC1),1],
     type='l',lwd=0.5, main='NASDAQ Volatility from 01-01-2015 until 20-02-2022',
     xlab='Date',ylab='Volatility')
abline(h=0, col='red', lty=3,lwd=3)

#table with daily and annualized return Statistics
IXIC1_return_annualized= mean(IXIC1_return_t)*252*100
IXIC1_return_annualized
IXIC1_volatility_annualized= sd(IXIC1_return_t)*sqrt(252)*100
IXIC1_volatility_annualized
IXIC1_Sharpe_Ratio<- IXIC1_return_annualized/IXIC1_volatility_annualized
IXIC1_Sharpe_Ratio



#IXIC Monte Carlo Simulation 

nsim=nrow(IXIC2)+1
IXIC1_daily_return<- IXIC1_return_annualized/100/252
IXIC1_daily_volatility<-IXIC1_volatility_annualized/100/sqrt(252)
IXIC1_T<- nrow(IXIC2)+1
IXIC1_T
IXIC1_S0=as.numeric(IXIC1[nrow(IXIC1),2])
IXIC1_S0

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
  a <- mc_function(nsim =nsim,N=IXIC1_T,daily_mean=IXIC1_daily_return,
                   daily_sd = IXIC1_daily_volatility, S0 = IXIC1_S0)
  a <- as.data.frame(a)
  if(i==1)
  {
    IXIC1_MC_results <- a 
  }
  else {
    IXIC1_MC_results <- cbind(IXIC1_MC_results,a)
  }
  
}


IXIC1_MC_results[1,]<- IXIC1_S0
IXIC1_MC_results

for (i in 1:ncol(IXIC1_MC_results)){
  colnames(IXIC1_MC_results)[i] <- paste0('Sim ',i)
}

for(i in 1:ncol(IXIC1_MC_results))
{
  b <- diff(log(IXIC1_MC_results[,i]))
  b <- as.data.frame(b)
  if(i==1)
  {
    IXIC1_MC_returns <- b
  }
  else {
    IXIC1_MC_returns <- cbind(IXIC1_MC_returns,b)
  }
  
}

for (i in 1:ncol(IXIC1_MC_returns)){
  colnames(IXIC1_MC_returns)[i] <- paste0('Sim ',i)
}


print(IXIC1_MC_returns)
IXIC1_MC_returns<- na.omit(IXIC1_MC_returns)

IXIC2_actual_prices<-c(IXIC1_S0,IXIC2$Close)
IXIC2_actual_prices
IXIC2_actual_returns<-diff(log(IXIC2_actual_prices))
IXIC2_actual_returns<- as.data.frame(IXIC2_actual_returns)
dim(IXIC2_actual_returns)

### Error for each day of the model by assuming daily price as correct one

IXIC2_daily_error_aux<- IXIC1_MC_returns
for (i in 1:ncol(IXIC2_daily_error_aux)) {
  IXIC2_daily_error_aux[,i]<-IXIC2_actual_returns
}
IXIC2_daily_error_aux<-as.data.frame(IXIC2_daily_error_aux)

for (i in 1:ncol(IXIC2_daily_error_aux)) {
  d<- abs(IXIC1_MC_returns[,i]-IXIC2_daily_error_aux[,i])
  d<- as.data.frame(d)
  
  if (i==1) {
    IXIC_error<-d
    
  }
  else{
    IXIC_error<-cbind(IXIC_error,d)
  }
}

IXIC_error    #use this for Error measurmments

for (i in 1:ncol(IXIC_error)){
  colnames(IXIC_error)[i] <- paste0('Sim ',i)
}

IXIC_error<-as.data.frame(IXIC_error)
IXIC_error_means<-rowMeans(IXIC_error)
IXIC2_actual_returns
IXIC2_print<-cbind(IXIC2_actual_returns,IXIC_error_means)
IXIC2_print
hist(IXIC2_actual_returns$IXIC2_actual_returns,col='light grey',
     breaks=seq(from=-0.2, to=0.2,by=0.025),
     xlab= "Daily Returns",main="Actual Returns of NASDAQ for 21-02-2022 until 04-03-2022",
     las=1,ylim=c(0,8))
plot(IXIC2_actual_returns$IXIC2_actual_returns~IXIC2$Date,type='l',col='black',ylim=c(-0.15,0.15), 
     xlab="Simulated Days",ylab="Return",lwd=1, main='NASDAQ
     Actual vs. Average Simulated Returns')
lines(IXIC_error_means~IXIC2$Date,type='l',col='Orange')

##  Absolute difference between the IXIC2_Actual_Returns and IXIC_error_menans
for (i in 1:nrow(as.data.frame(IXIC_error_means))) {
  
  e<- abs(IXIC2_print[i,1]-IXIC2_print[i,2])
  e<-as.data.frame(e)
  
  if (i==1) {
    IXIC_abs_error<-e  
  }
  
  else{
    IXIC_abs_error<- rbind(IXIC_abs_error,e)
  }
}
e
IXIC_abs_error

plot(IXIC_abs_error$e~IXIC2$Date,xlab='Days',ylab='Difference to actual', main='NASDAQ Monte Carlo Innefiency',
     type='l',lwd=2)

## Absolute squared error

for (i in 1:nrow(as.data.frame(IXIC_error_means))) {
  
  g<- abs(IXIC2_print[i,1]-IXIC2_print[i,2])**2
  g<-as.data.frame(g)
  
  if (i==1) {
    IXIC_mean_error<-g  
  }
  else{
    IXIC_mean_error<-rbind(IXIC_mean_error,g)
  }
}
g
IXIC_mean_error
# Mean Errors
IXIC_MC_absoulte_error<- sum(IXIC_abs_error$e)/nrow(IXIC_abs_error)
IXIC_MC_squared_mean_error<- sum(IXIC_mean_error$g)/nrow(IXIC_mean_error)
IXIC_MC_root_mean_error<-sqrt(IXIC_MC_squared_mean_error)

#IXIC autocorrelaction function
acf(IXIC1_return_t,lag.max = 10, main="Autocorrelation Function- NASDAQ")
#IXIC GARCH Model
IXIC1_s<- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                     distribution.model = "std")
ctrl = list(RHO = 1,DELTA = 1e-8,MAJIT = 100,MINIT = 650,TOL = 1e-6)
IXIC1_m<-ugarchfit(data = IXIC1_return_t,spec =IXIC1_s,solver = 'solnp',solver.control = ctrl)
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


