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
###########################################################################################
##################################################################################
#############################################################################################
#Data from 10/01/2020 until 04/03/2022, download from Yahoo Finance

#Daily prices for our indexes
BVSP <- read_excel(paste0(getwd(),"/BVSP.xlsx"),
                   col_types = c("date", "numeric", "numeric"))
BVSP<-as.data.frame(BVSP)
print(BVSP)
#Plot BVSP prices
plot(BVSP$Close ~BVSP$Date,type="l",col="black",axes="T",xlab="Date",ylab="Closing Prices", 
     main="IBOVESPA Daily Prices",lwd=2)
# Missing Values 
length(which((BVSP$Close==0)))
BVSP$Close[BVSP$Close==0]<-NA
colSums(is.na(BVSP))
BVSP<- na.omit(BVSP)
which(is.na(BVSP))
#Remove 2022-03-01 until 2022-03-30
BVSP1_date<- BVSP[BVSP$Date>='2020-10-01'&
                    BVSP$Date<'2022-02-20',]
BVSP1<-as.data.frame(BVSP1_date)
head(BVSP1)
BVSP2<-as.data.frame(BVSP[BVSP$Date>='2022-02-20'&
                            BVSP$Date<='2022-03-04',])
print(BVSP2)
tail(BVSP2)
#Daily Returns
BVSP1_x <- BVSP1$Close
BVSP1_return<- diff(log(BVSP1_x))
which(is.na(BVSP1_return))
print(BVSP1_return)
mean(BVSP1_return)
sd(BVSP1_return)
summary(BVSP1_return)
BVSP1_return<-as.data.frame(BVSP1_return)
BVSP1_return<-rbind('NA',BVSP1_return)
BVSP1<-cbind(BVSP1$Date,BVSP1$Close,BVSP1_return,BVSP1$Volume)
head(BVSP1)
BVSP1_return_t<- as.numeric(BVSP1[2:nrow(BVSP1),3])
hist(BVSP1_return_t, breaks=seq(from=-0.15, to=0.15,by=0.002),
     xlab= "Daily Returns",main=paste0('IBOVESPA Returns from 01-10-2020 until 20-02-2022'),
     las=1,col = "grey")
lines(density(BVSP1_return_t), col="red",lwd=1)

plot(BVSP1_return_t~BVSP1[2:nrow(BVSP1),1],
     type='l',lwd=1.5, main='IBOVESPA Volatility from 01-10-2020 until 20-02-2022',
     xlab='Date',ylab='Volatility')
abline(h=0, col='red', lty=3,lwd=3)
#table with daily and annualized return Statistics
BVSP1_return_annualized= mean(BVSP1_return_t)*252*100
BVSP1_return_annualized
BVSP1_volatility_annualized= sd(BVSP1_return_t)*sqrt(252)*100
BVSP1_volatility_annualized
BVSP1_Sharpe_Ratio<- BVSP1_return_annualized/BVSP1_volatility_annualized
BVSP1_Sharpe_Ratio


# GARCH

#BVSP autocorrelaction function
acf(BVSP1_return_t,lag.max = 10)
#BVSP GARCH Model
BVSP1_s<- ugarchspec(mean.model =list(armaOrder=c(1,1)), 
                     variance.model = list(model="sGARCH"),
                     distribution.model = 'norm' )
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
abs(BVSP1_return_t[400,1])== BVSP1_Garch_SD_t[400,1]-BVSP1_Garch_accuracy_t[400,1]
plot(abs(BVSP1_return_t$BVSP1_return_t)~BVSP1[2:nrow(BVSP1),1],type='l',lwd=1.5
     ,main='IBOVESPA (GARCH) Actual Absolute Return vs. Conditional SD',xlab='Date',ylab='Volatility'
     ,col='grey')
lines(abs(as.numeric(BVSP1[2:nrow(BVSP1),5]))
      ~BVSP1[2:nrow(BVSP1),1],type = 'l',col='deepskyblue4',lwd=1) # We can see that Model not cover a lot

plot(as.numeric(BVSP1[2:nrow(BVSP1),6])~BVSP1[2:nrow(BVSP1),1], type='l',xlab='Date'
     ,ylab='Volatility',main='IBOVESPA Inneficiency of GARCH Model')
abline(h=0,col='red',lwd=3,lty=3)
# BVSP2 Garch Forecast
BVSP2_f <- ugarchforecast(fitORspec = BVSP1_m, n.ahead = nrow(BVSP2))
plot(fitted(BVSP2_f))
plot(sigma(BVSP2_f))
print(BVSP2_f,which='all')
BVSP2_returns<-diff(log(BVSP2$Close))
BVSP2_returns
BVSP2_Volatility<- sd(BVSP2_returns)
BVSP2_Volatility
BVSP2_forecast_sigma<-BVSP2_f@forecast$sigmaFor
BVSP2_forecast_sigma
BVSP2_returns<-as.data.frame(BVSP2_returns)
for (i in 1:nrow(BVSP2_returns)) {
  b<-abs(BVSP2_returns[i,1])-BVSP2_forecast_sigma[i,1]
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
lines(density(as.numeric(BVSP2[2:nrow(BVSP2),3])), col="Red",lwd=1)

plot(abs(as.numeric(BVSP2[2:nrow(BVSP2),3]))~BVSP2[2:nrow(BVSP2),1],type='l',lwd=1
     ,main='IBOVESPA (GARCH) Actual Absolute Return vs. Forecasted Conditional SD',xlab='Date',ylab='Volatility'
     ,col='black')
lines(abs(as.numeric(BVSP2[2:nrow(BVSP2),5]))
      ~BVSP2[2:nrow(BVSP2),1],type = 'l',col='Red',lwd=0.1) 
plot(as.numeric(BVSP2[2:nrow(BVSP2),6])~BVSP2[2:nrow(BVSP2),1],type='l',lwd=1,
     main='IBOVESPA Inneficienfy of GARCH Forecasting Model',xlab='Date',ylab='Volatility'
     ,col='black')
abline(h=0,col='Red',lty=3,lwd=2)
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
BVSP_Garch_training_squared_error<-sum(as.numeric(BVSP1_squared_error[2:nrow(BVSP1_squared_error),1]))
BVSP_Garch_training_root_error<- sqrt(BVSP_Garch_training_squared_error)
BVSP_Garch_training_abs_error<- sum(abs(as.numeric(BVSP1[2:nrow(BVSP1),6])))
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
BVSP_Garch_abs_error<-sum(BVSP2_mean_error$h)
BVSP_Garch_squared_error<-BVSP2_squared_error_value
BVSP_Garch_root_error<-BVSP2_sqrt_error_value

head(BVSP2)


#Monte-carlo
nsim=nrow(BVSP2)+1
BVSP1_daily_return<- BVSP1_return_annualized/100/252
BVSP1_daily_volatility<-BVSP1_volatility_annualized/100/sqrt(252)
BVSP1_T<- nrow(BVSP2)+1
BVSP1_T
BVSP1_S0=as.numeric(BVSP1[nrow(BVSP1),2])
BVSP1_S0

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
  a <- mc_function(nsim =nsim,N=BVSP1_T,daily_mean=BVSP1_daily_return,
                   daily_sd = BVSP1_daily_volatility, S0 = BVSP1_S0)
  a <- as.data.frame(a)
  if(i==1)
  {
    BVSP1_MC_results <- a 
  }
  else {
    BVSP1_MC_results <- cbind(BVSP1_MC_results,a)
  }
  
}


BVSP1_MC_results[1,]<- BVSP1_S0
BVSP1_MC_results

for (i in 1:ncol(BVSP1_MC_results)){
  colnames(BVSP1_MC_results)[i] <- paste0('Sim ',i)
}

for(i in 1:ncol(BVSP1_MC_results))
{
  b <- diff(log(BVSP1_MC_results[,i]))
  b <- as.data.frame(b)
  if(i==1)
  {
    BVSP1_MC_returns <- b
  }
  else {
    BVSP1_MC_returns <- cbind(BVSP1_MC_returns,b)
  }
  
}

for (i in 1:ncol(BVSP1_MC_returns)){
  colnames(BVSP1_MC_returns)[i] <- paste0('Sim ',i)
}


View(BVSP1_MC_returns)
BVSP1_MC_returns<- na.omit(BVSP1_MC_returns)

BVSP2_actual_prices<-c(BVSP1_S0,BVSP2$Close)
BVSP2_actual_prices
BVSP2_actual_returns<-diff(log(BVSP2_actual_prices))
BVSP2_actual_returns<- as.data.frame(BVSP2_actual_returns)
dim(BVSP2_actual_returns)

### Error for each day of the model by assuming daily price as correct one

BVSP2_daily_error_aux<- BVSP1_MC_returns
for (i in 1:ncol(BVSP2_daily_error_aux)) {
  BVSP2_daily_error_aux[,i]<-BVSP2_actual_returns
}
BVSP2_daily_error_aux<-as.data.frame(BVSP2_daily_error_aux)

for (i in 1:ncol(BVSP2_daily_error_aux)) {
  d<- abs(BVSP1_MC_returns[,i]-BVSP2_daily_error_aux[,i])
  d<- as.data.frame(d)
  
  if (i==1) {
    BVSP_error<-d
    
  }
  else{
    BVSP_error<-cbind(BVSP_error,d)
  }
}

BVSP_error    #use this for Error measurmments

for (i in 1:ncol(BVSP_error)){
  colnames(BVSP_error)[i] <- paste0('Sim ',i)
}

BVSP_error<-as.data.frame(BVSP_error)
BVSP_error_means<-rowMeans(BVSP_error)
BVSP2_actual_returns
BVSP2_view<-cbind(BVSP2_actual_returns,BVSP_error_means)
BVSP2_view
hist(BVSP2_actual_returns$BVSP2_actual_returns,col='light grey',
     breaks=seq(from=-0.2, to=0.2,by=0.02),
     xlab= "Daily Returns",main="Actual Returns of IBOVESPA for 21-02-2022 until 04-04-2022",
     las=1,ylim=c(0,8))
lines(density(BVSP2_actual_returns$BVSP2_actual_returns),type='l',col='red')
plot(BVSP2_actual_returns$BVSP2_actual_returns,type='l',col='black',ylim=c(-0.15,0.15), 
     xlab="Simulated Days",ylab="Closing Price",lwd=1, main='IBOVESPA
     Actual vs. Average Simulated Returns')
lines(BVSP_error_means,type='p',col='Dark blue')

##  Absolute difference between the BVSP2_Actual_Returns and BVSP_error_menans

for (i in 1:nrow(as.data.frame(BVSP_error_means))) {
  
  e<- abs(BVSP2_view[i,1]-BVSP2_view[i,2])
  e<-as.data.frame(e)
  
  if (i==1) {
    BVSP_abs_error<-e  
  }
  
  else{
    BVSP_abs_error<- rbind(BVSP_abs_error,e)
  }
}
e
BVSP_abs_error

plot(BVSP_abs_error$e~BVSP2$Date,xlab='Days',ylab='Difference to actual', main='IBOVESPA Monte Carlo Innefiency',
     type='l',lwd=2)

## Absolute squared error

for (i in 1:nrow(as.data.frame(BVSP_error_means))) {
  
  g<- abs(BVSP2_view[i,1]-BVSP2_view[i,2])**2
  g<-as.data.frame(g)
  
  if (i==1) {
    BVSP_mean_error<-g  
  }
  else{
    BVSP_mean_error<-rbind(BVSP_mean_error,g)
  }
}
g
BVSP_mean_error
# Mean Errors
BVSP_absoulte_error<- sum(BVSP_abs_error$e)/nrow(BVSP_abs_error)
BVSP_squared_mean_error<- sum(BVSP_mean_error$g)/nrow(BVSP_mean_error)
BVSP_root_mean_error<-sqrt(BVSP_squared_mean_error)

###############################################################################
###############################################################################
##############################################################################
##################

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


############################################################################################3
##############################################################################################
#################################################################################################
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
####################################################################################3
###################################################################################33
######################################################################################3
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








