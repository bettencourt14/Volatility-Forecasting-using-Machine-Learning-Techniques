# BVSP big source

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





