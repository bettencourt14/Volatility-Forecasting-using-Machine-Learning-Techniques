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


setwd("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data") #change this

#Daily prices for our indexes
N100 <- read_excel("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/N100.xlsx", 
                   col_types = c("date", "numeric", "numeric"))
N100<-as.data.frame(N100)
print(N100)
#Plot N100 prices
plot(N100$Close ~N100$Date,type="l",col="black",axes="T",xlab="Date",ylab="Closing Prices", 
     main="Euronext 100 Daily Prices",lwd=2)
# Missing Values 
length(which((N100$Close==0)))
N100$Close[N100$Close==0]<-NA
colSums(is.na(N100))
N100<- na.omit(N100)
which(is.na(N100))

#Remove 20-02-2022 until 04-03-2022

N1001_date<- N100[N100$Date>='2020-10-01'&
                    N100$Date<'2022-02-20',]
N1001<-as.data.frame(N1001_date)
head(N1001)
N1002<-as.data.frame(N100[N100$Date>='2022-02-20'&
                            N100$Date<='2022-03-04',])
print(N1002)
tail(N1002)

#N100 Monte CARLO SIM
nsim=nrow(N1002)+1
N1001_return=diff(log(N1001$Close),na.rm=T)
N1001_return
N1001_mean_return=mean(N1001_return)
N1001_mean_return
N1001_annualized_mean_return<- N1001_mean_return*252
N1001_annualized_mean_return
N1001_SD= sqrt(var(N1001_return))
N1001_annualized_SD_return<- N1001_SD*sqrt(252)
N1001_annualized_SD_return

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

for (i in 1:500)
{
  a <- mc_function(nsim =nsim,N=N1001_T,daily_mean =N1001_mean_return,
                   daily_sd = N1001_SD, S0 = N1001_S0)
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


View(N1001_MC_returns)
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

N100_error_mode<-rowQuantiles(N1001_MC_returns,prob=0.50)
N100_error<-as.data.frame(N100_error)
N100_error_means<-rowMeans(N100_error)
N1002_actual_returns
N1002_view<-cbind(N1002_actual_returns,N100_error_means)
N1002_view
hist(N1002_actual_returns$N1002_actual_returns,col='light grey',
     breaks=seq(from=-0.2, to=0.2,by=0.02),
     xlab= "Daily Returns",main="Actual Returns of Euronext for 21-02-2022 until 04-04-2022",
     las=1,ylim=c(0,8))

lines(density(N1002_actual_returns$N1002_actual_returns),type='l',col='red')
plot(N1002_actual_returns$N1002_actual_returns,type='l',col='black',ylim=c(-0.15,0.15), 
     xlab="Simulated Days",ylab="Closing Price",lwd=1, main='Euronext 100
     Actual vs. Average Simulated Returns')
lines(N100_error_means,type='p',col='Dark blue')
##  N100 mean absolute error
N100_error_means<- as.data.frame(N100_error_means)
N100_mean_absolute_error<- sum(N100_error_means)
N100_mean_absolute_error
#### N100 squared mean error      i..e by assuming mean as actual return for each day
for (i in 1:ncol(N1002_daily_error_aux)) {
  d<- abs(N1001_MC_returns[,i]-N1002_daily_error_aux[,i])^2
  d<- as.data.frame(d)
  
  if (i==1) {
    N100_error_squared<-d
    
  }
  else{
    N100_error_squared<-cbind(N100_error_squared,d)
  }
}

N100_error_squared
N100_error_squared_mean<-rowMeans(N100_error_squared)
N100_squared_mean_error<- sum(N100_error_squared_mean)
N100_root_squared_mean_error<- sqrt(N100_squared_mean_error)
N100_root_squared_mean_error

