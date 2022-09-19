
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

# Period forecasted atctual Returns


N1002_returns<-rbind(log(N1002[1,2]/N1001[nrow(N1001),2]),as.data.frame(diff(log(N1002$Close))))
N1002_returns<-as.data.frame(N1002_returns)

