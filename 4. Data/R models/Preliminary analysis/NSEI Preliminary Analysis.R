
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

# Period forecasted atctual Returns


NSEI2_returns<-rbind(log(NSEI2[1,2]/NSEI1[nrow(NSEI1),2]),as.data.frame(diff(log(NSEI2$Close))))
NSEI2_returns<-as.data.frame(NSEI2_returns)

