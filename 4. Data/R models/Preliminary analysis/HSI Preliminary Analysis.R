
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

# Period forecasted atctual Returns


HSI2_returns<-rbind(log(HSI2[1,2]/HSI1[nrow(HSI1),2]),as.data.frame(diff(log(HSI2$Close))))
HSI2_returns<-as.data.frame(HSI2_returns)

