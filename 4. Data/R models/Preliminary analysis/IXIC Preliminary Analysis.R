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
plot(IXIC$Close ~IXIC$Date,type="l",col="darkblue",axes="T",xlab="Date",ylab="Closing Prices", 
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
     las=1,col = "darkblue")

plot(IXIC1_return_t~IXIC1[2:nrow(IXIC1),1],
     type='l',lwd=0.5, main='NASDAQ Volatility from 01-01-2015 until 20-02-2022',
     xlab='Date',ylab='Volatility',col='darkblue')
abline(h=0, col='darkorange', lty=4,lwd=4)
legend('bottomright',
       legend = c('Volatility','Zero Volatility'),
       col=c('darkblue','darkorange'),
       pch = c(9,9))

#table with daily and annualized return Statistics
IXIC1_return_annualized= mean(IXIC1_return_t)*252*100
IXIC1_return_annualized
IXIC1_volatility_annualized= sd(IXIC1_return_t)*sqrt(252)*100
IXIC1_volatility_annualized
IXIC1_Sharpe_Ratio<- IXIC1_return_annualized/IXIC1_volatility_annualized
IXIC1_Sharpe_Ratio

# Period forecasted atctual Returns


IXIC2_returns<-rbind(log(IXIC2[1,2]/IXIC1[nrow(IXIC1),2]),as.data.frame(diff(log(IXIC2$Close))))
IXIC2_returns<-as.data.frame(IXIC2_returns)

