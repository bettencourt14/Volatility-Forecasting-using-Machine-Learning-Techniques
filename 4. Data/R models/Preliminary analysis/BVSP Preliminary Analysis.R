
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
BVSP <- read_excel(paste0(getwd(),"/BVSP.xlsx"),
                   col_types = c("date", "numeric", "numeric"))
BVSP<-as.data.frame(BVSP)
print(BVSP)
#Plot BVSP prices
plot(BVSP$Close ~BVSP$Date,type="l",col="black",axes="T",xlab="Date",ylab="Closing Prices", 
     main="IBOVESPA Daily Prices",lwd=1)
# Missing Values 
length(which((BVSP$Close==0)))
BVSP$Close[BVSP$Close==0]<-NA
colSums(is.na(BVSP))
BVSP<- na.omit(BVSP)
which(is.na(BVSP))
#Remove 2022-03-01 until 2022-03-30
BVSP1_date<- BVSP[BVSP$Date>='2015-01-01'&
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
hist(BVSP1_return_t, breaks=seq(from=-0.2, to=0.2,by=0.002),
     xlab= "Daily Returns",main=paste0('IBOVESPA Returns from 01-01-2015 until 20-02-2022'),
     las=1,col = "grey")

plot(BVSP1_return_t~BVSP1[2:nrow(BVSP1),1],
     type='l',lwd=0.5, main='IBOVESPA Volatility from 01-01-2015 until 20-02-2022',
     xlab='Date',ylab='Volatility')
abline(h=0, col='red', lty=3,lwd=3)

#table with daily and annualized return Statistics
BVSP1_return_annualized= mean(BVSP1_return_t)*252*100
BVSP1_return_annualized
BVSP1_volatility_annualized= sd(BVSP1_return_t)*sqrt(252)*100
BVSP1_volatility_annualized
BVSP1_Sharpe_Ratio<- BVSP1_return_annualized/BVSP1_volatility_annualized
BVSP1_Sharpe_Ratio

# Period forecasted atctual Returns


BVSP2_returns<-rbind(log(BVSP2[1,2]/BVSP1[nrow(BVSP1),2]),as.data.frame(diff(log(BVSP2$Close))))
BVSP2_returns<-as.data.frame(BVSP2_returns)

