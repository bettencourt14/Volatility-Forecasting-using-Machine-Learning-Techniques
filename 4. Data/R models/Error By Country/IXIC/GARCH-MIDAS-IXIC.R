### IXIC GARCH-MIDAS

source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Preliminary analysis/IXIC Preliminary Analysis.R")
library(mfGARCH)
library(lubridate)
library(ggplot2)

IXIC_MIDAS_Data <- read_excel("Low Frequency Data/IXIC-low freq.xlsx",
                              col_types = c("date", "numeric", "numeric",
                                            "numeric", "text", "numeric", "numeric",
                                            "text", "numeric", "numeric"))

par(mar=c(5,5,5,5))
plot(IXIC_MIDAS_Data$Date,y= IXIC_MIDAS_Data$`Value ltir`,type='l',
     col='darkblue',xlab='Date',ylab='Long Term Interest Rate % Value')
par(new=T)
plot(IXIC_MIDAS_Data$Date,y=IXIC_MIDAS_Data$`value house`,type='l',
     col='darkorange',ylab='',xlab='',axes=F,
     main=' United States of America Long Term Interest Rate and House Pricing')
axis(side=4,at=pretty(range(IXIC_MIDAS_Data$`value house`)))
mtext("House Price",side=4,line=3)
legend('bottomleft',
       legend=c('Long term Interest Rate','House Pricing'),
       col=c('darkblue','darkorange'),
       pch = c(9,9))


IXIC_MIDAS_Data$Return<-IXIC_MIDAS_Data$Return*100
IXIC_MIDAS_Data$`LTIR return`<-IXIC_MIDAS_Data$`LTIR return`*100
IXIC_MIDAS_Data$`House return`<-IXIC_MIDAS_Data$`House return`*100
IXIC_MIDAS_Data_1<-IXIC_MIDAS_Data[1:nrow(IXIC1),]
IXIC_MIDAS_Data_1<- as.data.frame(IXIC_MIDAS_Data_1)
IXIC_MIDAS_Data_1[,1]<- as.Date(IXIC_MIDAS_Data_1[,1])
str(IXIC_MIDAS_Data_1)
IXIC_MIDAS_Data_1[,5]<- as.Date.character(IXIC_MIDAS_Data_1[,5], "%d/%m/%Y")
IXIC_MIDAS_Data_1[,8]<- as.Date.character(IXIC_MIDAS_Data_1[,8], "%d/%m/%Y")
str(IXIC_MIDAS_Data_1)
IXIC_MIDAS_Data_1<- na.omit(IXIC_MIDAS_Data_1)
which(is.na(IXIC_MIDAS_Data_1))
colnames(IXIC_MIDAS_Data_1)<-c('date','close','volume','return',
                               'time_ltir','value_ltir','ltir_return',
                               'time_house','value_house','house_return')

IXIC_GARCH_MIDAS<-cbind(IXIC_MIDAS_Data_1$date,
                        IXIC_MIDAS_Data_1$return,
                        IXIC_MIDAS_Data_1$time_ltir,
                        IXIC_MIDAS_Data_1$ltir_return,
                        IXIC_MIDAS_Data_1$house_return,
                        IXIC_MIDAS_Data_1$time_house)

IXIC_GARCH_MIDAS<-as.data.frame(IXIC_GARCH_MIDAS)
IXIC_GARCH_MIDAS[,1]<-as.Date(IXIC_GARCH_MIDAS[,1])
IXIC_GARCH_MIDAS[,3]<-as.Date(IXIC_GARCH_MIDAS[,3])
IXIC_GARCH_MIDAS[,6]<-as.Date(IXIC_GARCH_MIDAS[,6])
colnames(IXIC_GARCH_MIDAS)<-c("date","return","time_ltir",
                              "ltir_return","house_return","time_house")
str(IXIC_GARCH_MIDAS)
print(IXIC_GARCH_MIDAS)

#correlation between variables

cor(IXIC_GARCH_MIDAS$ltir_return,IXIC_GARCH_MIDAS$house_return)


# GARCH-MIDAS PROCESS

IXIC_mf<-fit_mfgarch(data = IXIC_GARCH_MIDAS,y = "return",
                     x = "house_return",low.freq = "time_house",K=4,
                     x.two = "ltir_return", K.two =12,low.freq.two = "time_ltir",
                     weighting.two = "beta.restricted")

View(IXIC_mf$df.fitted)

# Based on my understanding G is the conditional Variance for a given day, and by multiplying it by Tau we acheive the Conditional variance total

IXIC_GARCH_MIDAS_Variance<-as.data.frame(cbind(as.Date(IXIC_GARCH_MIDAS$date),
                                               IXIC_mf$g,IXIC_mf$tau))
IXIC_GARCH_MIDAS_Variance[,1]<- as.Date(IXIC_GARCH_MIDAS_Variance[,1])
print(IXIC_GARCH_MIDAS_Variance)

IXIC_GARCH_MIDAS_Variance<-na.omit(IXIC_GARCH_MIDAS_Variance)
which(is.na(IXIC_GARCH_MIDAS_Variance))

for (i in 1:nrow(IXIC_GARCH_MIDAS_Variance)) {
  
  c<-IXIC_GARCH_MIDAS_Variance[i,2]*IXIC_GARCH_MIDAS_Variance[i,3]
  c<-as.data.frame(c)
  
  if (i==1) {
    IXIC_GARCH_MIDAS_t<-c
  }
  else{
    IXIC_GARCH_MIDAS_t<-rbind(IXIC_GARCH_MIDAS_t,c)
  }
}

IXIC_GARCH_MIDAS_SD<-sqrt(IXIC_GARCH_MIDAS_t)
IXIC_mf_df<-as.data.frame(IXIC_mf$df.fitted)
IXIC1_lower<-max(which(is.na(IXIC_mf_df$g)))
IXIC1_higher<-nrow(IXIC1_return)-nrow(IXIC_GARCH_MIDAS_Variance)-IXIC1_lower
IXIC1_total<-nrow(IXIC1_return)- IXIC1_higher-1

plot(abs(as.numeric(IXIC1_return[IXIC1_lower:IXIC1_total,1]))*100
     ~IXIC_GARCH_MIDAS_Variance[,1],
     type='l',xlab='Date',ylab="Volatility", 
     main='NASDAQ GARCH-MIDAS Training Dataset',
     col='darkblue')
lines(IXIC_GARCH_MIDAS_SD[1:nrow(IXIC_GARCH_MIDAS_SD),]~IXIC_GARCH_MIDAS_Variance[,1],
      type='l',col='darkorange',lwd=2)
legend("topleft",
       legend=c('Absolute Actual Returns','GARCH-MIDAS Forecast'),
       col=c('darkblue','darkorange'),
       pch=c(9,9))

#### error for each data point within the data frame

IXIC1_return_MIDAS<-IXIC1_return[IXIC1_lower:IXIC1_total,]
IXIC1_return_MIDAS<-as.data.frame(IXIC1_return_MIDAS)

for (i in 1:nrow(IXIC1_return_MIDAS)) {
  
  d<- abs(as.numeric(IXIC1_return_MIDAS[i,])/100-as.numeric(IXIC_GARCH_MIDAS_SD[i,])/100)
  d<-as.data.frame(d)
  
  if (i==1) {
    IXIC_GARCH_MIDAS_training_error<-d
  }
  else{
    IXIC_GARCH_MIDAS_training_error<-rbind(IXIC_GARCH_MIDAS_training_error,d)
  }

}

IXIC_GARCH_MIDAS_training_error<-as.data.frame(IXIC_GARCH_MIDAS_training_error)

plot(IXIC_GARCH_MIDAS_training_error$d~IXIC_GARCH_MIDAS_Variance[,1],type='l',
     col='grey',xlab='Date',ylab='Difference to Actual', main='Inneficency of the GARCH-MIDAS Model')


# Forecasted values for period in hand

IXIC_mf_f<-simulate_mfgarch(n.days = nrow(IXIC2), mu = IXIC_mf$par[1], 
                            alpha =  IXIC_mf$par[2], beta =  IXIC_mf$par[3], 
                            gamma =IXIC_mf$par[4],m =  IXIC_mf$par[5],
                            theta = IXIC_mf$par[6], w1 = 1,
                            w2 =  IXIC_mf$par[7],
                            K = 9, psi = 0.98, sigma.psi = 0.1, low.freq = 1)
set.seed(999)
IXIC_mf_f
IXIC_mf_f<-as.data.frame(IXIC_mf_f)

for (i in 1:nrow(IXIC_mf_f)) {
  
  e<-IXIC_mf_f[i,5]*IXIC_mf_f[i,6]
  e<- as.data.frame(e)
  
  if (i==1) {
    IXIC_GARCH_MIDAS_forecast_var<-e
  }
  else{
    IXIC_GARCH_MIDAS_forecast_var<-rbind(IXIC_GARCH_MIDAS_forecast_var,e)
  }
}

IXIC_GARCH_MIDAS_forecast_var
IXIC_GARCH_MIDAS_forecast_SD<-sqrt(IXIC_GARCH_MIDAS_forecast_var)/100

plot(abs(IXIC2_returns[,])~IXIC2$Date,type='l',col='darkblue',
     xlab='Date',ylab='Volatility', main='NASDAQ GARCH-MIDAS Test',
     ylim=c(0,0.07))
lines(IXIC_GARCH_MIDAS_forecast_SD$e~IXIC2$Date,type='l',col='darkorange')
legend("topleft",
       legend=c('Absolute Actual Return','GARCH-MIDAS Forecast'),
       col=c('darkblue','darkorange'),
       pch = c(9,9))


## error for forecasting

for (i in 1:nrow(IXIC_GARCH_MIDAS_forecast_SD)) {
  
  h<-abs(IXIC2_returns[i,1]/100-IXIC_GARCH_MIDAS_forecast_SD[i,1]/100)
  h<-as.data.frame(h)
  
  if (i==1) {
    IXIC_GARCH_MIDAS_error<-h
    
  }
  else{
    IXIC_GARCH_MIDAS_error<-rbind(IXIC_GARCH_MIDAS_error,h)
  }

}
IXIC_GARCH_MIDAS_error


# Training Error for GARCH-MIDAS IXIC

for (i in 1:nrow(IXIC1_return_MIDAS)) {
  
  g<- abs(as.numeric(IXIC1_return_MIDAS[i,])/100-as.numeric(IXIC_GARCH_MIDAS_SD[i,])/100)**2
  g<-as.data.frame(g)
  
  if (i==1) {
    IXIC_GARCH_MIDAS_training_squared_error_values<-g
  }
  else{
    IXIC_GARCH_MIDAS_training_squared_error_values<-rbind(
      IXIC_GARCH_MIDAS_training_squared_error_values,g)
  }
  
}
IXIC_GARCH_MIDAS_training_squared_error_values

IXIC_GARCH_MIDAS_training_abs_error<- (sum(IXIC_GARCH_MIDAS_training_error)/
                                         nrow(IXIC_GARCH_MIDAS_training_error))
IXIC_GARCH_MIDAS_training_mean_squared_error<-(sum(IXIC_GARCH_MIDAS_training_squared_error_values)/
                                              nrow(IXIC_GARCH_MIDAS_training_squared_error_values))
IXIC_GARCH_MIDAS_training_mean_root_error<- sqrt(IXIC_GARCH_MIDAS_training_mean_squared_error)


# test error for IXIC GARCH-MIDAS

for (i in 1:nrow(IXIC_GARCH_MIDAS_forecast_SD)) {
  
  t<-abs(IXIC2_returns[i,1]/100-IXIC_GARCH_MIDAS_forecast_SD[i,1]/100)**2
  t<-as.data.frame(t)
  
  if (i==1) {
    IXIC_GARCH_MIDAS_squared_error<-t
    
  }
  else{
    IXIC_GARCH_MIDAS_squared_error<-rbind(IXIC_GARCH_MIDAS_squared_error,t)
  }
  
}


IXIC_GARCH_MIDAS_abs_error<- (sum(IXIC_GARCH_MIDAS_error)/
                                nrow(IXIC_GARCH_MIDAS_error))
IXIC_GARCH_MIDAS_mean_squared_error<- (sum(IXIC_GARCH_MIDAS_squared_error)/
                                         nrow(IXIC_GARCH_MIDAS_squared_error))
IXIC_GARCH_MIDAS_mean_root_error<-sqrt(IXIC_GARCH_MIDAS_mean_squared_error)




