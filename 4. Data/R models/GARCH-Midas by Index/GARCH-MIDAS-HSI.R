### HSI GARCH-MIDAS

source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Preliminary analysis/HSI Preliminary Analysis.R")
library(mfGARCH)
library(lubridate)
library(ggplot2)

HSI_MIDAS_Data <- read_excel("Low Frequency Data/HSI-low freq.xlsx",
                              col_types = c("date", "numeric", "numeric",
                                            "numeric", "text", "numeric", "numeric",
                                            "text", "numeric", "numeric"))

par(mar=c(5,5,5,5))
plot(HSI_MIDAS_Data$Date,y= HSI_MIDAS_Data$Value_LTIR,type='l',
     col='darkblue',xlab='Date',ylab='Long Term Interest Rate % Value')
par(new=T)
plot(HSI_MIDAS_Data$Date,y=HSI_MIDAS_Data$Value_House,type='l',
     col='darkorange',ylab='',xlab='',axes=F,
     main=' Hong Kong Long Term Interest Rate and House Pricing')
axis(side=4,at=pretty(range(HSI_MIDAS_Data$Value_House,na.rm=T)))
mtext("House Price",side=4,line=3)
legend('bottomleft',
       legend=c('Long term Interest Rate','House Pricing'),
       col=c('darkblue','darkorange'),
       pch = c(9,9))



HSI_MIDAS_Data$Return<-HSI_MIDAS_Data$Return*100
HSI_MIDAS_Data$`LTIR return`<-HSI_MIDAS_Data$`LTIR return`*100
HSI_MIDAS_Data$`House return`<-HSI_MIDAS_Data$`House return`*100
HSI_MIDAS_Data_1<-HSI_MIDAS_Data[1:nrow(HSI1),]
HSI_MIDAS_Data_1<- as.data.frame(HSI_MIDAS_Data_1)
HSI_MIDAS_Data_1[,1]<- as.Date(HSI_MIDAS_Data_1[,1])
str(HSI_MIDAS_Data_1)
HSI_MIDAS_Data_1[,5]<- as.Date.character(HSI_MIDAS_Data_1[,5], "%d/%m/%Y")
HSI_MIDAS_Data_1[,8]<- as.Date.character(HSI_MIDAS_Data_1[,8], "%d/%m/%Y")
str(HSI_MIDAS_Data_1)
HSI_MIDAS_Data_1<- na.omit(HSI_MIDAS_Data_1)
which(is.na(HSI_MIDAS_Data_1))
colnames(HSI_MIDAS_Data_1)<-c('date','close','volume','return',
                               'time_ltir','value_ltir','ltir_return',
                               'time_house','value_house','house_return')

HSI_GARCH_MIDAS<-cbind(HSI_MIDAS_Data_1$date,
                        HSI_MIDAS_Data_1$return,
                        HSI_MIDAS_Data_1$time_ltir,
                        HSI_MIDAS_Data_1$ltir_return,
                        HSI_MIDAS_Data_1$house_return,
                        HSI_MIDAS_Data_1$time_house)

HSI_GARCH_MIDAS<-as.data.frame(HSI_GARCH_MIDAS)
HSI_GARCH_MIDAS[,1]<-as.Date(HSI_GARCH_MIDAS[,1])
HSI_GARCH_MIDAS[,3]<-as.Date(HSI_GARCH_MIDAS[,3])
HSI_GARCH_MIDAS[,6]<-as.Date(HSI_GARCH_MIDAS[,6])
colnames(HSI_GARCH_MIDAS)<-c("date","return","time_ltir",
                              "ltir_return","house_return","time_house")
str(HSI_GARCH_MIDAS)
print(HSI_GARCH_MIDAS)

#correlation between variables

cor(HSI_GARCH_MIDAS$ltir_return,HSI_GARCH_MIDAS$house_return)


# GARCH-MIDAS PROCESS

HSI_mf<-fit_mfgarch(data = HSI_GARCH_MIDAS,y = "return",
                     x = "house_return",low.freq = "time_house",K=4,
                     x.two = "ltir_return", K.two =12,low.freq.two = "time_ltir",
                     weighting.two = "beta.restricted")

View(HSI_mf$df.fitted)

# Based on my understanding G is the conditional Variance for a given day, and by multiplying it by Tau we acheive the Conditional variance total

HSI_GARCH_MIDAS_Variance<-as.data.frame(cbind(as.Date(HSI_GARCH_MIDAS$date),
                                               HSI_mf$g,HSI_mf$tau))
HSI_GARCH_MIDAS_Variance[,1]<- as.Date(HSI_GARCH_MIDAS_Variance[,1])
print(HSI_GARCH_MIDAS_Variance)

HSI_GARCH_MIDAS_Variance<-na.omit(HSI_GARCH_MIDAS_Variance)
which(is.na(HSI_GARCH_MIDAS_Variance))

for (i in 1:nrow(HSI_GARCH_MIDAS_Variance)) {
  
  c<-HSI_GARCH_MIDAS_Variance[i,2]*HSI_GARCH_MIDAS_Variance[i,3]
  c<-as.data.frame(c)
  
  if (i==1) {
    HSI_GARCH_MIDAS_t<-c
  }
  else{
    HSI_GARCH_MIDAS_t<-rbind(HSI_GARCH_MIDAS_t,c)
  }
}

HSI_GARCH_MIDAS_SD<-sqrt(HSI_GARCH_MIDAS_t)
HSI_mf_df<-as.data.frame(HSI_mf$df.fitted)
HSI1_lower<-max(which(is.na(HSI_mf_df$g)))
HSI1_higher<-nrow(HSI1_return)-nrow(HSI_GARCH_MIDAS_Variance)-HSI1_lower
HSI1_total<-nrow(HSI1_return)- HSI1_higher-1

plot(abs(as.numeric(HSI1_return[HSI1_lower:HSI1_total,1]))*100
     ~HSI_GARCH_MIDAS_Variance[,1],
     type='l',xlab='Date',ylab="Volatility", 
     main='Hang Seng GARCH-MIDAS Training Dataset',
     col='darkblue')
lines(HSI_GARCH_MIDAS_SD[1:nrow(HSI_GARCH_MIDAS_SD),]~HSI_GARCH_MIDAS_Variance[,1],
      type='l',col='darkorange',lwd=2)
legend("topleft",
       legend=c('Absolute Actual Returns','GARCH-MIDAS Forecast'),
       col=c('darkblue','darkorange'),
       pch=c(9,9))

#### error for each data point within the data frame

HSI1_return_MIDAS<-HSI1_return[HSI1_lower:HSI1_total,]
HSI1_return_MIDAS<-as.data.frame(HSI1_return_MIDAS)

for (i in 1:nrow(HSI1_return_MIDAS)) {
  
  d<- abs(as.numeric(HSI1_return_MIDAS[i,])/100-as.numeric(HSI_GARCH_MIDAS_SD[i,])/100)
  d<-as.data.frame(d)
  
  if (i==1) {
    HSI_GARCH_MIDAS_training_error<-d
  }
  else{
    HSI_GARCH_MIDAS_training_error<-rbind(HSI_GARCH_MIDAS_training_error,d)
  }

}

HSI_GARCH_MIDAS_training_error<-as.data.frame(HSI_GARCH_MIDAS_training_error)

plot(HSI_GARCH_MIDAS_training_error$d~HSI_GARCH_MIDAS_Variance[,1],type='l',
     col='grey',xlab='Date',ylab='Difference to Actual', main='Inneficency of the GARCH-MIDAS Model')


# Forecasted values for period in hand

HSI_mf_f<-simulate_mfgarch(n.days = nrow(HSI2), mu = HSI_mf$par[1], 
                            alpha =  HSI_mf$par[2], beta =  HSI_mf$par[3], 
                            gamma =HSI_mf$par[4],m =  HSI_mf$par[5],
                            theta = HSI_mf$par[6], w1 = 1,
                            w2 =  HSI_mf$par[7],
                            K = 9, psi = 0.98, sigma.psi = 0.1, low.freq = 1)
set.seed(999)
HSI_mf_f
HSI_mf_f<-as.data.frame(HSI_mf_f)

for (i in 1:nrow(HSI_mf_f)) {
  
  e<-HSI_mf_f[i,5]*HSI_mf_f[i,6]
  e<- as.data.frame(e)
  
  if (i==1) {
    HSI_GARCH_MIDAS_forecast_var<-e
  }
  else{
    HSI_GARCH_MIDAS_forecast_var<-rbind(HSI_GARCH_MIDAS_forecast_var,e)
  }
}

HSI_GARCH_MIDAS_forecast_var
HSI_GARCH_MIDAS_forecast_SD<-sqrt(HSI_GARCH_MIDAS_forecast_var)/100

plot(abs(HSI2_returns[,])~HSI2$Date,type='l',col='darkblue',
     xlab='Date',ylab='Volatility', main='Hang Seng GARCH-MIDAS Test',
     ylim=c(0,0.07))
lines(HSI_GARCH_MIDAS_forecast_SD$e~HSI2$Date,type='l',col='darkorange')
legend("topleft",
       legend=c('Absolute Actual Return','GARCH-MIDAS Forecast'),
       col=c('darkblue','darkorange'),
       pch = c(9,9))


## error for forecasting

for (i in 1:nrow(HSI_GARCH_MIDAS_forecast_SD)) {
  
  h<-abs(HSI2_returns[i,1]/100-HSI_GARCH_MIDAS_forecast_SD[i,1]/100)
  h<-as.data.frame(h)
  
  if (i==1) {
    HSI_GARCH_MIDAS_error<-h
    
  }
  else{
    HSI_GARCH_MIDAS_error<-rbind(HSI_GARCH_MIDAS_error,h)
  }

}
HSI_GARCH_MIDAS_error


# Training Error for GARCH-MIDAS HSI

for (i in 1:nrow(HSI1_return_MIDAS)) {
  
  g<- abs(as.numeric(HSI1_return_MIDAS[i,])/100-as.numeric(HSI_GARCH_MIDAS_SD[i,])/100)**2
  g<-as.data.frame(g)
  
  if (i==1) {
    HSI_GARCH_MIDAS_training_squared_error_values<-g
  }
  else{
    HSI_GARCH_MIDAS_training_squared_error_values<-rbind(
      HSI_GARCH_MIDAS_training_squared_error_values,g)
  }
  
}
HSI_GARCH_MIDAS_training_squared_error_values

HSI_GARCH_MIDAS_training_abs_error<- (sum(HSI_GARCH_MIDAS_training_error)/
                                         nrow(HSI_GARCH_MIDAS_training_error))
HSI_GARCH_MIDAS_training_mean_squared_error<-(sum(HSI_GARCH_MIDAS_training_squared_error_values)/
                                              nrow(HSI_GARCH_MIDAS_training_squared_error_values))
HSI_GARCH_MIDAS_training_mean_root_error<- sqrt(HSI_GARCH_MIDAS_training_mean_squared_error)


# test error for HSI GARCH-MIDAS

for (i in 1:nrow(HSI_GARCH_MIDAS_forecast_SD)) {
  
  t<-abs(HSI2_returns[i,1]/100-HSI_GARCH_MIDAS_forecast_SD[i,1]/100)**2
  t<-as.data.frame(t)
  
  if (i==1) {
    HSI_GARCH_MIDAS_squared_error<-t
    
  }
  else{
    HSI_GARCH_MIDAS_squared_error<-rbind(HSI_GARCH_MIDAS_squared_error,t)
  }
  
}


HSI_GARCH_MIDAS_abs_error<- (sum(HSI_GARCH_MIDAS_error)/
                                nrow(HSI_GARCH_MIDAS_error))
HSI_GARCH_MIDAS_mean_squared_error<- (sum(HSI_GARCH_MIDAS_squared_error)/
                                         nrow(HSI_GARCH_MIDAS_squared_error))
HSI_GARCH_MIDAS_mean_root_error<-sqrt(HSI_GARCH_MIDAS_mean_squared_error)




