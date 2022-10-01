### NSEI GARCH-MIDAS

source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Preliminary analysis/NSEI Preliminary Analysis.R")
library(mfGARCH)
library(lubridate)

NSEI_MIDAS_Data <- read_excel("Low Frequency Data/NSEI-low freq.xlsx",
                              col_types = c("date", "numeric", "numeric",
                                            "numeric", "text", "numeric", "numeric",
                                            "text", "numeric", "numeric"))

NSEI_MIDAS_Data$Return<-NSEI_MIDAS_Data$Return*100
NSEI_MIDAS_Data$`LTIR return`<-NSEI_MIDAS_Data$`LTIR return`*100
NSEI_MIDAS_Data$`House return`<-NSEI_MIDAS_Data$`House return`*100
NSEI_MIDAS_Data_1<-NSEI_MIDAS_Data[1:nrow(NSEI1),]
NSEI_MIDAS_Data_1<- as.data.frame(NSEI_MIDAS_Data_1)
NSEI_MIDAS_Data_1[,1]<- as.Date(NSEI_MIDAS_Data_1[,1])
str(NSEI_MIDAS_Data_1)
NSEI_MIDAS_Data_1[,5]<- as.Date.character(NSEI_MIDAS_Data_1[,5], "%d/%m/%Y")
NSEI_MIDAS_Data_1[,8]<- as.Date.character(NSEI_MIDAS_Data_1[,8], "%d/%m/%Y")
str(NSEI_MIDAS_Data_1)
NSEI_MIDAS_Data_1<- na.omit(NSEI_MIDAS_Data_1)
which(is.na(NSEI_MIDAS_Data_1))
colnames(NSEI_MIDAS_Data_1)<-c('date','close','volume','return',
                               'time_ltir','value_ltir','ltir_return',
                               'time_house','value_house','house_return')

NSEI_GARCH_MIDAS<-cbind(NSEI_MIDAS_Data_1$date,
                        NSEI_MIDAS_Data_1$return,
                        NSEI_MIDAS_Data_1$time_ltir,
                        NSEI_MIDAS_Data_1$ltir_return,
                        NSEI_MIDAS_Data_1$house_return,
                        NSEI_MIDAS_Data_1$time_house)

NSEI_GARCH_MIDAS<-as.data.frame(NSEI_GARCH_MIDAS)
NSEI_GARCH_MIDAS[,1]<-as.Date(NSEI_GARCH_MIDAS[,1])
NSEI_GARCH_MIDAS[,3]<-as.Date(NSEI_GARCH_MIDAS[,3])
NSEI_GARCH_MIDAS[,6]<-as.Date(NSEI_GARCH_MIDAS[,6])
colnames(NSEI_GARCH_MIDAS)<-c("date","return","time_ltir",
                              "ltir_return","house_return","time_house")
str(NSEI_GARCH_MIDAS)
print(NSEI_GARCH_MIDAS)

#correlation between variables

cor(NSEI_GARCH_MIDAS$ltir_return,NSEI_GARCH_MIDAS$house_return)


# GARCH-MIDAS PROCESS

NSEI_mf<-fit_mfgarch(data = NSEI_GARCH_MIDAS,y = "return",
                     x = "house_return",low.freq = "time_house",K=4,
                     x.two = "ltir_return", K.two =12,low.freq.two = "time_ltir",
                     weighting.two = "beta.restricted")

View(NSEI_mf$df.fitted)

# Based on my understanding G is the conditional Variance for a given day, and by multiplying it by Tau we acheive the Conditional variance total

NSEI_GARCH_MIDAS_Variance<-as.data.frame(cbind(as.Date(NSEI_GARCH_MIDAS$date),
                                               NSEI_mf$g,NSEI_mf$tau))
NSEI_GARCH_MIDAS_Variance[,1]<- as.Date(NSEI_GARCH_MIDAS_Variance[,1])
print(NSEI_GARCH_MIDAS_Variance)

NSEI_GARCH_MIDAS_Variance<-na.omit(NSEI_GARCH_MIDAS_Variance)
which(is.na(NSEI_GARCH_MIDAS_Variance))

for (i in 1:nrow(NSEI_GARCH_MIDAS_Variance)) {
  
  c<-NSEI_GARCH_MIDAS_Variance[i,2]*NSEI_GARCH_MIDAS_Variance[i,3]
  c<-as.data.frame(c)
  
  if (i==1) {
    NSEI_GARCH_MIDAS_t<-c
  }
  else{
    NSEI_GARCH_MIDAS_t<-rbind(NSEI_GARCH_MIDAS_t,c)
  }
}

NSEI_GARCH_MIDAS_SD<-sqrt(NSEI_GARCH_MIDAS_t)
NSEI_mf_df<-as.data.frame(NSEI_mf$df.fitted)
NSEI1_lower<-max(which(is.na(NSEI_mf_df$g)))
NSEI1_higher<-nrow(NSEI1_return)-nrow(NSEI_GARCH_MIDAS_Variance)-NSEI1_lower
NSEI1_total<-nrow(NSEI1_return)- NSEI1_higher-1

plot(abs(as.numeric(NSEI1_return[NSEI1_lower:NSEI1_total,1]))*100
     ~NSEI_GARCH_MIDAS_Variance[,1],
     type='l',xlab='Date',ylab="Conditional SD as %", 
     main='NIFTY 50 GARCH-MIDAS Conditonal SD vs. Actual Returns',
     col='grey')
lines(NSEI_GARCH_MIDAS_SD[1:nrow(NSEI_GARCH_MIDAS_SD),]~NSEI_GARCH_MIDAS_Variance[,1],
      type='l',col='Orange')

#### error for each data poitn within the data frame

NSEI1_return_MIDAS<-NSEI1_return[NSEI1_lower:NSEI1_total,]
NSEI1_return_MIDAS<-as.data.frame(NSEI1_return_MIDAS)

for (i in 1:nrow(NSEI1_return_MIDAS)) {
  
  d<- abs(as.numeric(NSEI1_return_MIDAS[i,])/100-as.numeric(NSEI_GARCH_MIDAS_SD[i,])/100)
  d<-as.data.frame(d)
  
  if (i==1) {
    NSEI_GARCH_MIDAS_training_error<-d
  }
  else{
    NSEI_GARCH_MIDAS_training_error<-rbind(NSEI_GARCH_MIDAS_training_error,d)
  }

}

NSEI_GARCH_MIDAS_training_error<-as.data.frame(NSEI_GARCH_MIDAS_training_error)

plot(NSEI_GARCH_MIDAS_training_error$d~NSEI_GARCH_MIDAS_Variance[,1],type='l',
     col='grey',xlab='Date',ylab='Difference to Actual', main='Inneficency of the GARCH-MIDAS Model')


# Forecasted values for period in hand

NSEI_mf_f<-simulate_mfgarch(n.days = nrow(NSEI2), mu = NSEI_mf$par[1], 
                            alpha =  NSEI_mf$par[2], beta =  NSEI_mf$par[3], 
                            gamma =NSEI_mf$par[4],m =  NSEI_mf$par[5],
                            theta = NSEI_mf$par[6], w1 = 1,
                            w2 =  NSEI_mf$par[7],
                            K = 9, psi = 0.98, sigma.psi = 0.1, low.freq = 1)
set.seed(999)
NSEI_mf_f
NSEI_mf_f<-as.data.frame(NSEI_mf_f)

for (i in 1:nrow(NSEI_mf_f)) {
  
  e<-NSEI_mf_f[i,5]*NSEI_mf_f[i,6]
  e<- as.data.frame(e)
  
  if (i==1) {
    NSEI_GARCH_MIDAS_forecast_var<-e
  }
  else{
    NSEI_GARCH_MIDAS_forecast_var<-rbind(NSEI_GARCH_MIDAS_forecast_var,e)
  }
}

NSEI_GARCH_MIDAS_forecast_var
NSEI_GARCH_MIDAS_forecast_SD<-sqrt(NSEI_GARCH_MIDAS_forecast_var)/100

plot(abs(NSEI2_returns[,])~NSEI2$Date,type='l',col='black',
     xlab='Date',ylab='Return', main='NIFTY 50 Absolute returns vs. GARCH-MIDAS Forecast',
     ylim=c(0,0.07))
lines(NSEI_GARCH_MIDAS_forecast_SD$e~NSEI2$Date,type='l',col='Orange')


## error for forecasting

for (i in 1:nrow(NSEI_GARCH_MIDAS_forecast_SD)) {
  
  h<-abs(NSEI2_returns[i,1]/100-NSEI_GARCH_MIDAS_forecast_SD[i,1]/100)
  h<-as.data.frame(h)
  
  if (i==1) {
    NSEI_GARCH_MIDAS_error<-h
    
  }
  else{
    NSEI_GARCH_MIDAS_error<-rbind(NSEI_GARCH_MIDAS_error,h)
  }

}
NSEI_GARCH_MIDAS_error


# Training Error for GARCH-MIDAS NSEI

for (i in 1:nrow(NSEI1_return_MIDAS)) {
  
  g<- abs(as.numeric(NSEI1_return_MIDAS[i,])/100-as.numeric(NSEI_GARCH_MIDAS_SD[i,])/100)**2
  g<-as.data.frame(g)
  
  if (i==1) {
    NSEI_GARCH_MIDAS_training_squared_error_values<-g
  }
  else{
    NSEI_GARCH_MIDAS_training_squared_error_values<-rbind(
      NSEI_GARCH_MIDAS_training_squared_error_values,g)
  }
  
}
NSEI_GARCH_MIDAS_training_squared_error_values

NSEI_GARCH_MIDAS_training_abs_error<- (sum(NSEI_GARCH_MIDAS_training_error)/
                                         nrow(NSEI_GARCH_MIDAS_training_error))
NSEI_GARCH_MIDAS_training_mean_squared_error<-(sum(NSEI_GARCH_MIDAS_training_squared_error_values)/
                                              nrow(NSEI_GARCH_MIDAS_training_squared_error_values))
NSEI_GARCH_MIDAS_training_mean_root_error<- sqrt(NSEI_GARCH_MIDAS_training_mean_squared_error)


# test error for NSEI GARCH-MIDAS

for (i in 1:nrow(NSEI_GARCH_MIDAS_forecast_SD)) {
  
  t<-abs(NSEI2_returns[i,1]/100-NSEI_GARCH_MIDAS_forecast_SD[i,1]/100)**2
  t<-as.data.frame(t)
  
  if (i==1) {
    NSEI_GARCH_MIDAS_squared_error<-t
    
  }
  else{
    NSEI_GARCH_MIDAS_squared_error<-rbind(NSEI_GARCH_MIDAS_squared_error,t)
  }
  
}


NSEI_GARCH_MIDAS_abs_error<- (sum(NSEI_GARCH_MIDAS_error)/
                                nrow(NSEI_GARCH_MIDAS_error))
NSEI_GARCH_MIDAS_mean_squared_error<- (sum(NSEI_GARCH_MIDAS_squared_error)/
                                         nrow(NSEI_GARCH_MIDAS_squared_error))
NSEI_GARCH_MIDAS_mean_root_error<-sqrt(NSEI_GARCH_MIDAS_mean_squared_error)




