### BVSP GARCH-MIDAS

source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Preliminary analysis/BVSP Preliminary Analysis.R")
library(mfGARCH)
library(lubridate)

BVSP_MIDAS_Data <- read_excel("Low Frequency Data/BVSP-low freq.xlsx",
                              col_types = c("date", "numeric", "numeric",
                                            "numeric", "text", "numeric", "numeric",
                                            "text", "numeric", "numeric"))

BVSP_MIDAS_Data$Return<-BVSP_MIDAS_Data$Return*100
BVSP_MIDAS_Data$`LTIR return`<-BVSP_MIDAS_Data$`LTIR return`*100
BVSP_MIDAS_Data$`House return`<-BVSP_MIDAS_Data$`House return`*100
BVSP_MIDAS_Data_1<-BVSP_MIDAS_Data[1:nrow(BVSP1),]
BVSP_MIDAS_Data_1<- as.data.frame(BVSP_MIDAS_Data_1)
BVSP_MIDAS_Data_1[,1]<- as.Date(BVSP_MIDAS_Data_1[,1])
str(BVSP_MIDAS_Data_1)
BVSP_MIDAS_Data_1[,5]<- as.Date.character(BVSP_MIDAS_Data_1[,5], "%d/%m/%Y")
BVSP_MIDAS_Data_1[,8]<- as.Date.character(BVSP_MIDAS_Data_1[,8], "%d/%m/%Y")
str(BVSP_MIDAS_Data_1)
BVSP_MIDAS_Data_1<- na.omit(BVSP_MIDAS_Data_1)
which(is.na(BVSP_MIDAS_Data_1))
colnames(BVSP_MIDAS_Data_1)<-c('date','close','volume','return',
                               'time_ltir','value_ltir','ltir_return',
                               'time_house','value_house','house_return')

BVSP_GARCH_MIDAS<-cbind(BVSP_MIDAS_Data_1$date,
                        BVSP_MIDAS_Data_1$return,
                        BVSP_MIDAS_Data_1$time_ltir,
                        BVSP_MIDAS_Data_1$ltir_return,
                        BVSP_MIDAS_Data_1$house_return,
                        BVSP_MIDAS_Data_1$time_house)

BVSP_GARCH_MIDAS<-as.data.frame(BVSP_GARCH_MIDAS)
BVSP_GARCH_MIDAS[,1]<-as.Date(BVSP_GARCH_MIDAS[,1])
BVSP_GARCH_MIDAS[,3]<-as.Date(BVSP_GARCH_MIDAS[,3])
BVSP_GARCH_MIDAS[,6]<-as.Date(BVSP_GARCH_MIDAS[,6])
colnames(BVSP_GARCH_MIDAS)<-c("date","return","time_ltir",
                              "ltir_return","house_return","time_house")
str(BVSP_GARCH_MIDAS)
print(BVSP_GARCH_MIDAS)

#correlation between variables

cor(BVSP_GARCH_MIDAS$ltir_return,BVSP_GARCH_MIDAS$house_return)


# GARCH-MIDAS PROCESS

BVSP_mf<-fit_mfgarch(data = BVSP_GARCH_MIDAS,y = "return",
                     x = "house_return",low.freq = "time_house",K=4,
                     x.two = "ltir_return", K.two =12,low.freq.two = "time_ltir",
                     weighting.two = "beta.restricted")

View(BVSP_mf$df.fitted)

# Based on my understanding G is the conditional Variance for a given day, and by multiplying it by Tau we acheive the Conditional variance total

BVSP_GARCH_MIDAS_Variance<-as.data.frame(cbind(as.Date(BVSP_GARCH_MIDAS$date),
                                               BVSP_mf$g,BVSP_mf$tau))
BVSP_GARCH_MIDAS_Variance[,1]<- as.Date(BVSP_GARCH_MIDAS_Variance[,1])
print(BVSP_GARCH_MIDAS_Variance)

BVSP_GARCH_MIDAS_Variance<-na.omit(BVSP_GARCH_MIDAS_Variance)
which(is.na(BVSP_GARCH_MIDAS_Variance))

for (i in 1:nrow(BVSP_GARCH_MIDAS_Variance)) {
  
  c<-BVSP_GARCH_MIDAS_Variance[i,2]*BVSP_GARCH_MIDAS_Variance[i,3]
  c<-as.data.frame(c)
  
  if (i==1) {
    BVSP_GARCH_MIDAS_t<-c
  }
  else{
    BVSP_GARCH_MIDAS_t<-rbind(BVSP_GARCH_MIDAS_t,c)
  }
}

BVSP_GARCH_MIDAS_SD<-sqrt(BVSP_GARCH_MIDAS_t)
BVSP_mf_df<-as.data.frame(BVSP_mf$df.fitted)
BVSP1_lower<-max(which(is.na(BVSP_mf_df$g)))
BVSP1_higher<-nrow(BVSP1_return)-nrow(BVSP_GARCH_MIDAS_Variance)-BVSP1_lower
BVSP1_total<-nrow(BVSP1_return)- BVSP1_higher-1

plot(abs(as.numeric(BVSP1_return[BVSP1_lower:BVSP1_total,1]))*100
     ~BVSP_GARCH_MIDAS_Variance[,1],
     type='l',xlab='Date',ylab="Conditional SD as %", 
     main='IBOVESPA GARCH-MIDAS Conditonal SD vs. Actual Returns',
     col='grey')
lines(BVSP_GARCH_MIDAS_SD[1:nrow(BVSP_GARCH_MIDAS_SD),]~BVSP_GARCH_MIDAS_Variance[,1],
      type='l',col='Orange')

#### error for each data poitn within the data frame

BVSP1_return_MIDAS<-BVSP1_return[BVSP1_lower:BVSP1_total,]
BVSP1_return_MIDAS<-as.data.frame(BVSP1_return_MIDAS)

for (i in 1:nrow(BVSP1_return_MIDAS)) {
  
  d<- abs(as.numeric(BVSP1_return_MIDAS[i,])/100-as.numeric(BVSP_GARCH_MIDAS_SD[i,])/100)
  d<-as.data.frame(d)
  
  if (i==1) {
    BVSP_GARCH_MIDAS_training_error<-d
  }
  else{
    BVSP_GARCH_MIDAS_training_error<-rbind(BVSP_GARCH_MIDAS_training_error,d)
  }

}

BVSP_GARCH_MIDAS_training_error<-as.data.frame(BVSP_GARCH_MIDAS_training_error)

plot(BVSP_GARCH_MIDAS_training_error$d~BVSP_GARCH_MIDAS_Variance[,1],type='l',
     col='grey',xlab='Date',ylab='Difference to Actual', main='Inneficency of the GARCH-MIDAS Model')


# Forecasted values for period in hand

BVSP_mf_f<-simulate_mfgarch(n.days = nrow(BVSP2), mu = BVSP_mf$par[1], 
                            alpha =  BVSP_mf$par[2], beta =  BVSP_mf$par[3], 
                            gamma =BVSP_mf$par[4],m =  BVSP_mf$par[5],
                            theta = BVSP_mf$par[6], w1 = 1,
                            w2 =  BVSP_mf$par[7],
                            K = 9, psi = 0.98, sigma.psi = 0.1, low.freq = 1)
set.seed(999)
BVSP_mf_f
BVSP_mf_f<-as.data.frame(BVSP_mf_f)

for (i in 1:nrow(BVSP_mf_f)) {
  
  e<-BVSP_mf_f[i,5]*BVSP_mf_f[i,6]
  e<- as.data.frame(e)
  
  if (i==1) {
    BVSP_GARCH_MIDAS_forecast_var<-e
  }
  else{
    BVSP_GARCH_MIDAS_forecast_var<-rbind(BVSP_GARCH_MIDAS_forecast_var,e)
  }
}

BVSP_GARCH_MIDAS_forecast_var
BVSP_GARCH_MIDAS_forecast_SD<-sqrt(BVSP_GARCH_MIDAS_forecast_var)/100

plot(abs(BVSP2_returns[,])~BVSP2$Date,type='l',col='black',
     xlab='Date',ylab='Return', main='IBOVESPA Absolute returns vs. GARCH-MIDAS Forecast',
     ylim=c(0,0.07))
lines(BVSP_GARCH_MIDAS_forecast_SD$e~BVSP2$Date,type='l',col='Orange')


## error for forecasting

for (i in 1:nrow(BVSP_GARCH_MIDAS_forecast_SD)) {
  
  h<-abs(BVSP2_returns[i,1]/100-BVSP_GARCH_MIDAS_forecast_SD[i,1]/100)
  h<-as.data.frame(h)
  
  if (i==1) {
    BVSP_GARCH_MIDAS_error<-h
    
  }
  else{
    BVSP_GARCH_MIDAS_error<-rbind(BVSP_GARCH_MIDAS_error,h)
  }

}
BVSP_GARCH_MIDAS_error


# Training Error for GARCH-MIDAS BVSP

for (i in 1:nrow(BVSP1_return_MIDAS)) {
  
  g<- abs(as.numeric(BVSP1_return_MIDAS[i,])/100-as.numeric(BVSP_GARCH_MIDAS_SD[i,])/100)**2
  g<-as.data.frame(g)
  
  if (i==1) {
    BVSP_GARCH_MIDAS_training_squared_error_values<-g
  }
  else{
    BVSP_GARCH_MIDAS_training_squared_error_values<-rbind(
      BVSP_GARCH_MIDAS_training_squared_error_values,g)
  }
  
}
BVSP_GARCH_MIDAS_training_squared_error_values

BVSP_GARCH_MIDAS_training_abs_error<- (sum(BVSP_GARCH_MIDAS_training_error)/
                                         nrow(BVSP_GARCH_MIDAS_training_error))
BVSP_GARCH_MIDAS_training_mean_squared_error<-(sum(BVSP_GARCH_MIDAS_training_squared_error_values)/
                                              nrow(BVSP_GARCH_MIDAS_training_squared_error_values))
BVSP_GARCH_MIDAS_training_mean_root_error<- sqrt(BVSP_GARCH_MIDAS_training_mean_squared_error)


# test error for BVSP GARCH-MIDAS

for (i in 1:nrow(BVSP_GARCH_MIDAS_forecast_SD)) {
  
  t<-abs(BVSP2_returns[i,1]/100-BVSP_GARCH_MIDAS_forecast_SD[i,1]/100)**2
  t<-as.data.frame(t)
  
  if (i==1) {
    BVSP_GARCH_MIDAS_squared_error<-t
    
  }
  else{
    BVSP_GARCH_MIDAS_squared_error<-rbind(BVSP_GARCH_MIDAS_squared_error,t)
  }
  
}


BVSP_GARCH_MIDAS_abs_error<- (sum(BVSP_GARCH_MIDAS_error)/
                                nrow(BVSP_GARCH_MIDAS_error))
BVSP_GARCH_MIDAS_mean_squared_error<- (sum(BVSP_GARCH_MIDAS_squared_error)/
                                         nrow(BVSP_GARCH_MIDAS_squared_error))
BVSP_GARCH_MIDAS_mean_root_error<-sqrt(BVSP_GARCH_MIDAS_mean_squared_error)




