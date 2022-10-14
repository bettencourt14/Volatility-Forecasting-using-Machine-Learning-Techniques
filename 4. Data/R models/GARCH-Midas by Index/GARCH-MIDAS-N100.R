### N100 GARCH-MIDAS

source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Preliminary analysis/N100 Preliminary Analysis.R")
library(mfGARCH)
library(lubridate)
library(ggplot2)

N100_MIDAS_Data <- read_excel("Low Frequency Data/N100-low freq.xlsx",
                              col_types = c("date", "numeric", "numeric",
                                            "numeric", "text", "numeric", "numeric",
                                            "text", "numeric", "numeric"))

par(mar=c(5,5,5,5))
plot(N100_MIDAS_Data$Date,y= N100_MIDAS_Data$Value_LTIR,type='l',
     col='darkblue',xlab='Date',ylab='Long Term Interest Rate % Value')
par(new=T)
plot(N100_MIDAS_Data$Date,y=N100_MIDAS_Data$Value_House,type='l',
     col='darkorange',ylab='',xlab='',axes=F,
     main=' Eurozone Long Term Interest Rate and House Pricing')
axis(side=4,at=pretty(range(N100_MIDAS_Data$Value_House)))
mtext("House Price",side=4,line=3)
legend('bottomleft',
       legend=c('Long term Interest Rate','House Pricing'),
       col=c('darkblue','darkorange'),
       pch = c(9,9))


N100_MIDAS_Data$Return<-N100_MIDAS_Data$Return*100
N100_MIDAS_Data$`LTIR return`<-N100_MIDAS_Data$`LTIR return`*100
N100_MIDAS_Data$`House return`<-N100_MIDAS_Data$`House return`*100
N100_MIDAS_Data_1<-N100_MIDAS_Data[1:nrow(N1001),]
N100_MIDAS_Data_1<- as.data.frame(N100_MIDAS_Data_1)
N100_MIDAS_Data_1[,1]<- as.Date(N100_MIDAS_Data_1[,1])
str(N100_MIDAS_Data_1)
N100_MIDAS_Data_1[,5]<- as.Date.character(N100_MIDAS_Data_1[,5], "%d/%m/%Y")
N100_MIDAS_Data_1[,8]<- as.Date.character(N100_MIDAS_Data_1[,8], "%d/%m/%Y")
str(N100_MIDAS_Data_1)
N100_MIDAS_Data_1<- na.omit(N100_MIDAS_Data_1)
which(is.na(N100_MIDAS_Data_1))
colnames(N100_MIDAS_Data_1)<-c('date','close','volume','return',
                               'time_ltir','value_ltir','ltir_return',
                               'time_house','value_house','house_return')

N100_GARCH_MIDAS<-cbind(N100_MIDAS_Data_1$date,
                        N100_MIDAS_Data_1$return,
                        N100_MIDAS_Data_1$time_ltir,
                        N100_MIDAS_Data_1$ltir_return,
                        N100_MIDAS_Data_1$house_return,
                        N100_MIDAS_Data_1$time_house)

N100_GARCH_MIDAS<-as.data.frame(N100_GARCH_MIDAS)
N100_GARCH_MIDAS[,1]<-as.Date(N100_GARCH_MIDAS[,1])
N100_GARCH_MIDAS[,3]<-as.Date(N100_GARCH_MIDAS[,3])
N100_GARCH_MIDAS[,6]<-as.Date(N100_GARCH_MIDAS[,6])
colnames(N100_GARCH_MIDAS)<-c("date","return","time_ltir",
                              "ltir_return","house_return","time_house")
str(N100_GARCH_MIDAS)
print(N100_GARCH_MIDAS)

#correlation between variables

cor(N100_GARCH_MIDAS$ltir_return,N100_GARCH_MIDAS$house_return)


# GARCH-MIDAS PROCESS

N100_mf<-fit_mfgarch(data = N100_GARCH_MIDAS,y = "return",
                     x = "house_return",low.freq = "time_house",K=4,
                     x.two = "ltir_return", K.two =12,low.freq.two = "time_ltir",
                     weighting.two = "beta.restricted")

View(N100_mf$df.fitted)

# Based on my understanding G is the conditional Variance for a given day, and by multiplying it by Tau we acheive the Conditional variance total

N100_GARCH_MIDAS_Variance<-as.data.frame(cbind(as.Date(N100_GARCH_MIDAS$date),
                                               N100_mf$g,N100_mf$tau))
N100_GARCH_MIDAS_Variance[,1]<- as.Date(N100_GARCH_MIDAS_Variance[,1])
print(N100_GARCH_MIDAS_Variance)

N100_GARCH_MIDAS_Variance<-na.omit(N100_GARCH_MIDAS_Variance)
which(is.na(N100_GARCH_MIDAS_Variance))

for (i in 1:nrow(N100_GARCH_MIDAS_Variance)) {
  
  c<-N100_GARCH_MIDAS_Variance[i,2]*N100_GARCH_MIDAS_Variance[i,3]
  c<-as.data.frame(c)
  
  if (i==1) {
    N100_GARCH_MIDAS_t<-c
  }
  else{
    N100_GARCH_MIDAS_t<-rbind(N100_GARCH_MIDAS_t,c)
  }
}

N100_GARCH_MIDAS_SD<-sqrt(N100_GARCH_MIDAS_t)
N100_mf_df<-as.data.frame(N100_mf$df.fitted)
N1001_lower<-max(which(is.na(N100_mf_df$g)))
N1001_higher<-nrow(N1001_return)-nrow(N100_GARCH_MIDAS_Variance)-N1001_lower
N1001_total<-nrow(N1001_return)- N1001_higher-1

plot(abs(as.numeric(N1001_return[N1001_lower:N1001_total,1]))*100
     ~N100_GARCH_MIDAS_Variance[,1],
     type='l',xlab='Date',ylab="Volatility", 
     main='Euronext 100 GARCH-MIDAS Training Dataset',
     col='darkblue')
lines(N100_GARCH_MIDAS_SD[1:nrow(N100_GARCH_MIDAS_SD),]~N100_GARCH_MIDAS_Variance[,1],
      type='l',col='darkorange',lwd=2)
legend("topleft",
       legend=c('Absolute Actual Returns','GARCH-MIDAS Forecast'),
       col=c('darkblue','darkorange'),
       pch=c(9,9))

#### error for each data point within the data frame

N1001_return_MIDAS<-N1001_return[N1001_lower:N1001_total,]
N1001_return_MIDAS<-as.data.frame(N1001_return_MIDAS)

for (i in 1:nrow(N1001_return_MIDAS)) {
  
  d<- abs(as.numeric(N1001_return_MIDAS[i,])/100-as.numeric(N100_GARCH_MIDAS_SD[i,])/100)
  d<-as.data.frame(d)
  
  if (i==1) {
    N100_GARCH_MIDAS_training_error<-d
  }
  else{
    N100_GARCH_MIDAS_training_error<-rbind(N100_GARCH_MIDAS_training_error,d)
  }

}

N100_GARCH_MIDAS_training_error<-as.data.frame(N100_GARCH_MIDAS_training_error)

plot(N100_GARCH_MIDAS_training_error$d~N100_GARCH_MIDAS_Variance[,1],type='l',
     col='grey',xlab='Date',ylab='Difference to Actual', main='Inneficency of the GARCH-MIDAS Model')


# Forecasted values for period in hand

N100_mf_f<-simulate_mfgarch(n.days = nrow(N1002), mu = N100_mf$par[1], 
                            alpha =  N100_mf$par[2], beta =  N100_mf$par[3], 
                            gamma =N100_mf$par[4],m =  N100_mf$par[5],
                            theta = N100_mf$par[6], w1 = 1,
                            w2 =  N100_mf$par[7],
                            K = 9, psi = 0.98, sigma.psi = 0.1, low.freq = 1)
set.seed(999)
N100_mf_f
N100_mf_f<-as.data.frame(N100_mf_f)

for (i in 1:nrow(N100_mf_f)) {
  
  e<-N100_mf_f[i,5]*N100_mf_f[i,6]
  e<- as.data.frame(e)
  
  if (i==1) {
    N100_GARCH_MIDAS_forecast_var<-e
  }
  else{
    N100_GARCH_MIDAS_forecast_var<-rbind(N100_GARCH_MIDAS_forecast_var,e)
  }
}

N100_GARCH_MIDAS_forecast_var
N100_GARCH_MIDAS_forecast_SD<-sqrt(N100_GARCH_MIDAS_forecast_var)/100

plot(abs(N1002_returns[,])~N1002$Date,type='l',col='darkblue',
     xlab='Date',ylab='Volatility', main='Euronext 100 GARCH-MIDAS Test',
     ylim=c(0,0.07))
lines(N100_GARCH_MIDAS_forecast_SD$e~N1002$Date,type='l',col='darkorange')
legend("topleft",
       legend=c('Absolute Actual Return','GARCH-MIDAS Forecast'),
       col=c('darkblue','darkorange'),
       pch = c(9,9))


## error for forecasting

for (i in 1:nrow(N100_GARCH_MIDAS_forecast_SD)) {
  
  h<-abs(N1002_returns[i,1]/100-N100_GARCH_MIDAS_forecast_SD[i,1]/100)
  h<-as.data.frame(h)
  
  if (i==1) {
    N100_GARCH_MIDAS_error<-h
    
  }
  else{
    N100_GARCH_MIDAS_error<-rbind(N100_GARCH_MIDAS_error,h)
  }

}
N100_GARCH_MIDAS_error


# Training Error for GARCH-MIDAS N100

for (i in 1:nrow(N1001_return_MIDAS)) {
  
  g<- abs(as.numeric(N1001_return_MIDAS[i,])/100-as.numeric(N100_GARCH_MIDAS_SD[i,])/100)**2
  g<-as.data.frame(g)
  
  if (i==1) {
    N100_GARCH_MIDAS_training_squared_error_values<-g
  }
  else{
    N100_GARCH_MIDAS_training_squared_error_values<-rbind(
      N100_GARCH_MIDAS_training_squared_error_values,g)
  }
  
}
N100_GARCH_MIDAS_training_squared_error_values

N100_GARCH_MIDAS_training_abs_error<- (sum(N100_GARCH_MIDAS_training_error)/
                                         nrow(N100_GARCH_MIDAS_training_error))
N100_GARCH_MIDAS_training_mean_squared_error<-(sum(N100_GARCH_MIDAS_training_squared_error_values)/
                                              nrow(N100_GARCH_MIDAS_training_squared_error_values))
N100_GARCH_MIDAS_training_mean_root_error<- sqrt(N100_GARCH_MIDAS_training_mean_squared_error)


# test error for N100 GARCH-MIDAS

for (i in 1:nrow(N100_GARCH_MIDAS_forecast_SD)) {
  
  t<-abs(N1002_returns[i,1]/100-N100_GARCH_MIDAS_forecast_SD[i,1]/100)**2
  t<-as.data.frame(t)
  
  if (i==1) {
    N100_GARCH_MIDAS_squared_error<-t
    
  }
  else{
    N100_GARCH_MIDAS_squared_error<-rbind(N100_GARCH_MIDAS_squared_error,t)
  }
  
}


N100_GARCH_MIDAS_abs_error<- (sum(N100_GARCH_MIDAS_error)/
                                nrow(N100_GARCH_MIDAS_error))
N100_GARCH_MIDAS_mean_squared_error<- (sum(N100_GARCH_MIDAS_squared_error)/
                                         nrow(N100_GARCH_MIDAS_squared_error))
N100_GARCH_MIDAS_mean_root_error<-sqrt(N100_GARCH_MIDAS_mean_squared_error)




