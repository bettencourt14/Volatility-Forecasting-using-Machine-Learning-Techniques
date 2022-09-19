### ERROR TABLES #####


#Start only for N100 for simplification


library(tidyverse)
source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/3.R script with all by index/All code.R")

# N100 Actual Returns 
N100_actual_return_for_forecasting<- as.data.frame(N1002_actual_returns$N1002_actual_returns)
N100_Garch_forecasted_returns<- as.data.frame(N1002_forecast_sigma)
N100_MC_forecasted_returns<- as.data.frame(N100_error_means)

# N100 Error Table by country and training and test
N100_r1<- cbind('GARCH Training','GARCH Test','Monte Carlo')

N100_c1<-rbind('Model/data set','Mean Absolute Error','Mean Squared Error',
          'Root Mean Squared Error')

N100_r2<- cbind(N100_Garch_training_abs_error,N100_Garch_abs_error, N100_MC_absoulte_error)
N100_r3<-cbind(N100_Garch_training_squared_error,N100_Garch_squared_error, N100_MC_squared_mean_error)
N100_r4<-cbind(N100_Garch_training_root_error,N100_Garch_root_error,N100_MC_root_mean_error)
N100_rtotal<-rbind(N100_r1,N100_r2,N100_r3,N100_r4)
N100_Table<-cbind(N100_c1,N100_rtotal)

colnames(N100_Table)<-c('Country Index',"N100","N100","N100")
View(N100_Table)
