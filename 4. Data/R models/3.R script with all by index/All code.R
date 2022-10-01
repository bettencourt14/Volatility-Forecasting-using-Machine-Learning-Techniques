library(tidyverse)
library(multDM)

list.files(c('E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Monte Carlo by Index',
             'E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/GARCH by Index',
             'E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/GARCH-Midas by Index'),
           full.names =T ) %>% map(source)

#Below values for Diebold MAriano Test

# N100 Actual Returns 
N100_real_DM<-N1002_actual_returns$N1002_actual_returns

# N100 Forecasted returns
N100_MC_DM<-N100_MC_forecasted_returns$N100_error_means
N100_GARCH_DM<-N100_Garch_forecasted_returns$`1975-01-01`
N100_GARCH_MIDAS_DM<-N100_GARCH_MIDAS_forecast_SD

# N100 Error Table by country and training and test
N100_r1<- cbind('GARCH Training','GARCH Test','Monte Carlo',
                'GARCH-MIDAS Training','GARCH-MIDAS Test')

N100_c1<-rbind('Model/data set','Mean Absolute Error','Mean Squared Error',
               'Root Mean Squared Error')

N100_r2<- cbind(N100_Garch_training_abs_error,N100_Garch_abs_error, N100_MC_absoulte_error,
                N100_GARCH_MIDAS_training_abs_error,N100_GARCH_MIDAS_abs_error)

N100_r3<-cbind(N100_Garch_training_squared_error,N100_Garch_squared_error, N100_MC_squared_mean_error,
               N100_GARCH_MIDAS_training_mean_squared_error,N100_GARCH_MIDAS_mean_squared_error)

N100_r4<-cbind(N100_Garch_training_root_error,N100_Garch_root_error,N100_MC_root_mean_error,
               N100_GARCH_MIDAS_training_mean_root_error,N100_GARCH_MIDAS_mean_root_error)

N100_rtotal<-rbind(N100_r1,N100_r2,N100_r3,N100_r4)

N100_Table<-cbind(N100_c1,N100_rtotal)

colnames(N100_Table)<-c('Country Index',"N100","N100","N100","N100","N100")
View(N100_Table)

# Diebold-Mariano   do a loop for all errors
DM.test(N100_MC_DM,N100_GARCH_DM,N100_real_DM,loss.type = "SE",H1 ="same")
 