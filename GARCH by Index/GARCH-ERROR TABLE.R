### GARCH ERROR TABLES #####

options(scipen = 999)
rm(list=ls(all=TRUE))

library(tidyverse)
source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/GARCH by Index/GARCH-ALL.R")

# Error Table by country and training and test
r1<- cbind('GARCH Training','GARCH Test',
           'GARCH Training','GARCH Test',
           'GARCH Training','GARCH Test',
           'GARCH Training','GARCH Test')

c1<-rbind('Model/data set','Mean Absolute Error','Mean Squared Error',
          'Root Mean Squared Error')

r2<- cbind(N100_Garch_training_abs_error,N100_Garch_abs_error,
           IXIC_Garch_training_abs_error,IXIC_Garch_abs_error,
           NSEI_Garch_training_abs_error,NSEI_Garch_abs_error,
           BVSP_Garch_training_abs_error,BVSP_Garch_abs_error)

r3<-cbind(N100_Garch_training_squared_error,N100_Garch_squared_error,
          IXIC_Garch_training_squared_error,IXIC_Garch_squared_error,
          NSEI_Garch_training_squared_error,NSEI_Garch_squared_error,
          BVSP_Garch_training_squared_error,BVSP_Garch_squared_error)

r4<-cbind(N100_Garch_training_root_error,N100_Garch_root_error,
          IXIC_Garch_training_root_error,IXIC_Garch_root_error,
          NSEI_Garch_training_root_error,NSEI_Garch_root_error,
          BVSP_Garch_training_root_error,BVSP_Garch_root_error) 

rtotal<-rbind(r1,r2,r3,r4)
t1<-cbind(c1,rtotal)

colnames(t1)<-c('Country Index',
                'N100','N100',
                'IXIC','IXIC',
                'NSEI','NSEI',
                'BVSP','BVSP')  
View(t1)
