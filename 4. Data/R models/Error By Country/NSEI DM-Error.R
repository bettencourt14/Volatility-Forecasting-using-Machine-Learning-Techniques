library(tidyverse)
library(multDM)
library(writexl)

# Run this all at once, should be no less than 10 Min, if less make sure SVR runned
# not sure why, but run multiple times until SVR be on environment
start1<-Sys.time()
list.files(c('E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Error By Country/NSEI'),
           full.names =T ) %>% map(source)
source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Error By Country/NSEI/SVR-NSEI.R")
end1<-Sys.time()
elapsed<-end1-start1
elapsed

# NSEI Error Table by country and training and test
NSEI_r1<- rbind('GARCH Training','GARCH Test','Monte Carlo',
                'GARCH-MIDAS Training','GARCH-MIDAS Test',
                'LSTM Training','LSTM Test',
                'SVR Training','SVR Test')

NSEI_c1<-cbind('Model/data set','Mean Absolute Error','Mean Squared Error',
               'Root Mean Squared Error')

NSEI_r2<- rbind(NSEI_Garch_training_abs_error,NSEI_Garch_abs_error,
                NSEI_MC_absoulte_error,
                NSEI_GARCH_MIDAS_training_abs_error,NSEI_GARCH_MIDAS_abs_error,
                NSEI_LSTM_training_mean_abs_error,NSEI_LSTM_abs_error,
                NSEI_SVR_training_abs_error, NSEI_SVR_test_abs_error)

NSEI_r3<-rbind(NSEI_Garch_training_squared_error,NSEI_Garch_squared_error,
               NSEI_MC_squared_mean_error,
               NSEI_GARCH_MIDAS_training_mean_squared_error,NSEI_GARCH_MIDAS_mean_squared_error,
               NSEI_LSTM_training_squared_error,NSEI_LSTM_squared_error,
               NSEI_SVR_training_mean_squared_error,NSEI_SVR_test_mean_squared_error)

NSEI_r4<-rbind(NSEI_Garch_training_root_error,NSEI_Garch_root_error,
               NSEI_MC_root_mean_error,
               NSEI_GARCH_MIDAS_training_mean_root_error,NSEI_GARCH_MIDAS_mean_root_error,
               NSEI_LSTM_training_root_error,NSEI_LSTM_root_error,
               NSEI_SVR_training_mean_root_error,NSEI_SVR_test_mean_root_error)

NSEI_rtotal<-cbind(NSEI_r1,NSEI_r2,NSEI_r3,NSEI_r4)

NSEI_Table<-rbind(NSEI_c1,NSEI_rtotal)


View(NSEI_Table)
NSEI_Table<-as.data.frame(NSEI_Table)
setwd('E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Error By Country/NSEI')


# Run below to get data, do it once only

#  write_xlsx(NSEI_Table,"E:\\Thesis Francisco Bettencourt\\Tese Nov\\4. Data\\R models\\Error By Country\\NSEI\\NSEI_error.xlsx",col_names = T,format_headers = T,use_zip64 = FALSE)

########

NSEI_training_returns<-as.numeric(NSEI1_return$NSEI1_return,na.omit=T) 
NSEI_GARCH_training<-c(rep(0,length(NSEI_training_returns)-length(NSEI1_Garch_SD$`NSEI1_m@fit$sigma`)),
                       as.numeric(NSEI1_Garch_SD$`NSEI1_m@fit$sigma`,na.omit=T))
NSEI_GARCH_MIDAS_training<-c(rep(0,length(NSEI_training_returns)-length(NSEI_GARCH_MIDAS_SD$c))
                             ,as.numeric(NSEI_GARCH_MIDAS_SD$c/100,na.omit=T))  # This one must be divided by 100
NSEI_SVR_training<- c(rep(0,length(NSEI_training_returns)-length(NSEI_SVR_predicted_best_model$NSEI_SVR_predicted_best_model)),
                      as.numeric(NSEI_SVR_predicted_best_model$NSEI_SVR_predicted_best_model,
                               na.omit=T))
NSEI_LSTM_training<-as.numeric(res$pred_train,na.omit=T)
NSEI_LSTM_training<-NSEI_LSTM_training[1:nrow(NSEI1)]
NSEI_LSTM_training[is.na(NSEI_LSTM_training)]<-0

NSEI_training_DM<-tibble(actual=NSEI_training_returns,
                         GARCH=NSEI_GARCH_training,
                         MIDAS=NSEI_GARCH_MIDAS_training,
                         SVR=NSEI_SVR_training,
                         LSTM=NSEI_LSTM_training)

NSEI_training_DM[is.na(NSEI_training_DM)]<-0


nop1<-DM.test(NSEI_training_DM$GARCH,NSEI_training_DM$MIDAS,NSEI_training_DM$actual,c=T)
nop2<-DM.test(NSEI_training_DM$GARCH,NSEI_training_DM$SVR,NSEI_training_DM$actual,c=T)
nop3<-DM.test(NSEI_training_DM$GARCH,NSEI_training_DM$LSTM,NSEI_training_DM$actual,c=T)
nop4<-DM.test(NSEI_training_DM$MIDAS,NSEI_training_DM$SVR,NSEI_training_DM$actual,c=T)
nop5<-DM.test(NSEI_training_DM$MIDAS,NSEI_training_DM$LSTM,NSEI_training_DM$actual,c=T)
nop6<-DM.test(NSEI_training_DM$SVR,NSEI_training_DM$LSTM,NSEI_training_DM$actual,c=T)



# Die bold-Mariano

# training Error by model



row1<-cbind(0,nop1$p.value,nop2$p.value,nop3$p.value)
row2<-cbind(0,0,nop4$p.value,nop5$p.value)
row3<-cbind(0,0,0,nop6$p.value)
row4<-cbind(0,0,0,0)

nop_table<-rbind(row1,row2,row3,row4)
nop_table<-as.data.frame(nop_table)
rownames(nop_table)<-c('GARCH','GARCH-MIDAS','SVR','LSTM')
colnames(nop_table)<-c('GARCH','GARCH-MIDAS','SVR','LSTM')


#Run once, same as above

 # write_xlsx(nop_table,"E:\\Thesis Francisco Bettencourt\\Tese Nov\\4. Data\\R models\\Error By Country\\NSEI\\NSEI_DM_training.xlsx" ,col_names = T,format_headers = T,use_zip64 = FALSE)

#################################3

# Test set
NSEI_test_returns<-as.numeric(NSEI2_returns$`diff(log(NSEI2$Close))`,na.omit=T) 
NSEI_GARCH_test<-as.numeric(NSEI2_forecast_sigma[,1])
NSEI_GARCH_MIDAS_test<-as.numeric(NSEI_GARCH_MIDAS_forecast_SD$e)
NSEI_SVR_test<-as.numeric(NSEI_SVR_forecast$NSEI_SVR_forecast)
NSEI_LSTM_test<-as.numeric(res$pred_test,na.omit=T)
NSEI_LSTM_test<-NSEI_LSTM_test[length(NSEI1$Date):length(NSEI_LSTM_test)]
NSEI_LSTM_test<-NSEI_LSTM_test[2:length(NSEI_LSTM_test)]
NSEI_MC_test<-as.numeric(NSEI_error_means)


NSEI_test_DM<-tibble(actual=NSEI_test_returns,
                     GARCH=NSEI_GARCH_test,
                     M.Carlo=NSEI_MC_test,
                     MIDAS=NSEI_GARCH_MIDAS_test,
                     SVR=NSEI_SVR_test,
                     LSTM=NSEI_LSTM_test)

NSEI_test_DM[is.na(NSEI_test_DM)]<-0

test1<-DM.test(NSEI_test_DM$GARCH,NSEI_test_DM$M.Carlo,NSEI_test_DM$actual,c = T)
test2<-DM.test(NSEI_test_DM$GARCH,NSEI_test_DM$MIDAS,NSEI_test_DM$actual,c=T)
test3<-DM.test(NSEI_test_DM$GARCH,NSEI_test_DM$SVR,NSEI_test_DM$actual,c=T)
test4<-DM.test(NSEI_test_DM$GARCH,NSEI_test_DM$LSTM,NSEI_test_DM$actual,c=T)
test5<-DM.test(NSEI_test_DM$M.Carlo,NSEI_test_DM$MIDAS,NSEI_test_DM$actual,c=T)
test6<-DM.test(NSEI_test_DM$M.Carlo,NSEI_test_DM$SVR,NSEI_test_DM$actual,c=T)
test7<-DM.test(NSEI_test_DM$M.Carlo,NSEI_test_DM$LSTM,NSEI_test_DM$actual,c=T)
test8<-DM.test(NSEI_test_DM$MIDAS,NSEI_test_DM$SVR,NSEI_test_DM$actual,c=T)
test9<-DM.test(NSEI_test_DM$MIDAS,NSEI_test_DM$LSTM,NSEI_test_DM$actual,c=T)
test10<-DM.test(NSEI_test_DM$SVR,NSEI_test_DM$LSTM,NSEI_test_DM$actual,c=T)

col1<-cbind(0,test1$p.value,test2$p.value,test3$p.value,test4$p.value)
col2<-cbind(0,0,test5$p.value,test6$p.value,test7$p.value)
col3<-cbind(0,0,0,test8$p.value,test9$p.value)
col4<-cbind(0,0,0,0,test10$p.value)
col5<-cbind(0,0,0,0,0)

test_table<-rbind(col1,col2,col3,col4,col5)
test_table<-as.data.frame(test_table)
colnames(test_table)<-c('GARCH','Monte Carlo','GARCH-MIDAS','SVR','LSTM')

# write_xlsx(test_table,"E:\\Thesis Francisco Bettencourt\\Tese Nov\\4. Data\\R models\\Error By Country\\NSEI\\NSEI_DM_test.xlsx",col_names = T,format_headers = T,use_zip64 = FALSE)
