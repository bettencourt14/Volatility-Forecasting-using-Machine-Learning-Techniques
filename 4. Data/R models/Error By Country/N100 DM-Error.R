library(tidyverse)
library(multDM)
library(writexl)

# Run this all at once, should be no less than 4 Min, if less make sure SVR runned
# not sure why, but run multiple times until SVR be on environment
start1<-Sys.time()
list.files(c('E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Error By Country/N100'),
           full.names =T ) %>% map(source)
source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Error By Country/N100/SVR-N100.R")
end1<-Sys.time()
elapsed<-end1-start1
elapsed

# N100 Error Table by country and training and test
N100_r1<- rbind('GARCH Training','GARCH Test','Monte Carlo',
                'GARCH-MIDAS Training','GARCH-MIDAS Test',
                'LSTM Training','LSTM Test',
                'SVR Training','SVR Test')

N100_c1<-cbind('Model/data set','Mean Absolute Error','Mean Squared Error',
               'Root Mean Squared Error')

N100_r2<- rbind(N100_Garch_training_abs_error,N100_Garch_abs_error,
                N100_MC_absoulte_error,
                N100_GARCH_MIDAS_training_abs_error,N100_GARCH_MIDAS_abs_error,
                N100_LSTM_training_mean_abs_error,N100_LSTM_abs_error,
                N100_SVR_training_abs_error, N100_SVR_test_abs_error)

N100_r3<-rbind(N100_Garch_training_squared_error,N100_Garch_squared_error,
               N100_MC_squared_mean_error,
               N100_GARCH_MIDAS_training_mean_squared_error,N100_GARCH_MIDAS_mean_squared_error,
               N100_LSTM_training_squared_error,N100_LSTM_squared_error,
               N100_SVR_training_mean_squared_error,N100_SVR_test_mean_squared_error)

N100_r4<-rbind(N100_Garch_training_root_error,N100_Garch_root_error,
               N100_MC_root_mean_error,
               N100_GARCH_MIDAS_training_mean_root_error,N100_GARCH_MIDAS_mean_root_error,
               N100_LSTM_training_root_error,N100_LSTM_root_error,
               N100_SVR_training_mean_root_error,N100_SVR_test_mean_root_error)

N100_rtotal<-cbind(N100_r1,N100_r2,N100_r3,N100_r4)

N100_Table<-rbind(N100_c1,N100_rtotal)


View(N100_Table)
N100_Table<-as.data.frame(N100_Table)
setwd('E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Error By Country/N100')


# Run below to get data, do it once only

# write_xlsx(N100_Table,"E:\\Thesis Francisco Bettencourt\\Tese Nov\\4. Data\\R models\\Error By Country\\N100\\N100_error.xlsx",col_names = T,format_headers = T,use_zip64 = FALSE)

N100_training_returns<-as.numeric(N1001_return$N1001_return,na.omit=T) 
N100_GARCH_training<-c(rep(0,length(N100_training_returns)-length(N1001_Garch_SD$`N1001_m@fit$sigma`)),
                       as.numeric(N1001_Garch_SD$`N1001_m@fit$sigma`,na.omit=T))
N100_GARCH_MIDAS_training<-c(rep(0,length(N100_training_returns)-length(N100_GARCH_MIDAS_SD$c))
                             ,as.numeric(N100_GARCH_MIDAS_SD$c/100,na.omit=T))  # This one must be divided by 100
N100_SVR_training<- c(rep(0,length(N100_training_returns)-length(N100_SVR_predicted_best_model$N100_SVR_predicted_best_model)),
                      as.numeric(N100_SVR_predicted_best_model$N100_SVR_predicted_best_model,
                               na.omit=T))
N100_LSTM_training<-as.numeric(res$pred_train,na.omit=T)
N100_LSTM_training<-N100_LSTM_training[1:nrow(N1001)]
N100_LSTM_training[is.na(N100_LSTM_training)]<-0

N100_training_DM<-tibble(actual=N100_training_returns,
                         GARCH=N100_GARCH_training,
                         MIDAS=N100_GARCH_MIDAS_training,
                         SVR=N100_SVR_training,
                         LSTM=N100_LSTM_training)

N100_training_DM[is.na(N100_training_DM)]<-0


nop1<-DM.test(N100_training_DM$GARCH,N100_training_DM$MIDAS,N100_training_DM$actual,c=T)
nop2<-DM.test(N100_training_DM$GARCH,N100_training_DM$SVR,N100_training_DM$actual,c=T)
nop3<-DM.test(N100_training_DM$GARCH,N100_training_DM$LSTM,N100_training_DM$actual,c=T)
nop4<-DM.test(N100_training_DM$MIDAS,N100_training_DM$SVR,N100_training_DM$actual,c=T)
nop5<-DM.test(N100_training_DM$MIDAS,N100_training_DM$LSTM,N100_training_DM$actual,c=T)
nop6<-DM.test(N100_training_DM$SVR,N100_training_DM$LSTM,N100_training_DM$actual,c=T)



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

# write_xlsx(nop_table,"E:\\Thesis Francisco Bettencourt\\Tese Nov\\4. Data\\R models\\Error By Country\\N100\\N100_DM_training.xlsx" ,col_names = T,format_headers = T,use_zip64 = FALSE)

#################################3

# Test set
N100_test_returns<-as.numeric(N1002_returns$`diff(log(N1002$Close))`,na.omit=T) 
N100_GARCH_test<-as.numeric(N1002_forecast_sigma$`1975-01-01`)
N100_GARCH_MIDAS_test<-as.numeric(N100_GARCH_MIDAS_forecast_SD$e)
N100_SVR_test<-as.numeric(N100_SVR_forecast)
N100_LSTM_test<-as.numeric(res$pred_test,na.omit=T)
N100_LSTM_test<-N100_LSTM_test[length(N1001$Date):length(N100_LSTM_test)]
N100_LSTM_test<-N100_LSTM_test[2:length(N100_LSTM_test)]
N100_MC_test<-as.numeric(N100_error_means)


N100_test_DM<-tibble(actual=N100_test_returns,
                     GARCH=N100_GARCH_test,
                     M.Carlo=N100_MC_test,
                     MIDAS=N100_GARCH_MIDAS_test,
                     SVR=N100_SVR_test,
                     LSTM=N100_LSTM_test)

N100_test_DM[is.na(N100_test_DM)]<-0

test1<-DM.test(N100_test_DM$GARCH,N100_test_DM$M.Carlo,N100_test_DM$actual,c = T)
test2<-DM.test(N100_test_DM$GARCH,N100_test_DM$MIDAS,N100_test_DM$actual,c=T)
test3<-DM.test(N100_test_DM$GARCH,N100_test_DM$SVR,N100_test_DM$actual,c=T)
test4<-DM.test(N100_test_DM$GARCH,N100_test_DM$LSTM,N100_test_DM$actual,c=T)
test5<-DM.test(N100_test_DM$M.Carlo,N100_test_DM$MIDAS,N100_test_DM$actual,c=T)
test6<-DM.test(N100_test_DM$M.Carlo,N100_test_DM$SVR,N100_test_DM$actual,c=T)
test7<-DM.test(N100_test_DM$M.Carlo,N100_test_DM$LSTM,N100_test_DM$actual,c=T)
test8<-DM.test(N100_test_DM$MIDAS,N100_test_DM$SVR,N100_test_DM$actual,c=T)
test9<-DM.test(N100_test_DM$MIDAS,N100_test_DM$LSTM,N100_test_DM$actual,c=T)
test10<-DM.test(N100_test_DM$SVR,N100_test_DM$LSTM,N100_test_DM$actual,c=T)

col1<-cbind(0,test1$p.value,test2$p.value,test3$p.value,test4$p.value)
col2<-cbind(0,0,test5$p.value,test6$p.value,test7$p.value)
col3<-cbind(0,0,0,test8$p.value,test9$p.value)
col4<-cbind(0,0,0,0,test10$p.value)
col5<-cbind(0,0,0,0,0)

test_table<-rbind(col1,col2,col3,col4,col5)
test_table<-as.data.frame(test_table)
colnames(test_table)<-c('GARCH','Monte Carlo','GARCH-MIDAS','SVR','LSTM')

# write_xlsx(test_table,"E:\\Thesis Francisco Bettencourt\\Tese Nov\\4. Data\\R models\\Error By Country\\N100\\N100_DM_test.xlsx",col_names = T,format_headers = T,use_zip64 = FALSE)
