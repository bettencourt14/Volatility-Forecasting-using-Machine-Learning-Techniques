library(tidyverse)
library(multDM)
library(writexl)

# Run this all at once, should be no less than 7 Min, if less make sure SVR runned
# not sure why, but run multiple times until SVR be on environment
start1<-Sys.time()
list.files(c("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Error By Country/HSI"),
           full.names =T ) %>% map(source)
source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Error By Country/HSI/SVR-HSI.R")
end1<-Sys.time()
elapsed<-end1-start1
elapsed

# HSI Error Table by country and training and test
HSI_r1<- rbind('GARCH Training','GARCH Test','Monte Carlo',
                'GARCH-MIDAS Training','GARCH-MIDAS Test',
                'LSTM Training','LSTM Test',
                'SVR Training','SVR Test')

HSI_c1<-cbind('Model/data set','Mean Absolute Error','Mean Squared Error',
               'Root Mean Squared Error')

HSI_r2<- rbind(HSI_Garch_training_abs_error,HSI_Garch_abs_error,
                HSI_MC_absoulte_error,
                HSI_GARCH_MIDAS_training_abs_error,HSI_GARCH_MIDAS_abs_error,
                HSI_LSTM_training_mean_abs_error,HSI_LSTM_abs_error,
                HSI_SVR_training_abs_error, HSI_SVR_test_abs_error)

HSI_r3<-rbind(HSI_Garch_training_squared_error,HSI_Garch_squared_error,
               HSI_MC_squared_mean_error,
               HSI_GARCH_MIDAS_training_mean_squared_error,HSI_GARCH_MIDAS_mean_squared_error,
               HSI_LSTM_training_squared_error,HSI_LSTM_squared_error,
               HSI_SVR_training_mean_squared_error,HSI_SVR_test_mean_squared_error)

HSI_r4<-rbind(HSI_Garch_training_root_error,HSI_Garch_root_error,
               HSI_MC_root_mean_error,
               HSI_GARCH_MIDAS_training_mean_root_error,HSI_GARCH_MIDAS_mean_root_error,
               HSI_LSTM_training_root_error,HSI_LSTM_root_error,
               HSI_SVR_training_mean_root_error,HSI_SVR_test_mean_root_error)

HSI_rtotal<-cbind(HSI_r1,HSI_r2,HSI_r3,HSI_r4)

HSI_Table<-rbind(HSI_c1,HSI_rtotal)


View(HSI_Table)
HSI_Table<-as.data.frame(HSI_Table)
setwd('E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Error By Country/HSI')


# Run below to get data, do it once only

#  write_xlsx(HSI_Table,"E://Thesis Francisco Bettencourt//Tese Nov//4. Data//R models//Error By Country//HSI//HSI_error.xlsx",col_names = T,format_headers = T,use_zip64 = FALSE)

########

HSI_training_returns<-as.numeric(HSI1_return$HSI1_return,na.omit=T) 
HSI_GARCH_training<-c(rep(0,length(HSI_training_returns)-length(HSI1_Garch_SD$`HSI1_m@fit$sigma`)),
                       as.numeric(HSI1_Garch_SD$`HSI1_m@fit$sigma`,na.omit=T))
HSI_GARCH_MIDAS_training<-c(rep(0,length(HSI_training_returns)-length(HSI_GARCH_MIDAS_SD$c))
                             ,as.numeric(HSI_GARCH_MIDAS_SD$c/100,na.omit=T))  # This one must be divided by 100
HSI_SVR_training<- c(rep(0,length(HSI_training_returns)-length(HSI_SVR_predicted_best_model$HSI_SVR_predicted_best_model)),
                      as.numeric(HSI_SVR_predicted_best_model$HSI_SVR_predicted_best_model,
                               na.omit=T))
HSI_LSTM_training<-as.numeric(res$pred_train,na.omit=T)
HSI_LSTM_training<-HSI_LSTM_training[1:nrow(HSI1)]
HSI_LSTM_training[is.na(HSI_LSTM_training)]<-0

HSI_training_DM<-tibble(actual=HSI_training_returns,
                         GARCH=HSI_GARCH_training,
                         MIDAS=HSI_GARCH_MIDAS_training,
                         SVR=HSI_SVR_training,
                         LSTM=HSI_LSTM_training)

HSI_training_DM[is.na(HSI_training_DM)]<-0


nop1<-DM.test(HSI_training_DM$GARCH,HSI_training_DM$MIDAS,HSI_training_DM$actual)
nop2<-DM.test(HSI_training_DM$GARCH,HSI_training_DM$SVR,HSI_training_DM$actual)
nop3<-DM.test(HSI_training_DM$GARCH,HSI_training_DM$LSTM,HSI_training_DM$actual)
nop4<-DM.test(HSI_training_DM$MIDAS,HSI_training_DM$SVR,HSI_training_DM$actual)
nop5<-DM.test(HSI_training_DM$MIDAS,HSI_training_DM$LSTM,HSI_training_DM$actual)
nop6<-DM.test(HSI_training_DM$SVR,HSI_training_DM$LSTM,HSI_training_DM$actual)



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

# write_xlsx(nop_table,"E://Thesis Francisco Bettencourt//Tese Nov//4. Data//R models//Error By Country//HSI//HSI_DM_training.xlsx" ,col_names = T,format_headers = T,use_zip64 = FALSE)

#################################3

# Test set
HSI_test_returns<-as.numeric(HSI2_returns$`diff(log(HSI2$Close))`,na.omit=T) 
HSI_GARCH_test<-as.numeric(HSI2_forecast_sigma[,1])
HSI_GARCH_MIDAS_test<-as.numeric(HSI_GARCH_MIDAS_forecast_SD$e)
HSI_SVR_test<-as.numeric(HSI_SVR_forecast$HSI_SVR_forecast)
HSI_LSTM_test<-as.numeric(res$pred_test,na.omit=T)
HSI_LSTM_test<-HSI_LSTM_test[length(HSI1$Date):length(HSI_LSTM_test)]
HSI_LSTM_test<-HSI_LSTM_test[2:length(HSI_LSTM_test)]
HSI_MC_test<-as.numeric(HSI_error_means)


HSI_test_DM<-tibble(actual=HSI_test_returns,
                     GARCH=HSI_GARCH_test,
                     M.Carlo=HSI_MC_test,
                     MIDAS=HSI_GARCH_MIDAS_test,
                     SVR=HSI_SVR_test,
                     LSTM=HSI_LSTM_test)

HSI_test_DM[is.na(HSI_test_DM)]<-0

test1<-DM.test(HSI_test_DM$GARCH,HSI_test_DM$M.Carlo,HSI_test_DM$actual,c = T)
test2<-DM.test(HSI_test_DM$GARCH,HSI_test_DM$MIDAS,HSI_test_DM$actual,c=T)
test3<-DM.test(HSI_test_DM$GARCH,HSI_test_DM$SVR,HSI_test_DM$actual,c=T)
test4<-DM.test(HSI_test_DM$GARCH,HSI_test_DM$LSTM,HSI_test_DM$actual,c=T)
test5<-DM.test(HSI_test_DM$M.Carlo,HSI_test_DM$MIDAS,HSI_test_DM$actual,c=T)
test6<-DM.test(HSI_test_DM$M.Carlo,HSI_test_DM$SVR,HSI_test_DM$actual,c=T)
test7<-DM.test(HSI_test_DM$M.Carlo,HSI_test_DM$LSTM,HSI_test_DM$actual,c=T)
test8<-DM.test(HSI_test_DM$MIDAS,HSI_test_DM$SVR,HSI_test_DM$actual,c=T)
test9<-DM.test(HSI_test_DM$MIDAS,HSI_test_DM$LSTM,HSI_test_DM$actual,c=T)
test10<-DM.test(HSI_test_DM$SVR,HSI_test_DM$LSTM,HSI_test_DM$actual,c=T)

col1<-cbind(0,test1$p.value,test2$p.value,test3$p.value,test4$p.value)
col2<-cbind(0,0,test5$p.value,test6$p.value,test7$p.value)
col3<-cbind(0,0,0,test8$p.value,test9$p.value)
col4<-cbind(0,0,0,0,test10$p.value)
col5<-cbind(0,0,0,0,0)

test_table<-rbind(col1,col2,col3,col4,col5)
test_table<-as.data.frame(test_table)
colnames(test_table)<-c('GARCH','Monte Carlo','GARCH-MIDAS','SVR','LSTM')

# write_xlsx(test_table,"E://Thesis Francisco Bettencourt//Tese Nov//4. Data//R models//Error By Country//HSI//HSI_DM_test.xlsx",col_names = T,format_headers = T,use_zip64 = FALSE)

