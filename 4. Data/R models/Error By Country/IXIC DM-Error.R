library(tidyverse)
library(multDM)
library(writexl)

# Run this all at once, should be no less than 7 Min, if less make sure SVR runned
# not sure why, but run multiple times until SVR be on environment
start1<-Sys.time()
list.files(c('E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Error By Country/IXIC'),
           full.names =T ) %>% map(source)
source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Error By Country/IXIC/SVR-IXIC.R")
end1<-Sys.time()
elapsed<-end1-start1
elapsed

# IXIC Error Table by country and training and test
IXIC_r1<- rbind('GARCH Training','GARCH Test','Monte Carlo',
                'GARCH-MIDAS Training','GARCH-MIDAS Test',
                'LSTM Training','LSTM Test',
                'SVR Training','SVR Test')

IXIC_c1<-cbind('Model/data set','Mean Absolute Error','Mean Squared Error',
               'Root Mean Squared Error')

IXIC_r2<- rbind(IXIC_Garch_training_abs_error,IXIC_Garch_abs_error,
                IXIC_MC_absoulte_error,
                IXIC_GARCH_MIDAS_training_abs_error,IXIC_GARCH_MIDAS_abs_error,
                IXIC_LSTM_training_mean_abs_error,IXIC_LSTM_abs_error,
                IXIC_SVR_training_abs_error, IXIC_SVR_test_abs_error)

IXIC_r3<-rbind(IXIC_Garch_training_squared_error,IXIC_Garch_squared_error,
               IXIC_MC_squared_mean_error,
               IXIC_GARCH_MIDAS_training_mean_squared_error,IXIC_GARCH_MIDAS_mean_squared_error,
               IXIC_LSTM_training_squared_error,IXIC_LSTM_squared_error,
               IXIC_SVR_training_mean_squared_error,IXIC_SVR_test_mean_squared_error)

IXIC_r4<-rbind(IXIC_Garch_training_root_error,IXIC_Garch_root_error,
               IXIC_MC_root_mean_error,
               IXIC_GARCH_MIDAS_training_mean_root_error,IXIC_GARCH_MIDAS_mean_root_error,
               IXIC_LSTM_training_root_error,IXIC_LSTM_root_error,
               IXIC_SVR_training_mean_root_error,IXIC_SVR_test_mean_root_error)

IXIC_rtotal<-cbind(IXIC_r1,IXIC_r2,IXIC_r3,IXIC_r4)

IXIC_Table<-rbind(IXIC_c1,IXIC_rtotal)


View(IXIC_Table)
IXIC_Table<-as.data.frame(IXIC_Table)
setwd('E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Error By Country/IXIC')


# Run below to get data, do it once only

# write_xlsx(IXIC_Table,"E:\\Thesis Francisco Bettencourt\\Tese Nov\\4. Data\\R models\\Error By Country\\IXIC\\IXIC_error.xlsx",col_names = T,format_headers = T,use_zip64 = FALSE)

IXIC_training_returns<-as.numeric(IXIC1_return$IXIC1_return,na.omit=T) 
IXIC_GARCH_training<-c(rep(0,length(IXIC_training_returns)-length(IXIC1_Garch_SD$`IXIC1_m@fit$sigma`)),
                       as.numeric(IXIC1_Garch_SD$`IXIC1_m@fit$sigma`,na.omit=T))
IXIC_GARCH_MIDAS_training<-c(rep(0,length(IXIC_training_returns)-length(IXIC_GARCH_MIDAS_SD$c))
                             ,as.numeric(IXIC_GARCH_MIDAS_SD$c/100,na.omit=T))  # This one must be divided by 100
IXIC_SVR_training<- c(rep(0,length(IXIC_training_returns)-length(IXIC_SVR_predicted_best_model$IXIC_SVR_predicted_best_model)),
                      as.numeric(IXIC_SVR_predicted_best_model$IXIC_SVR_predicted_best_model,
                               na.omit=T))
IXIC_LSTM_training<-as.numeric(res$pred_train,na.omit=T)
IXIC_LSTM_training<-IXIC_LSTM_training[1:nrow(IXIC1)]
IXIC_LSTM_training[is.na(IXIC_LSTM_training)]<-0

IXIC_training_DM<-tibble(actual=IXIC_training_returns,
                         GARCH=IXIC_GARCH_training,
                         MIDAS=IXIC_GARCH_MIDAS_training,
                         SVR=IXIC_SVR_training,
                         LSTM=IXIC_LSTM_training)

IXIC_training_DM[is.na(IXIC_training_DM)]<-0


nop1<-DM.test(IXIC_training_DM$GARCH,IXIC_training_DM$MIDAS,IXIC_training_DM$actual,c=T)
nop2<-DM.test(IXIC_training_DM$GARCH,IXIC_training_DM$SVR,IXIC_training_DM$actual,c=T)
nop3<-DM.test(IXIC_training_DM$GARCH,IXIC_training_DM$LSTM,IXIC_training_DM$actual,c=T)
nop4<-DM.test(IXIC_training_DM$MIDAS,IXIC_training_DM$SVR,IXIC_training_DM$actual,c=T)
nop5<-DM.test(IXIC_training_DM$MIDAS,IXIC_training_DM$LSTM,IXIC_training_DM$actual,c=T)
nop6<-DM.test(IXIC_training_DM$SVR,IXIC_training_DM$LSTM,IXIC_training_DM$actual,c=T)



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

# write_xlsx(nop_table,"E:\\Thesis Francisco Bettencourt\\Tese Nov\\4. Data\\R models\\Error By Country\\IXIC\\IXIC_DM_training.xlsx" ,col_names = T,format_headers = T,use_zip64 = FALSE)

#################################3

# Test set
IXIC_test_returns<-as.numeric(IXIC2_returns$`diff(log(IXIC2$Close))`,na.omit=T) 
IXIC_GARCH_test<-as.numeric(IXIC2_forecast_sigma$`1974-12-02`)
IXIC_GARCH_MIDAS_test<-as.numeric(IXIC_GARCH_MIDAS_forecast_SD$e)
IXIC_SVR_test<-as.numeric(IXIC_SVR_forecast$IXIC_SVR_forecast)
IXIC_LSTM_test<-as.numeric(res$pred_test,na.omit=T)
IXIC_LSTM_test<-IXIC_LSTM_test[length(IXIC1$Date):length(IXIC_LSTM_test)]
IXIC_LSTM_test<-IXIC_LSTM_test[2:length(IXIC_LSTM_test)]
IXIC_MC_test<-as.numeric(IXIC_error_means)


IXIC_test_DM<-tibble(actual=IXIC_test_returns,
                     GARCH=IXIC_GARCH_test,
                     M.Carlo=IXIC_MC_test,
                     MIDAS=IXIC_GARCH_MIDAS_test,
                     SVR=IXIC_SVR_test,
                     LSTM=IXIC_LSTM_test)

IXIC_test_DM[is.na(IXIC_test_DM)]<-0

test1<-DM.test(IXIC_test_DM$GARCH,IXIC_test_DM$M.Carlo,IXIC_test_DM$actual,c = T)
test2<-DM.test(IXIC_test_DM$GARCH,IXIC_test_DM$MIDAS,IXIC_test_DM$actual,c=T)
test3<-DM.test(IXIC_test_DM$GARCH,IXIC_test_DM$SVR,IXIC_test_DM$actual,c=T)
test4<-DM.test(IXIC_test_DM$GARCH,IXIC_test_DM$LSTM,IXIC_test_DM$actual,c=T)
test5<-DM.test(IXIC_test_DM$M.Carlo,IXIC_test_DM$MIDAS,IXIC_test_DM$actual,c=T)
test6<-DM.test(IXIC_test_DM$M.Carlo,IXIC_test_DM$SVR,IXIC_test_DM$actual,c=T)
test7<-DM.test(IXIC_test_DM$M.Carlo,IXIC_test_DM$LSTM,IXIC_test_DM$actual,c=T)
test8<-DM.test(IXIC_test_DM$MIDAS,IXIC_test_DM$SVR,IXIC_test_DM$actual,c=T)
test9<-DM.test(IXIC_test_DM$MIDAS,IXIC_test_DM$LSTM,IXIC_test_DM$actual,c=T)
test10<-DM.test(IXIC_test_DM$SVR,IXIC_test_DM$LSTM,IXIC_test_DM$actual,c=T)

col1<-cbind(0,test1$p.value,test2$p.value,test3$p.value,test4$p.value)
col2<-cbind(0,0,test5$p.value,test6$p.value,test7$p.value)
col3<-cbind(0,0,0,test8$p.value,test9$p.value)
col4<-cbind(0,0,0,0,test10$p.value)
col5<-cbind(0,0,0,0,0)

test_table<-rbind(col1,col2,col3,col4,col5)
test_table<-as.data.frame(test_table)
colnames(test_table)<-c('GARCH','Monte Carlo','GARCH-MIDAS','SVR','LSTM')

# write_xlsx(test_table,"E:\\Thesis Francisco Bettencourt\\Tese Nov\\4. Data\\R models\\Error By Country\\IXIC\\IXIC_DM_test.xlsx",col_names = T,format_headers = T,use_zip64 = FALSE)
