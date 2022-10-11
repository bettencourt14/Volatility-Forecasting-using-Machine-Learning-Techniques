library(tidyverse)
library(multDM)
library(writexl)

# Run this all at once, should be no less than 7 Min, if less make sure SVR runned
# not sure why, but run multiple times until SVR be on environment
start1<-Sys.time()
list.files(c('E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Error By Country/BVSP'),
           full.names =T ) %>% map(source)
source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Error By Country/BVSP/SVR-BVSP.R")
end1<-Sys.time()
elapsed<-end1-start1
elapsed

# BVSP Error Table by country and training and test
BVSP_r1<- rbind('GARCH Training','GARCH Test','Monte Carlo',
                'GARCH-MIDAS Training','GARCH-MIDAS Test',
                'LSTM Training','LSTM Test',
                'SVR Training','SVR Test')

BVSP_c1<-cbind('Model/data set','Mean Absolute Error','Mean Squared Error',
               'Root Mean Squared Error')

BVSP_r2<- rbind(BVSP_Garch_training_abs_error,BVSP_Garch_abs_error,
                BVSP_MC_absoulte_error,
                BVSP_GARCH_MIDAS_training_abs_error,BVSP_GARCH_MIDAS_abs_error,
                BVSP_LSTM_training_mean_abs_error,BVSP_LSTM_abs_error,
                BVSP_SVR_training_abs_error, BVSP_SVR_test_abs_error)

BVSP_r3<-rbind(BVSP_Garch_training_squared_error,BVSP_Garch_squared_error,
               BVSP_MC_squared_mean_error,
               BVSP_GARCH_MIDAS_training_mean_squared_error,BVSP_GARCH_MIDAS_mean_squared_error,
               BVSP_LSTM_training_squared_error,BVSP_LSTM_squared_error,
               BVSP_SVR_training_mean_squared_error,BVSP_SVR_test_mean_squared_error)

BVSP_r4<-rbind(BVSP_Garch_training_root_error,BVSP_Garch_root_error,
               BVSP_MC_root_mean_error,
               BVSP_GARCH_MIDAS_training_mean_root_error,BVSP_GARCH_MIDAS_mean_root_error,
               BVSP_LSTM_training_root_error,BVSP_LSTM_root_error,
               BVSP_SVR_training_mean_root_error,BVSP_SVR_test_mean_root_error)

BVSP_rtotal<-cbind(BVSP_r1,BVSP_r2,BVSP_r3,BVSP_r4)

BVSP_Table<-rbind(BVSP_c1,BVSP_rtotal)


View(BVSP_Table)
BVSP_Table<-as.data.frame(BVSP_Table)
setwd('E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Error By Country/BVSP')


# Run below to get data, do it once only

# write_xlsx(BVSP_Table,"E:\\Thesis Francisco Bettencourt\\Tese Nov\\4. Data\\R models\\Error By Country\\BVSP\\BVSP_error.xlsx",col_names = T,format_headers = T,use_zip64 = FALSE)

########

BVSP_training_returns<-as.numeric(BVSP1_return$BVSP1_return,na.omit=T) 
BVSP_GARCH_training<-c(rep(0,length(BVSP_training_returns)-length(BVSP1_Garch_SD$`BVSP1_m@fit$sigma`)),
                       as.numeric(BVSP1_Garch_SD$`BVSP1_m@fit$sigma`,na.omit=T))
BVSP_GARCH_MIDAS_training<-c(rep(0,length(BVSP_training_returns)-length(BVSP_GARCH_MIDAS_SD$c))
                             ,as.numeric(BVSP_GARCH_MIDAS_SD$c/100,na.omit=T))  # This one must be divided by 100
BVSP_SVR_training<- c(rep(0,length(BVSP_training_returns)-length(BVSP_SVR_predicted_best_model$BVSP_SVR_predicted_best_model)),
                      as.numeric(BVSP_SVR_predicted_best_model$BVSP_SVR_predicted_best_model,
                               na.omit=T))
BVSP_LSTM_training<-as.numeric(res$pred_train,na.omit=T)
BVSP_LSTM_training<-BVSP_LSTM_training[1:nrow(BVSP1)]
BVSP_LSTM_training[is.na(BVSP_LSTM_training)]<-0

BVSP_training_DM<-tibble(actual=BVSP_training_returns,
                         GARCH=BVSP_GARCH_training,
                         MIDAS=BVSP_GARCH_MIDAS_training,
                         SVR=BVSP_SVR_training,
                         LSTM=BVSP_LSTM_training)

BVSP_training_DM[is.na(BVSP_training_DM)]<-0


nop1<-DM.test(BVSP_training_DM$GARCH,BVSP_training_DM$MIDAS,BVSP_training_DM$actual,c=T)
nop2<-DM.test(BVSP_training_DM$GARCH,BVSP_training_DM$SVR,BVSP_training_DM$actual,c=T)
nop3<-DM.test(BVSP_training_DM$GARCH,BVSP_training_DM$LSTM,BVSP_training_DM$actual,c=T)
nop4<-DM.test(BVSP_training_DM$MIDAS,BVSP_training_DM$SVR,BVSP_training_DM$actual,c=T)
nop5<-DM.test(BVSP_training_DM$MIDAS,BVSP_training_DM$LSTM,BVSP_training_DM$actual,c=T)
nop6<-DM.test(BVSP_training_DM$SVR,BVSP_training_DM$LSTM,BVSP_training_DM$actual,c=T)



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

#  write_xlsx(nop_table,"E:\\Thesis Francisco Bettencourt\\Tese Nov\\4. Data\\R models\\Error By Country\\BVSP\\BVSP_DM_training.xlsx" ,col_names = T,format_headers = T,use_zip64 = FALSE)

#################################3

# Test set
BVSP_test_returns<-as.numeric(BVSP2_returns$`diff(log(BVSP2$Close))`,na.omit=T) 
BVSP_GARCH_test<-as.numeric(BVSP2_forecast_sigma[,1])
BVSP_GARCH_MIDAS_test<-as.numeric(BVSP_GARCH_MIDAS_forecast_SD$e)
BVSP_SVR_test<-as.numeric(BVSP_SVR_forecast$BVSP_SVR_forecast)
BVSP_LSTM_test<-as.numeric(res$pred_test,na.omit=T)
BVSP_LSTM_test<-BVSP_LSTM_test[length(BVSP1$Date):length(BVSP_LSTM_test)]
BVSP_LSTM_test<-BVSP_LSTM_test[2:length(BVSP_LSTM_test)]
BVSP_MC_test<-as.numeric(BVSP_error_means)


BVSP_test_DM<-tibble(actual=BVSP_test_returns,
                     GARCH=BVSP_GARCH_test,
                     M.Carlo=BVSP_MC_test,
                     MIDAS=BVSP_GARCH_MIDAS_test,
                     SVR=BVSP_SVR_test,
                     LSTM=BVSP_LSTM_test)

BVSP_test_DM[is.na(BVSP_test_DM)]<-0

test1<-DM.test(BVSP_test_DM$GARCH,BVSP_test_DM$M.Carlo,BVSP_test_DM$actual,c = T)
test2<-DM.test(BVSP_test_DM$GARCH,BVSP_test_DM$MIDAS,BVSP_test_DM$actual,c=T)
test3<-DM.test(BVSP_test_DM$GARCH,BVSP_test_DM$SVR,BVSP_test_DM$actual,c=T)
test4<-DM.test(BVSP_test_DM$GARCH,BVSP_test_DM$LSTM,BVSP_test_DM$actual,c=T)
test5<-DM.test(BVSP_test_DM$M.Carlo,BVSP_test_DM$MIDAS,BVSP_test_DM$actual,c=T)
test6<-DM.test(BVSP_test_DM$M.Carlo,BVSP_test_DM$SVR,BVSP_test_DM$actual,c=T)
test7<-DM.test(BVSP_test_DM$M.Carlo,BVSP_test_DM$LSTM,BVSP_test_DM$actual,c=T)
test8<-DM.test(BVSP_test_DM$MIDAS,BVSP_test_DM$SVR,BVSP_test_DM$actual,c=T)
test9<-DM.test(BVSP_test_DM$MIDAS,BVSP_test_DM$LSTM,BVSP_test_DM$actual,c=T)
test10<-DM.test(BVSP_test_DM$SVR,BVSP_test_DM$LSTM,BVSP_test_DM$actual,c=T)

col1<-cbind(0,test1$p.value,test2$p.value,test3$p.value,test4$p.value)
col2<-cbind(0,0,test5$p.value,test6$p.value,test7$p.value)
col3<-cbind(0,0,0,test8$p.value,test9$p.value)
col4<-cbind(0,0,0,0,test10$p.value)
col5<-cbind(0,0,0,0,0)

test_table<-rbind(col1,col2,col3,col4,col5)
test_table<-as.data.frame(test_table)
colnames(test_table)<-c('GARCH','Monte Carlo','GARCH-MIDAS','SVR','LSTM')

# write_xlsx(test_table,"E:\\Thesis Francisco Bettencourt\\Tese Nov\\4. Data\\R models\\Error By Country\\BVSP\\BVSP_DM_test.xlsx",col_names = T,format_headers = T,use_zip64 = FALSE)

