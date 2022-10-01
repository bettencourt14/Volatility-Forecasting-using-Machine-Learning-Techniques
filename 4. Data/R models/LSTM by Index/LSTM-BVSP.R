library(dplyr)
library(tidyr)
library(keras)
library(tibble)
library(ggplot2)
library(httr) # downloading the xls(x) files
library(readxl) # reading xls(x) files

# Load the price data for the S&P 500
source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/High frequency preliminary analysis/BVSP-high.R")


# Set the training set and test set size
train_size <- nrow(BVSP1)-nrow(BVSP2)
test_size <- nrow(BVSP2)*2

# Set the amount of lags in months, the batch size and epochs
lag_n <- nrow(BVSP2)
batch_size <- 2
epochs <- 15

return<-diff(log(BVSP$Close))
return<-rbind(NA,as.data.frame(return))
return[is.na(return)]<-0
full_data<-cbind(as.data.frame(BVSP$Date)
                 ,as.data.frame(return$return))
head(full_data)
colnames(full_data)<- c('date','rt')

# Get a specified amount of rows and select columns
data <- full_data %>%
  slice((nrow(full_data) - (train_size + test_size) + 1):nrow(full_data)) %>%
  select(date,rt)

# Split into training and test sets ----
train_unscaled <- data %>%
  slice(1:train_size)

test_unscaled <- data %>%
  slice((train_size + 1):(train_size + test_size))

# Difference training and test set values
# TODO: percentages instead of plain differences
train_differenced <- train_unscaled %>% pull(rt)
test_differenced <- test_unscaled %>% pull(rt)

# Make lagged variables and put the values to matching positions
train_x <- t(sapply(1:(length(train_differenced) - lag_n),
                    function(x) train_differenced[x:(x + lag_n - 1)]))

train_y <- sapply((lag_n + 1):(length(train_differenced)),
                  function(x) train_differenced[x])

test_x <- t(sapply(1:(length(test_differenced) - lag_n),
                   function(x) test_differenced[x:(x + lag_n - 1)]))

test_y <- sapply((lag_n + 1):(length(test_differenced)),
                 function(x) test_differenced[x])

# Set the dimensions as required by TensorFlow
dim(train_x) <- c(nrow(train_x), ncol(train_x), 1)
dim(test_x) <- c(nrow(test_x), ncol(test_x), 1)

# Make the model ----
model <- keras_model_sequential()

model %>%
  layer_lstm(units = 4,
             input_shape = c(lag_n, 1)) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = "mse",
          optimizer = "adam",
          metrics = "mae")

# Print the loss and accuracy while training the model
history <- model %>% fit(x = train_x,
                         y = train_y,
                         batch_size = batch_size,
                         epochs = epochs,
                         verbose = 1)
plot(history,main='IBOVESPA LSTM Loss Function and MAE')

history$metrics

# Make predictions using both the training and test set and combine them ----

set.seed(999)
pred_train <- predict(model, train_x, batch_size = 1)
pred_test <- predict(model, test_x, batch_size = 1)

# Revert the differencing for the training and test sets
pred_train_undiff <- pred_train +
  train_unscaled %>%
  slice((lag_n ):(dim(train_unscaled)[1] - 1)) %>%
  pull(rt)

pred_test_undiff <- pred_test +
  test_unscaled %>%
  slice((lag_n ):(dim(test_unscaled)[1] - 1)) %>%
  pull(rt)

# Make a data frame containing the correctly scaled actuals and predictions
res <- tibble(dates = data$date,
              train = c(train_unscaled$rt, rep(NA, dim(test_unscaled)[1])),
              test = c(rep(NA, dim(train_unscaled)[1]), test_unscaled$rt),
              pred_train = c(rep(NA, lag_n),
                             pred_train_undiff,
                             rep(NA, dim(test_unscaled)[1])),
              pred_test = c(rep(NA, dim(train_unscaled)[1]),
                            rep(NA, lag_n),
                            pred_test_undiff))


# plot

plot(abs(res$train)~res$dates
     ,type='l',col='grey',xlab='Date',ylab='Volatility',
     main='IBOVESPA LSTM Training Capacity',lwd=1)
lines(abs(res$pred_train)~res$dates,type='l',col='orange',lwd=01)

# Error of Training set

BVSP_LSTM_abs_value<-res$train

for (i in nrow(BVSP2)+1:nrow(res)) {
  p<-abs(res[i,2]-res[i,4])
  p<-as.data.frame(p)
  
  if(i==nrow(BVSP2)+1){
    
    BVSP_LSTM_abs_value<-p
  }
  else{
    BVSP_LSTM_abs_value<-rbind(BVSP_LSTM_abs_value,p)
  }  
}
d<-length(which(is.na(BVSP_LSTM_abs_value)))
BVSP_LSTM_training_mean_abs_error<-sum(BVSP_LSTM_abs_value$train,na.rm=T)/(nrow(BVSP_LSTM_abs_value)-d)


BVSP_LSTM_squared_value<-BVSP_LSTM_abs_value

for (i in nrow(BVSP2)+1:nrow(res)) {
  q<-abs(res[i,2]-res[i,4])**2
  q<-as.data.frame(q)
  
  if(i==nrow(BVSP2)+1){
    
    BVSP_LSTM_squared_value<-q
  }
  else{
    BVSP_LSTM_squared_value<-rbind(BVSP_LSTM_squared_value,q)
  }  
}
e<-length(which(is.na(BVSP_LSTM_squared_value)))

BVSP_LSTM_training_mean_abs_error
BVSP_LSTM_training_squared_error<-sum(BVSP_LSTM_squared_value,na.rm=T)/
  (nrow(BVSP_LSTM_abs_value)-e)
BVSP_LSTM_training_root_error<-sqrt(BVSP_LSTM_training_squared_error)


# plot of Test
test_BVSP<-as.data.frame(res[length(which(is.na(res$pred_test))):nrow(res),5])
dim(test_BVSP)
head(test_BVSP,8)
test_BVSP<-as.data.frame(test_BVSP[2:nrow(test_BVSP),1])

plot(abs(BVSP2_returns$`diff(log(BVSP2$Close))`)~BVSP2$Date
     ,type='l',main=' IBOVESPA LSTM forecasting capaacity',
     xlab='Date',ylab='Absolute Return')
lines(abs(test_BVSP[,1])~BVSP2$Date,col='orange',type='l')

# test errors
  
for (i in 1:nrow(BVSP2)) {
  
  u<- abs(test_BVSP[i,1]-BVSP2_returns[i,1])
  u<-as.data.frame(u)
  
  if (i==1) {
    BVSP_LSTM_abs_error_value<- u
  }
  else{
    BVSP_LSTM_abs_error_value<-rbind(BVSP_LSTM_abs_error_value,u)
  }
}

BVSP_LSTM_abs_error_value

for (i in 1:nrow(BVSP2)) {
  
  w<- abs(test_BVSP[i,1]-BVSP2_returns[i,1])**2
  w<-as.data.frame(w)
  
  if (i==1) {
    BVSP_LSTM_squared_error_value<- w
  }
  else{
    BVSP_LSTM_squared_error_value<-rbind(BVSP_LSTM_squared_error_value,w)
  }
}
BVSP_LSTM_squared_error_value


BVSP_LSTM_abs_error<-sum(BVSP_LSTM_abs_error_value$u)/nrow(BVSP_LSTM_abs_error_value)
BVSP_LSTM_squared_error<- sum(BVSP_LSTM_squared_error_value$w)/nrow(BVSP_LSTM_squared_error_value)
BVSP_LSTM_root_error<-sqrt(BVSP_LSTM_squared_error)
