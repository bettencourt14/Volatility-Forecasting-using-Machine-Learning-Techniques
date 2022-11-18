
source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Preliminary analysis/HSI Preliminary Analysis.R")

#HSI Monte Carlo Simulation 

nsim=nrow(HSI2)+1
HSI1_daily_return<- HSI1_return_annualized/100/252
HSI1_daily_volatility<-HSI1_volatility_annualized/100/sqrt(252)
HSI1_T<- nrow(HSI2)+1
HSI1_T
HSI1_S0=as.numeric(HSI1[nrow(HSI1),2])
HSI1_S0

mc_function=function(nsim,N,daily_mean,daily_sd,S0)
{
  Z=rnorm(nsim,0,1)
  WT=sqrt(N)*Z
  ST=S0*exp((daily_mean-0.5*daily_sd^2)*N+daily_sd*WT)
  
  
  output_list=list(ST=ST)
  output_list
}

set.seed(994)

for (i in 1:1000)
{
  a <- mc_function(nsim =nsim,N=HSI1_T,daily_mean=HSI1_daily_return,
                   daily_sd = HSI1_daily_volatility, S0 = HSI1_S0)
  a <- as.data.frame(a)
  if(i==1)
  {
    HSI1_MC_results <- a 
  }
  else {
    HSI1_MC_results <- cbind(HSI1_MC_results,a)
  }
  
}


HSI1_MC_results[1,]<- HSI1_S0
HSI1_MC_results

for (i in 1:ncol(HSI1_MC_results)){
  colnames(HSI1_MC_results)[i] <- paste0('Sim ',i)
}

for(i in 1:ncol(HSI1_MC_results))
{
  b <- diff(log(HSI1_MC_results[,i]))
  b <- as.data.frame(b)
  if(i==1)
  {
    HSI1_MC_returns <- b
  }
  else {
    HSI1_MC_returns <- cbind(HSI1_MC_returns,b)
  }
  
}

for (i in 1:ncol(HSI1_MC_returns)){
  colnames(HSI1_MC_returns)[i] <- paste0('Sim ',i)
}


print(HSI1_MC_returns)
HSI1_MC_returns<- na.omit(HSI1_MC_returns)

HSI2_actual_prices<-c(HSI1_S0,HSI2$Close)
HSI2_actual_prices
HSI2_actual_returns<-diff(log(HSI2_actual_prices))
HSI2_actual_returns<- as.data.frame(HSI2_actual_returns)
dim(HSI2_actual_returns)

### Error for each day of the model by assuming daily price as correct one

HSI2_daily_error_aux<- HSI1_MC_returns
for (i in 1:ncol(HSI2_daily_error_aux)) {
  HSI2_daily_error_aux[,i]<-HSI2_actual_returns
}
HSI2_daily_error_aux<-as.data.frame(HSI2_daily_error_aux)

for (i in 1:ncol(HSI2_daily_error_aux)) {
  d<- abs(HSI1_MC_returns[,i]-HSI2_daily_error_aux[,i])
  d<- as.data.frame(d)
  
  if (i==1) {
    HSI_error<-d
    
  }
  else{
    HSI_error<-cbind(HSI_error,d)
  }
}

HSI_error    #use this for Error measurmments

for (i in 1:ncol(HSI_error)){
  colnames(HSI_error)[i] <- paste0('Sim ',i)
}

HSI_error<-as.data.frame(HSI_error)
HSI_error_means<-rowMeans(HSI_error)
HSI2_actual_returns
HSI2_print<-cbind(HSI2_actual_returns,HSI_error_means)
HSI2_print
hist(HSI2_actual_returns$HSI2_actual_returns,col='light grey',
     breaks=seq(from=-0.2, to=0.2,by=0.025),
     xlab= "Daily Returns",main="Actual Returns of Hang Seng for 21-02-2022 until 04-03-2022",
     las=1,ylim=c(0,8))
plot(HSI2_actual_returns$HSI2_actual_returns~HSI2$Date,type='l',col='darkblue',
     ylim=c(-0.15,0.15), 
     xlab="Simulated Days",ylab="Return",lwd=1, main='Hang Seng
     Actual vs. Monte Carlo Forecast')
lines(HSI_error_means~HSI2$Date,type='l',col='darkorange')
legend("bottomright",
       legend = c('Actual Returns','Forecasted Returns'),
       col = c('darkblue','darkorange'),
       pch = c(9,9))

##  Absolute difference between the HSI2_Actual_Returns and HSI_error_menans
for (i in 1:nrow(as.data.frame(HSI_error_means))) {

  e<- abs(HSI2_print[i,1]-HSI2_print[i,2])
  e<-as.data.frame(e)
  
  if (i==1) {
  HSI_abs_error<-e  
  }
  
  else{
    HSI_abs_error<- rbind(HSI_abs_error,e)
  }
}
e
HSI_abs_error

plot(HSI_abs_error$e~HSI2$Date,xlab='Days',ylab='Difference to actual', main='Hang Seng Monte Carlo Innefiency',
     type='l',lwd=2)

## Absolute squared error

for (i in 1:nrow(as.data.frame(HSI_error_means))) {
  
  g<- abs(HSI2_print[i,1]-HSI2_print[i,2])**2
  g<-as.data.frame(g)
  
  if (i==1) {
  HSI_mean_error<-g  
  }
  else{
  HSI_mean_error<-rbind(HSI_mean_error,g)
  }
}
g
HSI_mean_error
# Mean Errors
HSI_MC_absoulte_error<- sum(HSI_abs_error$e)/nrow(HSI_abs_error)
HSI_MC_squared_mean_error<- sum(HSI_mean_error$g)/nrow(HSI_mean_error)
HSI_MC_root_mean_error<-sqrt(HSI_MC_squared_mean_error)


