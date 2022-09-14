
source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Preliminary analysis/NSEI Preliminary Analysis.R")

#NSEI Monte Carlo Simulation 

nsim=nrow(NSEI2)+1
NSEI1_daily_return<- NSEI1_return_annualized/100/252
NSEI1_daily_volatility<-NSEI1_volatility_annualized/100/sqrt(252)
NSEI1_T<- nrow(NSEI2)+1
NSEI1_T
NSEI1_S0=as.numeric(NSEI1[nrow(NSEI1),2])
NSEI1_S0

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
  a <- mc_function(nsim =nsim,N=NSEI1_T,daily_mean=NSEI1_daily_return,
                   daily_sd = NSEI1_daily_volatility, S0 = NSEI1_S0)
  a <- as.data.frame(a)
  if(i==1)
  {
    NSEI1_MC_results <- a 
  }
  else {
    NSEI1_MC_results <- cbind(NSEI1_MC_results,a)
  }
  
}


NSEI1_MC_results[1,]<- NSEI1_S0
NSEI1_MC_results

for (i in 1:ncol(NSEI1_MC_results)){
  colnames(NSEI1_MC_results)[i] <- paste0('Sim ',i)
}

for(i in 1:ncol(NSEI1_MC_results))
{
  b <- diff(log(NSEI1_MC_results[,i]))
  b <- as.data.frame(b)
  if(i==1)
  {
    NSEI1_MC_returns <- b
  }
  else {
    NSEI1_MC_returns <- cbind(NSEI1_MC_returns,b)
  }
  
}

for (i in 1:ncol(NSEI1_MC_returns)){
  colnames(NSEI1_MC_returns)[i] <- paste0('Sim ',i)
}


print(NSEI1_MC_returns)
NSEI1_MC_returns<- na.omit(NSEI1_MC_returns)

NSEI2_actual_prices<-c(NSEI1_S0,NSEI2$Close)
NSEI2_actual_prices
NSEI2_actual_returns<-diff(log(NSEI2_actual_prices))
NSEI2_actual_returns<- as.data.frame(NSEI2_actual_returns)
dim(NSEI2_actual_returns)

### Error for each day of the model by assuming daily price as correct one

NSEI2_daily_error_aux<- NSEI1_MC_returns
for (i in 1:ncol(NSEI2_daily_error_aux)) {
  NSEI2_daily_error_aux[,i]<-NSEI2_actual_returns
}
NSEI2_daily_error_aux<-as.data.frame(NSEI2_daily_error_aux)

for (i in 1:ncol(NSEI2_daily_error_aux)) {
  d<- abs(NSEI1_MC_returns[,i]-NSEI2_daily_error_aux[,i])
  d<- as.data.frame(d)
  
  if (i==1) {
    NSEI_error<-d
    
  }
  else{
    NSEI_error<-cbind(NSEI_error,d)
  }
}

NSEI_error    #use this for Error measurmments

for (i in 1:ncol(NSEI_error)){
  colnames(NSEI_error)[i] <- paste0('Sim ',i)
}

NSEI_error<-as.data.frame(NSEI_error)
NSEI_error_means<-rowMeans(NSEI_error)
NSEI2_actual_returns
NSEI2_print<-cbind(NSEI2_actual_returns,NSEI_error_means)
NSEI2_print
hist(NSEI2_actual_returns$NSEI2_actual_returns,col='light grey',
     breaks=seq(from=-0.2, to=0.2,by=0.02),
     xlab= "Daily Returns",main="Actual Returns of NIFTY 50 for 21-02-2022 until 04-04-2022",
     las=1,ylim=c(0,8))
lines(density(NSEI2_actual_returns$NSEI2_actual_returns),type='l',col='red')
plot(NSEI2_actual_returns$NSEI2_actual_returns,type='l',col='black',ylim=c(-0.15,0.15), 
     xlab="Simulated Days",ylab="Closing Price",lwd=1, main='NIFTY 50
     Actual vs. Average Simulated Returns')
lines(NSEI_error_means,type='p',col='Dark blue')

##  Absolute difference between the NSEI2_Actual_Returns and NSEI_error_menans

for (i in 1:nrow(as.data.frame(NSEI_error_means))) {

  e<- abs(NSEI2_print[i,1]-NSEI2_print[i,2])
  e<-as.data.frame(e)
  
  if (i==1) {
  NSEI_abs_error<-e  
  }
  
  else{
    NSEI_abs_error<- rbind(NSEI_abs_error,e)
  }
}
e
NSEI_abs_error

plot(NSEI_abs_error$e~NSEI2$Date,xlab='Days',ylab='Difference to actual', main='NIFTY 50 Monte Carlo Innefiency',
     type='l',lwd=2)

## Absolute squared error

for (i in 1:nrow(as.data.frame(NSEI_error_means))) {
  
  g<- abs(NSEI2_print[i,1]-NSEI2_print[i,2])**2
  g<-as.data.frame(g)
  
  if (i==1) {
  NSEI_mean_error<-g  
  }
  else{
  NSEI_mean_error<-rbind(NSEI_mean_error,g)
  }
}
g
NSEI_mean_error
# Mean Errors
NSEI_absoulte_error<- sum(NSEI_abs_error$e)/nrow(NSEI_abs_error)
NSEI_squared_mean_error<- sum(NSEI_mean_error$g)/nrow(NSEI_mean_error)
NSEI_root_mean_error<-sqrt(NSEI_squared_mean_error)


