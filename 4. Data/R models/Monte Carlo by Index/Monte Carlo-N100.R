
source("E:/Thesis Francisco Bettencourt/Tese Nov/4. Data/R models/Preliminary analysis/N100 Preliminary Analysis.R")

#N100 Monte Carlo Simulation 

nsim=nrow(N1002)+1
N1001_daily_return<- N1001_return_annualized/100/252
N1001_daily_volatility<-N1001_volatility_annualized/100/sqrt(252)
N1001_T<- nrow(N1002)+1
N1001_T
N1001_S0=as.numeric(N1001[nrow(N1001),2])
N1001_S0

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
  a <- mc_function(nsim =nsim,N=N1001_T,daily_mean=N1001_daily_return,
                   daily_sd = N1001_daily_volatility, S0 = N1001_S0)
  a <- as.data.frame(a)
  if(i==1)
  {
    N1001_MC_results <- a 
  }
  else {
    N1001_MC_results <- cbind(N1001_MC_results,a)
  }
  
}


N1001_MC_results[1,]<- N1001_S0
N1001_MC_results

for (i in 1:ncol(N1001_MC_results)){
  colnames(N1001_MC_results)[i] <- paste0('Sim ',i)
}

for(i in 1:ncol(N1001_MC_results))
{
  b <- diff(log(N1001_MC_results[,i]))
  b <- as.data.frame(b)
  if(i==1)
  {
    N1001_MC_returns <- b
  }
  else {
    N1001_MC_returns <- cbind(N1001_MC_returns,b)
  }
  
}

for (i in 1:ncol(N1001_MC_returns)){
  colnames(N1001_MC_returns)[i] <- paste0('Sim ',i)
}


print(N1001_MC_returns)
N1001_MC_returns<- na.omit(N1001_MC_returns)

N1002_actual_prices<-c(N1001_S0,N1002$Close)
N1002_actual_prices
N1002_actual_returns<-diff(log(N1002_actual_prices))
N1002_actual_returns<- as.data.frame(N1002_actual_returns)
dim(N1002_actual_returns)

### Error for each day of the model by assuming daily price as correct one

N1002_daily_error_aux<- N1001_MC_returns
for (i in 1:ncol(N1002_daily_error_aux)) {
  N1002_daily_error_aux[,i]<-N1002_actual_returns
}
N1002_daily_error_aux<-as.data.frame(N1002_daily_error_aux)

for (i in 1:ncol(N1002_daily_error_aux)) {
  d<- abs(N1001_MC_returns[,i]-N1002_daily_error_aux[,i])
  d<- as.data.frame(d)
  
  if (i==1) {
    N100_error<-d
    
  }
  else{
    N100_error<-cbind(N100_error,d)
  }
}

N100_error    #use this for Error measurmments

for (i in 1:ncol(N100_error)){
  colnames(N100_error)[i] <- paste0('Sim ',i)
}

N100_error<-as.data.frame(N100_error)
N100_error_means<-rowMeans(N100_error)
N1002_actual_returns
N1002_print<-cbind(N1002_actual_returns,N100_error_means)
N1002_print
hist(N1002_actual_returns$N1002_actual_returns,col='light grey',
     breaks=seq(from=-0.2, to=0.2,by=0.025),
     xlab= "Daily Returns",main="Actual Returns of Euronext 100 for 21-02-2022 until 04-03-2022",
     las=1,ylim=c(0,8))
plot(N1002_actual_returns$N1002_actual_returns~N1002$Date,type='l',col='black',ylim=c(-0.15,0.15), 
     xlab="Simulated Days",ylab="Return",lwd=1, main='Euronext 100
     Actual vs. Average Simulated Returns')
lines(N100_error_means~N1002$Date,type='l',col='Orange')

##  Absolute difference between the N1002_Actual_Returns and N100_error_menans
for (i in 1:nrow(as.data.frame(N100_error_means))) {

  e<- abs(N1002_print[i,1]-N1002_print[i,2])
  e<-as.data.frame(e)
  
  if (i==1) {
  N100_abs_error<-e  
  }
  
  else{
    N100_abs_error<- rbind(N100_abs_error,e)
  }
}
e
N100_abs_error

plot(N100_abs_error$e~N1002$Date,xlab='Days',ylab='Difference to actual', main='Euronext 100 Monte Carlo Innefiency',
     type='l',lwd=2)

## Absolute squared error

for (i in 1:nrow(as.data.frame(N100_error_means))) {
  
  g<- abs(N1002_print[i,1]-N1002_print[i,2])**2
  g<-as.data.frame(g)
  
  if (i==1) {
  N100_mean_error<-g  
  }
  else{
  N100_mean_error<-rbind(N100_mean_error,g)
  }
}
g
N100_mean_error
# Mean Errors
N100_absoulte_error<- sum(N100_abs_error$e)/nrow(N100_abs_error)
N100_squared_mean_error<- sum(N100_mean_error$g)/nrow(N100_mean_error)
N100_root_mean_error<-sqrt(N100_squared_mean_error)


